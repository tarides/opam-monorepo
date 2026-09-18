(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* -------------------------------------------------------------------------- *)

(**A displacement is a nonnegative integer, which, once decoded in a certain
   way, represents a possibly negative offset into a data array. *)
type displacement =
  int

(**A compressed table is represented as a pair of a displacement array and a
   data array. The displacement array is an array of (encoded) offsets into
   the data array. *)
type 'a table =
  displacement array * 'a array

(* -------------------------------------------------------------------------- *)

(* In order to avoid producing negative displacements, we encode displacements
   by moving to the sign bit to the least significant position. This encoding
   function undoes this. *)

(* One could also think, say, of adding [n] to every displacement, so as to
   ensure that all displacements are nonnegative. This would work, but would
   require [n] to be published, for use by the decoder. *)

let[@inline] encode (displacement : int) : displacement =
  if displacement >= 0 then
    displacement lsl 1
  else
    (-displacement) lsl 1 + 1

(* -------------------------------------------------------------------------- *)

module A = Bitsets.WordBitSet
module S = Bitsets.SparseBitSet
module V = Bitsets.DenseBitVector

(* A cell is a pair of an index and a piece of data. *)
type 'a cell =
  int * 'a

(* A sparse row is a list of cells, whose indices must be pairwise distinct.
   A sparse row may have holes: that is, it may be that, for some index [i],
   there is no cell of the form [(i, v)]. *)
type 'a sparse_row =
  'a cell list

(* This fast packing algorithm proceeds in two steps:

   1. Determine which rows can be merged;
   2. Pack the merged rows into a one-dimensional array,
      without allowing any new merges.

   Two sparse rows are compatible (mergeable) if the values of the cells on
   which they overlap are equal. That is, if one row has a cell [(i, v1)]
   and the other row has a cell [(i, v2)] then the values [v1] and [v2] must
   be equal.

   The classic implementation of compression in Menhir performs both steps in
   a single pass. This implies checking for compatibility while fitting a row
   in the sparse table; this forbids the use of vectorised operations (i.e.,
   operations on bit sets) to speed things up.

   By separating the two steps, this algorithm enables the use of bit sets. *)

(* -------------------------------------------------------------------------- *)

(* The first step is to compute a merge plan, that is, a partition of the
   rows, grouped by compatibility. This is done in two passes:

   1. For each column, a set of rows defined in this column.
      For each cell, a set of "compatible" rows having this cell.
      (Rows and columns are represented by their index, a cell is represented as
      a pair of a column index and a value.)

   2. For each row, merging the next non-merged row that is compatible with this
      row (the step 1. allows compatibility to be quickly determined). *)

(**A row is represented by its index in the array of rows that is passed
   to [merge_rows]. *)
type index =
  int

(**A group is a list of row indices. *)
 type group =
   index list

(**[merge_rows rows cols] determines which rows in the array [rows] are
   compatible and therefore can be merged. [cols] is the common logical
   width of all rows. *)

let merge_rows (type a) (rows : a sparse_row array) (cols : int) : group list =
  let module M = struct

  (* This function is quite long, so, for syntactic comfort,
     we write its body as a local module. *)

  (* A rich row, or ranked row, is a row that has been decorated with:

     - the index of this row in the original array of rows;
     - its rank, that is, the number of its nonempty cells.

     We sort the array [rows] in place, which is why a row must be
     explicitly decorated with its original index. Outside of this
     specific field, when we use row indices, these are indices
     into the *sorted* array [rows]. *)
  type 'a rrow = {
    index: int;
    rank: int;
    cells: 'a sparse_row;
  }

  (* Enrich each row with its index and rank. *)
  let enrich index cells =
    let rank = List.length cells in
    { index; rank; cells }
  let rows : a rrow array = Array.mapi enrich rows

  (* Sort the rows by decreasing rank. *)
  let decreasing_rank r1 r2 = Int.compare r2.rank r1.rank
  let () = Array.sort decreasing_rank rows

  (* Compute two mappings, [compatible] and [defined].

     The function [compatible] maps a cell to a set of all rows that
     have this cell. (This cell must exist in some row.)

     The array [defined] maps a column to the set of all rows that
     have a cell in this column. *)

  let
    (compatible : a cell -> S.t),
    (defined : S.t array)
  =

    (* We use a hash table whose keys are cells. Therefore the type [a]
       must support OCaml's polymorphic equality and hash functions. *)
    let compatibility = Hashtbl.create 7 in
    let defined = Array.make cols S.empty in

    (* [register i cell] registers the existence of cell [cell] in row [i]. *)
    let register i cell =
      (* Extract the column index [j]. *)
      let j, _ = cell in
      (* Record that this row is defined on column [j]. *)
      defined.(j) <- S.add i defined.(j);
      (* Record that the cell [cell] appears in row [i]. *)
      match Hashtbl.find compatibility cell with
      | rows ->
          rows := S.add i !rows
      | exception Not_found ->
          let rows = ref (S.singleton i) in
          Hashtbl.add compatibility cell rows
    in

    (* Treat each row in turn. [i] is the row's index. Within each row, treat
       each cell in turn. *)
    for i = Array.length rows - 1 downto 0 do
      List.iter (register i) rows.(i).cells
    done;

    let compatible cell = !(Hashtbl.find compatibility cell) in
    compatible, defined

  (* During the construction of a merge plan, a row is available until it
     becomes part of a group; then, it is no longer available. The mutable
     array [available] keeps track of which rows are still available. *)

  let available =
    Array.map (fun _ -> true) rows

  (* [build_group i] builds a new group, which contains at least the row [i].
     The array [available] is updated: the rows in the new group are marked as
     no longer available. *)

  let build_group (i : index) : group =

    (* Accumulate the rows that are candidates for inclusion in the current
       group. This set grows; but, because we are interested only in rows
       above the current row [i], we remove rows whose index is less than [i];
       and, because an incompatible row is not a suitable candidate, we also
       (lazily) remove incompatible rows. *)
    let candidates = ref S.empty in
    (* Accumulate the rows that are incompatible with the current group.
       This set grows; but, because we are interested only in rows above
       the current row [i], we remove rows whose index is less than [i]. *)
    let incompatible = ref S.empty in

    (* [update i cell] updates the sets [candidates] and [incompatible]
       so as to reflect the existence in row [i] of the cell [cell].
       This row has been included in the current group already. *)
    let update i cell =
      let (j, _) = cell in
      (* Restricting our attention to rows whose index is above [i], compute
         which rows are compatible / incompatible with this cell. *)
      let compatible_at_j   = S.above i (compatible cell)
      and defined_at_j      = S.above i defined.(j) in
      let incompatible_at_j = S.diff defined_at_j compatible_at_j in
      (* The compatible cells become new candidates for inclusion in this
         group. The incompatible cells are recorded. *)
      candidates := S.union compatible_at_j !candidates;
      incompatible := S.union incompatible_at_j !incompatible
    in

    (* [add_row group i] adds the row [i] to the group [group], then
       continues extending this group with rows whose indices lie above [i]. *)
    let rec add_row group i : group =
      (* Mark the row [i] as unavailable. *)
      assert available.(i);
      available.(i) <- false;
      (* Fetch the row whose index is [i]. Extend the group with its index.
         (A group is a list of *original* indices.) *)
      let row = rows.(i) in
      let group = row.index :: group in
      (* Update the sets [incompatible] and [candidates]. *)
      List.iter (update i) row.cells;
      candidates := S.diff !candidates !incompatible;
      (* Find the first (available) candidate, if there are any. *)
      match S.find_first_opt (Array.get available) !candidates with
      | None ->
          (* Done. *)
          group
      | Some i ->
          (* The next candidate is row [i]. Prune the sets [incompatible] and
             [candidate] by removing now-irrelevant rows. Then, add row [i] to
             the group and continue. *)
          incompatible := S.above i !incompatible;
          candidates := S.above i !candidates;
          add_row group i
    in

    (* Start with the row [i] and build a group for it. *)
    add_row [] i

  (* A merge plan is a partition of the rows; it is represented as a list of
     groups. The rows within a group are compatible with each other. We are
     now ready to build a merge plan.  *)

  let merge_plan : group list =

    (* Process each row in turn. For each available row, greedily build a new
       group, which contains at least this row. Because the arrays [rows] has
       been sorted by decreasing rank, we begin with the rows that have higher
       ranks. *)
    let merge_plan = ref [] in
    for i = 0 to Array.length rows - 1 do
      if available.(i) then
        let group = build_group i in
        merge_plan := group :: !merge_plan
    done;
    !merge_plan

  end (* M *)
  (* Return the merge plan. *)
  in M.merge_plan

(* -------------------------------------------------------------------------- *)

(* The second step is specialized to work on bit sets:

   - a row is represented as a sparse bit set;
   - the table is represented as a dense bit vector.

   No merging is attempted; only packing (fitting all rows in a single table)
   is attempted. For this reason, in a row of type [a sparse_row], the data of
   type [a] is irrelevant. This data is ignored; only occupancy matters. This
   explains why a row can be represented as a bit set.

   The function [add] allows adding rows one by one to the table, in a greedy
   manner. As each row is added, its final offset within the table is computed.

   In the worst case, the cost of adding one row is quadratic in the number
   of non-empty cells in this row. Therefore, in the worst case, the cost of
   adding all rows is cubic. However, according to Yao and Tarjan, under a
   certain harmonic decay assumption, the worst-case is only quadratic.

   Anyway, the use of bit vectors allows this code to be significantly faster
   than the classic algorithm. *)

module Pack : sig

  (**A table is a bit set where a number of rows have been packed. *)
  type table

  (**[create()] creates an empty table. *)
  val create : unit -> table

  (**[add table row] adds the row [row] to the table [table]. This row is
     moved towards by a certain offset so that it fits in a position where it
     does not collide with previously added rows. This offset is returned.
     This offset can be negative. *)
  val add : table -> 'a sparse_row -> int

  (**[width table] returns the current width of the table [table], expressed
     in bits. *)
  val width : table -> int

end = struct

  type table = V.vector
  let create = V.create
  let width = V.width

  (* [valid v a] determines whether the address [a] is valid with respect
     to the vector [v], that is, less than the length of this vector. *)

  let[@inline] valid v a =
    assert (0 <= a);
    a < V.length v

  (* [get' v a] reads the vector [v] at address [a]. If this address is
     out of bounds then an empty word is returned. *)

  let[@inline] get' v a =
    assert (0 <= a);
    if valid v a then V.get v a else A.empty

  (* [set_mask v a mask] updates the word at address [a] in the vector [v]
     by setting the bits in [mask]. These bits must not be set already. *)

  let[@inline] set_mask v a mask =
    let word = V.get v a in
    assert (A.disjoint word mask);
    V.set v a (A.union word mask)

  (* A row is represented internally as a pair of
     - an offset [delta] that must be added to all elements, and
     - a sparse bit set of elements, [cells].
     If [cells] is empty then the value of [delta] is irrelevant. *)

  type row =
    int * S.t

  (* [import] converts a sparse row to the above representation. *)

  let import (type a) (row : a sparse_row) : row =
    match row with
    | [] ->
        (0, S.empty)
    | (delta, _) :: row ->
        let add cells (i, _) = S.add (i - delta) cells in
        (* We iterate on the list [row] from back to front, so each
           invocation of [S.add] inserts at or near the front of the
           sparse bit set. This avoids a quadratic blowup. *)
        let cells = List.fold_left add S.empty (List.rev row) in
        (delta, S.add 0 cells)

  (* [compatible cells0 cells1 mask shift] determines whether the two
     words [cells0] and [cells1] are compatible with the mask [mask],
     shifted up by the offset [shift]. This offset must be comprised
     between 0 included and [A.bound] excluded. *)

  let[@inline] compatible cells0 cells1 mask shift : bool =
    let mask0, mask1 = A.shift mask shift in
    A.disjoint cells0 mask0 &&
    A.disjoint cells1 mask1

  (* [fit table oaddr shift cells] determines whether the cells [cells]
     fit in the table [table] at address [oaddr] and offset [shift].

     Three outcomes are possible:
     - If [shift] itself is returned, fitting was successful.
     - If a value comprised between [shift] and [A.bound] (both excluded)
       is returned, then fitting should be retried with this new offset.
     - If [A.bound] is returned then fitting should be retried with
       an incremented address.

     The reason why [fit] can return "please retry", as opposed to
     retrying by itself, is that retrying requires starting over
     with a full row, of which [cells] is just a subrow.  *)

  let rec fit table oaddr shift cells : int =
    match S.view cells with
    | S.N ->
        shift (* success *)
    | S.C (base, mask, cells) ->
        let addr = oaddr + base / A.bound in
        (* Shortcut: if [addr] is out of bounds then the table is logically
           empty at [addr] and beyond. Therefore fitting this entire row
           must succeed. *)
        if not (valid table addr) then shift (* success *) else
        (* Read two words in the table and test whether they are compatible
           with the bit set [mask], shifted up by [shift]. *)
        let cells0 = V.get table addr in
        let cells1 = get' table (addr + 1) in
        if compatible cells0 cells1 mask shift then
          (* This cell fits: continue with the rest. *)
          fit table oaddr shift cells
        else
          (* This cell does not fit: increment [shift] until either this
             cell fits or [shift] reaches [A.bound]. In either case, return
             a request for a retry. *)
          let shift = ref (shift + 1) in
          while !shift < A.bound &&
                not (compatible cells0 cells1 mask !shift)
          do incr shift; done;
          !shift

  (* [fit table oaddr shift cells] writes the cells [cells] in the table
     [table] at address [oaddr] and offset [shift]. These cells must fit. *)

  let rec write table oaddr shift cells =
    match S.view cells with
    | S.N ->
        ()
    | S.C (base, mask, cells) ->
        let addr = oaddr + base / A.bound in
        V.grow table (addr + 1);
        let mask0, mask1 = A.shift mask shift in
        set_mask table (addr + 0) mask0;
        set_mask table (addr + 1) mask1;
        write table oaddr shift cells

  (* [add table oaddr shift cells] fits the row [cells] in the table [table]
     at address [oaddr] and offset [shift] or above. The position at which
     this row was inserted is returned. *)

  let rec add table oaddr shift cells =
    (* Determine whether this row fits at this address and offset. *)
    let shift' = fit table oaddr shift cells in
    if shift' = shift then
      (* It does fit. Commit and return this address and offset,
         converted to an offset expressed in bits. *)
      let () = write table oaddr shift cells in
      oaddr * A.bound + shift
    else if shift' < A.bound then
      (* It does not fit. Retry. *)
      add table oaddr shift' cells
    else
      (* It does not fit. Retry. *)
      add table (oaddr + 1) 0 cells

  (* [add table cells] fits the row [cells] in the table [table].
     The position at which this row was inserted is returned. *)

  let add table row =
    let index, cells = import row in
    let offset = add table 0 0 cells in
    offset - index

end (* Pack *)

(* -------------------------------------------------------------------------- *)

(* The main function is organized as follows:
   - Determine which rows can be merged;
   - Determine how to pack the (merged) rows,
     now assumed to be pairwise incompatible,
     can be packed within a one-dimensional table;
   - Based on this information,
     construct a displacement table and a data table. *)

let compress_nonempty (type a)
  (insignificant : a -> bool) (dummy : a)
  (t : a array array)
: a table =
  let module C = struct

  (* [sparsify] turns a dense row, represented as an array, into a sparse row. *)

  let rec sparsify (line : a array) j (row : a sparse_row) =
    if j < 0 then
      row
    else
      let x = line.(j) in
      if insignificant x then
        sparsify line (j - 1) row
      else
        sparsify line (j - 1) ((j, x) :: row)

  let[@inline] sparsify (line : a array) : a sparse_row =
    sparsify line (Array.length line - 1) []

  (* Construct an array of sparse rows. *)

  let rows : a sparse_row array =
    Array.map sparsify t

  (* Construct a merge plan; that is, gather compatible rows in groups. *)

  let merge_plan : group list =
    let cols = Array.length t.(0) in (* this requires [t] to be nonempty *)
    merge_rows rows cols

  (* [compare_cells] compares two cells that originate in two rows that we
     wish to merge. Because we assume that the two rows are compatible, if
     the column indices [j1] and [j2] are equal then the pieces of data [a1]
     and [a2] must be equal as well. Thus, two cells can be compared based
     on [j1] and [j2] alone. *)

  let compare_cells (j1, a1) (j2, a2) =
    let c = Int.compare j1 j2 in
    (* Sanity check: only two equal values can be merged. *)
    assert (c <> 0 || a1 = a2);
    c

  (* The type ['a rgroup] is analogous to the type ['a rrow] that was used
     earlier, except it represents a group of merged rows, as opposed to a
     single row. Therefore, instead of an [index] field, it carries a [group]
     field, a list of indices. *)

  type 'a rgroup = {
    group: group;
    rank: int;
    cells: 'a sparse_row;
  }

  (* [enrich] converts a group (a list of row indices) to a rich group,
     as described above. *)

  let enrich (group : group) : a rgroup =
    (* Gather every row in this group. *)
    let rows : a sparse_row list = List.map (Array.get rows) group in
    (* Merge these rows into a single row using via merge sort. *)
    let cells : a sparse_row =
      MList.reduce [] (MList.merge_uniq compare_cells) rows in
    (* Done. *)
    let rank = List.length cells in
    { group; rank; cells }

  (* Construct a list of rich groups. Sort it by decreasing rank. *)

  let decreasing_rank rgroup1 rgroup2 =
    Int.compare rgroup2.rank rgroup1.rank

  let rgroups : a rgroup list =
    List.map enrich merge_plan
    |> List.sort decreasing_rank

  (* Pack each rich group in turn into a one-dimensional table.
     This yields a list [offsets], which maps each rich group
     to an offset, and an integer [width], which is the length
     of the necessary table. *)

  let v = Pack.create ()
  let add rgroup = Pack.add v rgroup.cells
  let offsets : int list = List.map add rgroups
  let width : int = Pack.width v

  (* Allocate and populate the displacement and data arrays. *)

  let displacement : int array = Array.make (Array.length t) 0
  let data : a array = Array.make width dummy

  let store offset rgroup =
    (* Store an (encoded) offset in the displacement table. *)
    let code = encode offset in
    List.iter (fun row -> displacement.(row) <- code) rgroup.group;
    (* Store the content of the data cells in the data table. *)
    List.iter (fun (j, a) -> data.(offset + j) <- a) rgroup.cells

  let () =
    List.iter2 store offsets rgroups

  end (* C *)
  (* Return the compressed tables. *)
  in C.(displacement, data)

(* The case where [t] has length zero must be dealt with separately. *)

let compress insignificant dummy t =
  if Array.length t = 0 then
    ([||], [||])
  else
    compress_nonempty insignificant dummy t
