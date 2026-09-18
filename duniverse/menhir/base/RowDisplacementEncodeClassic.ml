(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module compresses a two-dimensional table, where some values
   are considered insignificant, via row displacement. *)

type displacement =
  int

type 'a table =
  displacement array * 'a array

(* In a natural version of this algorithm, displacements would be greater
   than (or equal to) [-n]. However, in the particular setting of Menhir,
   both arrays are intended to be compressed with [PackedIntArray], which
   does not efficiently support negative numbers. For this reason, we are
   careful not to produce negative displacements. *)

(* In order to avoid producing negative displacements, we encode displacements
   by moving to the sign bit to the least significant position. This encoding
   function does this. *)

let[@inline] encode (displacement : int) : displacement =
  if displacement >= 0 then
    displacement lsl 1
  else
    (-displacement) lsl 1 + 1

(* It is reasonable to assume that, as matrices grow large, their
   density becomes low, i.e., they have many insignificant entries.
   As a result, it is important to work with a sparse data structure
   for rows. We internally represent a row as a list of its
   significant entries, where each entry is a pair of a [j] index and
   an element. *)

type 'a row =
    (int * 'a) list

(* [compress insignificant dummy m n t] turns the two-dimensional
   table [t] into a compressed table. The parameter [insignificant]
   determines which data values are insignificant, and can thus be
   overwritten with other values. The parameter [dummy] is used to
   fill holes in the data array. [m] and [n] are the integer
   dimensions of the table [t]. The type ['a] must support OCaml's
   polymorphic equality test. *)

let compress
    (insignificant : 'a -> bool)
    (dummy : 'a)
    (m : int) (n : int)
    (t : 'a array array)
    : 'a table =

  let equal : 'a -> 'a -> bool = (=) in

  (* Be defensive. *)

  assert (Array.length t = m);
  assert begin
    for i = 0 to m - 1 do
      assert (Array.length t.(i) = n)
    done;
    true
  end;

  (* This turns a row-as-array into a row-as-sparse-list. The row is
     accompanied by its index [i] and by its rank (the number of its
     significant entries, that is, the length of the row-as-a-list. *)

  let sparse (i : int) (line : 'a array) : int * int * 'a row (* index, rank, row *) =

    let rec loop (j : int) (rank : int) (row : 'a row) =
      if j < 0 then
        i, rank, row
      else
        let x = line.(j) in
        if insignificant x then
          loop (j - 1) rank row
        else
          loop (j - 1) (1 + rank) ((j, x) :: row)
    in

    loop (n - 1) 0 []

  in

  (* Construct an array of all rows, together with their index and rank. *)

  let rows : (int * int * 'a row) array = (* index, rank, row *)
    Array.mapi sparse t
  in

  (* Sort this array by decreasing rank. This does not have any impact
     on correctness, but reportedly improves compression. The
     intuitive idea is that rows with few significant elements are
     easy to fit, so they should be inserted last, after the problem
     has become quite constrained by fitting the heavier rows. This
     heuristic is attributed to Ziegler. *)

  Array.fast_sort (fun (_, rank1, _) (_, rank2, _) ->
    compare rank2 rank1
  ) rows;

  (* Allocate a one-dimensional array of displacements. *)

  let displacement : int array =
    Array.make m 0
  in

  (* Allocate a one-dimensional, infinite array of values. Indices
     into this array are written [k]. *)

  let data : 'a InfiniteArray.t =
    InfiniteArray.make dummy
  in

  (* Determine whether [row] fits at offset [k] within the current [data]
     array, up to extension of this array. *)

  (* Note that this check always succeeds when [k] equals the length of
     the [data] array. Indeed, the loop is then skipped. This property
     guarantees the termination of the recursive function [fit] below. *)

  let fits k (row : 'a row) : bool =

    let d = InfiniteArray.extent data in

    let rec loop = function
      | [] ->
          true
      | (j, x) :: row ->

          (* [x] is a significant element. *)

          (* By hypothesis, [k + j] is nonnegative. If it is greater than or
             equal to the current length of the data array, stop -- the row
             fits. *)

          assert (k + j >= 0);

          if k + j >= d then
            true

          (* We now know that [k + j] is within bounds of the data
             array. Check whether it is compatible with the element [y] found
             there. If it is, continue. If it isn't, stop -- the row does not
             fit. *)

          else
            let y = InfiniteArray.get data (k + j) in
            if insignificant y || equal x y then
              loop row
            else
              false

    in
    loop row

  in

  (* Find the leftmost position where a row fits. *)

  (* If the leftmost significant element in this row is at offset [j],
     then we can hope to fit as far left as [-j] -- so this element
     lands at offset [0] in the data array. *)

  (* Note that displacements may be negative. This means that, for
     insignificant elements, accesses to the data array could fail: they could
     be out of bounds, either towards the left or towards the right. This is
     not a problem, as long as [get] is invoked only at significant
     elements. *)

  let rec fit k row : int =
    if fits k row then
      k
    else
      fit (k + 1) row
  in

  let fit row =
    match row with
    | [] ->
        0 (* irrelevant *)
    | (j, _) :: _ ->
        fit (-j) row
  in

  (* Write [row] at (compatible) offset [k]. *)

  let rec write k = function
    | [] ->
        ()
    | (j, x) :: row ->
        InfiniteArray.set data (k + j) x;
        write k row
  in

  (* Iterate over the sorted array of rows. Fit and write each row at
     the leftmost compatible offset. Update the displacement table. *)

  Array.iter (fun (i, _, row) ->
    let k = fit row in (* if [row] has leading insignificant elements, then [k] can be negative *)
    write k row;
    displacement.(i) <- encode k
  ) rows;

  (* Return the compressed tables. *)

  displacement, InfiniteArray.domain data

(* A simpler version of [compress], which does not require [m] and [n]
   as arguments. *)

let compress
  (insignificant : 'a -> bool)
  (dummy : 'a)
  (t : 'a array array)
  : 'a table =
  let m = Array.length t in
  if m = 0 then
    [||], [||]
  else
    let n = Array.length t.(0) in
    compress insignificant dummy m n t
