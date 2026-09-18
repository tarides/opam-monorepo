(******************************************************************************)
(*                                                                            *)
(*                                  Bitsets                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2025--2025 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

module[@inline] Make (Set : sig
  type t
  val is_empty : t -> bool
  val compare : t -> t -> int
  val compare_minimum : t -> t -> int
  val big_union : t list -> t
  val extract_unique_prefix : t -> t -> t * t
  val extract_shared_prefix : t -> t -> t * (t * t)
end) = struct

type set =
  Set.t

type sets =
  set list

let[@inline] nonempty (s : set) =
  not (Set.is_empty s)

module H =
  LeftistHeap.Make(struct
    type t = Set.t
    let compare = Set.compare_minimum
  end)
open H

let[@inline] insert_if_nonempty s k heap : 'v heap =
  if nonempty s then H.insert s k heap else heap

(* A "key" is a set of small integers in the range [0, n) where [n] is (at
   most) the length of the list [ss]. We choose an implementation of [Key]
   based on the value of [n]. If [n] is small enough, [Key] is [WordMini],
   which is very efficient (a set is just an integer value). Otherwise,
   [Key] is [SparseMini]. *)

(* We cannot use any of the modules [...BitSet] because that would introduce a
   cyclic dependency; these modules use [Partition.Make]. *)

module Run (N : sig val n : int end) () = struct

  module Key =
    BoundedMini.Make(N)()

  type key =
    Key.t

  type part =
    set * key

  type parts =
    part list

  let[@inline] cons_if_nonempty s k parts : parts =
    if nonempty s then (s, k) :: parts else parts

  (* [loop] grows the list [parts] by extracting pairs [(s, k)] out of
     the priority queue [heap]. A better comment would be desirable! *)

  let rec loop (parts : parts) (heap : key heap) : parts =
    match pop2 heap with
    | Head (s1, k1, s2, k2, heap) ->
        let sp, s1 = Set.extract_unique_prefix s1 s2 in
        let sc, (s1, s2) = Set.extract_shared_prefix s1 s2 in
        let parts = cons_if_nonempty sp k1 parts in
        let heap =
          (* [insert_if_nonempty sc (Key.union k1 k2)] *)
          if nonempty sc then insert sc (Key.union k1 k2) heap
          else heap
        in
        let heap = insert_if_nonempty s1 k1 heap in
        let heap = insert_if_nonempty s2 k2 heap in
        loop parts heap
    | Tail (k, v) ->
        (k, v) :: parts
    | Done ->
        parts

  (* [prepare ss] builds a heap of pairs [(s, k)] where [s] ranges
     over the nonempty sets in the list [ss] and where the key [k]
     is a singleton set that contains a unique integer index. *)

  let prepare (ss : sets) : key heap =
    let c = ref 0 in
    List.fold_left (fun h (s : set) ->
      (* Any empty sets in the list [ss] are ignored on the fly. *)
      if nonempty s then
        let i = !c in
        c := i + 1;
        insert s (Key.singleton i) h
      else
        h
    ) empty ss

  (* [union_parts] expects a list of pairs [(s, k)] where [s] is a set and [k]
     is a key. It finds all sets that have a common key and fuses them. The
     result is a list of sets. *)

  let by_key (_, k1) (_, k2) =
    Key.compare k1 k2

  let rec merge accu ss k parts : sets =
    match parts with
    | [] ->
        (* [merge] intentionally uses an accumulator [ss] of type [set list],
           as opposed to just [set]. Indeed, it can be more efficient to use
           [Set.big_union] just once at the end, as opposed to repeatedly
           using [Set.union] along the way. *)
        Set.big_union ss :: accu
    | (s, k') :: parts ->
        if Key.equal k k' then
          merge accu (s :: ss) k parts
        else
          merge (Set.big_union ss :: accu) [s] k' parts

  let union_parts (parts : parts) : sets =
    (* Sort the pairs [(s, k)] by key. *)
    match List.sort by_key parts with
    | [] ->
        []
    | (s1, k1) :: parts ->
        (* Merge the sets that have the same key. *)
        merge [] [s1] k1 parts

  let partition ss =
    ss
    |> prepare
    |> loop []
    |> union_parts

end (* Run *)

(* [count_nonempty ss] counts the nonempty sets in the list [ss]. *)

let rec count_nonempty accu (ss : sets) : int =
  match ss with
  | [] ->
      accu
  | s :: ss ->
      let accu = if nonempty s then accu + 1 else accu in
      count_nonempty accu ss

let[@inline] count_nonempty (ss : sets) : int =
  count_nonempty 0 ss

let partition (ss : sets) : sets =
  (* Remove duplicate elements in [ss].
     This can speed up the partitioning process significantly.
     Furthermore, it can decrease the value of [n],
     increasing our chances of using [WordMini]. *)
  let ss = List.sort_uniq Set.compare ss in
  (* If the empty set was the least element in the ordering [Set.compare] then
     we could filter it out just by testing whether the first element of the
     list [ss] is the empty set. This is not the case, though, so we
     explicitly filter out the empty set in [count_nonempty] and [prepare]. *)
  let module R = Run(struct let n = count_nonempty ss end)() in
  R.partition ss

end (* Make *)
