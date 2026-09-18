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

module W =
  WordBitSet

let bound =
  2 * W.bound

type elt =
  int

(* A bit set is represented as a pair of words. *)

type t =
  | D of W.t * W.t

(* -------------------------------------------------------------------------- *)

(* Construction. *)

let empty =
  D (W.empty, W.empty)

(* The smart constructor [construct hi lo] produces a result that is
   equivalent to [D (hi, lo)]. In the special case where [hi] and [lo] are
   both empty, it produces [empty], thereby avoiding memory allocation.

   Furthermore, because [empty] is the sole empty set, [is_empty] can be
   implemented in the form of a physical equality test. (We assume that
   the OCaml compiler does not perform unsharing!) *)

(* Because [is_empty] is supposedly fast, one might wish to add a fast path
   to many (most) functions, where the case of an empty set receives special
   treatment. However, this would make the code longer, and it is not clear
   whether it would be beneficial overall, so it is not done. *)

let[@inline] construct hi lo =
  if W.is_empty hi && W.is_empty lo then
    empty
  else
    D (hi, lo)

let check s =
  let D (hi, lo) = s in
  if W.is_empty hi && W.is_empty lo then
    assert (s == empty)

let singleton i =
  if i < W.bound then
    D (W.empty, W.singleton i)
  else
    D (W.singleton (i - W.bound), W.empty)

let add i s =
  let D (hi, lo) = s in
  if i < W.bound then
    let lo' = W.add i lo in
    if lo == lo' then s else
    D (hi, lo')
  else
    let hi' = W.add (i - W.bound) hi in
    if hi == hi' then s else
    D (hi', lo)

let remove i s =
  let D (hi, lo) = s in
  if i < W.bound then
    let lo' = W.remove i lo in
    if lo == lo' then s else
    construct hi lo'
  else
    let hi' = W.remove (i - W.bound) hi in
    if hi == hi' then s else
    construct hi' lo

let union s1 s2 =
  let D (hi1, lo1) = s1
  and D (hi2, lo2) = s2 in
  let hi = W.union hi1 hi2
  and lo = W.union lo1 lo2 in
  if hi2 == hi && lo2 == lo then s2 else
  if hi1 == hi && lo1 == lo then s1 else
  D (hi, lo)

let inter s1 s2 =
  let D (hi1, lo1) = s1
  and D (hi2, lo2) = s2 in
  let hi = W.inter hi1 hi2
  and lo = W.inter lo1 lo2 in
  if hi2 == hi && lo2 == lo then s2 else
  if hi1 == hi && lo1 == lo then s1 else
  construct hi lo

let diff s1 s2 =
  let D (hi1, lo1) = s1
  and D (hi2, lo2) = s2 in
  let hi = W.diff hi1 hi2
  and lo = W.diff lo1 lo2 in
  if hi1 == hi && lo1 == lo then s1 else
  construct hi lo

let above x s =
  let D (hi, lo) = s in
  if x < W.bound then
    let lo' = W.above x lo in
    if lo == lo' then s else
    construct hi lo'
  else
    let hi' = W.above (x - W.bound) hi
    and lo' = W.empty in
    if hi == hi' && lo == lo' then s else
    construct hi' lo'

(* -------------------------------------------------------------------------- *)

(* Cardinality. *)

let[@inline] is_empty s =
  s == empty

let is_singleton s =
  let D (hi, lo) = s in
  W.is_empty hi && W.is_singleton lo ||
  W.is_singleton hi && W.is_empty lo

let cardinal s =
  let D (hi, lo) = s in
  W.cardinal hi + W.cardinal lo

(* -------------------------------------------------------------------------- *)

(* Tests. *)

let mem i s =
  let D (hi, lo) = s in
  if i < W.bound then
    W.mem i lo
  else
    W.mem (i - W.bound) hi

let equal s1 s2 =
  s1 == s2 ||
  let D (hi1, lo1) = s1
  and D (hi2, lo2) = s2 in
  W.equal hi1 hi2 &&
  W.equal lo1 lo2

let compare s1 s2 =
  if s1 == s2 then 0 else
  let D (hi1, lo1) = s1
  and D (hi2, lo2) = s2 in
  let c = W.compare hi1 hi2 in
  if c = 0 then W.compare lo1 lo2
  else c

let disjoint s1 s2 =
  let D (hi1, lo1) = s1
  and D (hi2, lo2) = s2 in
  W.disjoint hi1 hi2 && W.disjoint lo1 lo2

let subset s1 s2 =
  let D (hi1, lo1) = s1
  and D (hi2, lo2) = s2 in
  W.subset hi1 hi2 && W.subset lo1 lo2

let[@inline] quick_subset s1 s2 =
  not (disjoint s1 s2)

(* -------------------------------------------------------------------------- *)

(* Extraction. *)

let minimum s =
  let D (hi, lo) = s in
  if not (W.is_empty lo) then
    W.minimum lo
  else
    W.minimum hi + W.bound

let maximum s =
  let D (hi, lo) = s in
  if not (W.is_empty hi) then
    W.maximum hi + W.bound
  else
    W.maximum lo

let choose =
  minimum

(* -------------------------------------------------------------------------- *)

(* Iteration. *)

let iter yield s =
  let D (hi, lo) = s in
  W.iter yield lo;
  W.iter_delta W.bound yield hi

let fold yield s accu =
  let D (hi, lo) = s in
  let accu = W.fold yield lo accu in
  let accu = W.fold_delta W.bound yield hi accu in
  accu

let[@inline] elements s =
  (* Note: the list is produced in decreasing order. *)
  fold (fun tl hd -> tl :: hd) s []

let[@inline] of_list xs =
  List.fold_left (fun s x -> add x s) empty xs

exception Found of elt

let find_first_opt p s =
  try
    iter (fun x -> if p x then raise (Found x)) s;
    None
  with Found x ->
    Some x

(* -------------------------------------------------------------------------- *)

(* Decomposition. *)

let compare_minimum s1 s2 =
  match is_empty s1, is_empty s2 with
  | true , true  ->  0
  | true , false -> -1
  | false, true  -> +1
  | false, false ->
      let D (hi1, lo1) = s1
      and D (hi2, lo2) = s2 in
      match W.is_empty lo1, W.is_empty lo2 with
      | false, false -> W.compare_minimum lo1 lo2
      | true , false -> +1
      | false, true  -> -1
      | true , true  -> W.compare_minimum hi1 hi2

let[@inline] big_union ss =
  List.fold_left union empty ss

(* In the following two functions, we do *not* attempt to avoid memory
   allocation by testing whether one of the results happens to be
   structurally equal to one of the arguments. This would result in
   excessive pollution of the code. *)

let extract_unique_prefix s1 s2 =
  assert (not (is_empty s2));
  let D (hi1, lo1) = s1
  and D (hi2, lo2) = s2 in
  if W.equal hi1 hi2 && W.equal lo1 lo2 then empty, s1 else (* fast path *)
  if not (W.is_empty lo2) then
    (* [lo1] must be split; [hi1] is entirely outside of the unique prefix. *)
    let lo1a, lo1b = W.extract_unique_prefix lo1 lo2 in
    construct W.empty lo1a, construct hi1 lo1b
  else
    (* [lo1] is entirely part of the unique prefix; [hi1] must be split. *)
    let hi1a, hi1b = W.extract_unique_prefix hi1 hi2 in
    construct hi1a lo1, construct hi1b W.empty

let extract_shared_prefix s1 s2 =
  let D (hi1, lo1) = s1
  and D (hi2, lo2) = s2 in
  if not (W.equal lo1 lo2) then
    (* The shared prefix is a fragment of [lo]. *)
    let lo, (lo1, lo2) = W.extract_shared_prefix lo1 lo2 in
    construct W.empty lo, (construct hi1 lo1, construct hi2 lo2)
  else if not (W.equal hi1 hi2) then
    (* [lo1] is entirely part of the shared prefix. *)
    let hi, (hi1, hi2) = W.extract_shared_prefix hi1 hi2 in
    construct hi lo1, (construct hi1 W.empty, construct hi2 W.empty)
  else
    (* [s1] and [s2] are equal. *)
    s1, (empty, empty)

include Partition.Make(struct
  type nonrec t = t
  let is_empty = is_empty
  let compare = compare
  let compare_minimum = compare_minimum
  let big_union = big_union
  let extract_unique_prefix = extract_unique_prefix
  let extract_shared_prefix = extract_shared_prefix
end)
