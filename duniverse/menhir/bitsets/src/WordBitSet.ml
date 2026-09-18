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

type t =
  int

let check s =
  ignore s

type elt =
  int

let bound =
  Sys.word_size - 1

(* -------------------------------------------------------------------------- *)

(* [bit i] is [2^i]. *)

let[@inline] bit i =
  assert (0 <= i && i < bound);
  1 lsl i

(* -------------------------------------------------------------------------- *)

(* Under the assumption that [s] is a power of two, that is, a single
   bit is set in [s], [tib s] is the base-2 logarithm of [s].

   The function [tib] is so named because it is the inverse of [bit]:
   [tib (bit i) = i]. *)

(* It would be nice if we could use gcc's __builtin_clz to do this.
   See caml_z.c in the library zarith. *)

let[@inline] tib2 accu s =
  (* [s] is 2 bits wide, so it is either 1 or 2. *)
  if s = 1 then accu else accu + 1

let tib4 accu s =
  (* [s] is 4 bits wide. *)
  if s land 0x3 = 0 then
    tib2 (accu + 2) (s lsr 2)
  else
    tib2 accu s

let tib8 accu s =
  if s land 0xF = 0 then
    tib4 (accu + 4) (s lsr 4)
  else
    tib4 accu s

let tib16 accu s =
  if s land 0xFF = 0 then
    tib8 (accu + 8) (s lsr 8)
  else
    tib8 accu s

let tib32 accu s =
  if s land 0xFFFF = 0 then
    tib16 (accu + 16) (s lsr 16)
  else
    tib16 accu s

let ffffffff =
  (0xffff lsl 16) lor 0xffff
  (* We cannot use the literal 0xffffffff because the OCaml compiler will
     reject it when compiling for a 32-bit machine. *)

let tib64 s =
  if s land ffffffff = 0 then
    tib32 32 (s lsr 32)
  else
    tib32 0 s

let tib s =
  match Sys.word_size with
  | 32 -> tib32 0 s
  | 64 -> tib64 s
  | _ -> assert false

let () =
  (* A sanity check. *)
  assert (
    for i = 0 to bound - 1 do assert (tib (bit i) = i) done;
    true
  )

(* -------------------------------------------------------------------------- *)

(* [pop s] is the population count of [s],
   that is, the number of bits that are set in [s]. *)

(* Again, it would be nice if we could use gcc's __builtin_popcount. *)

let b0 = 0x55555555
let b1 = 0x33333333
let b2 = 0xf0f0f0f

(* https://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel *)

let pop32 s =
  (* Count the bits inside each byte, in parallel. *)
  let s = s         - (s lsr 1) land b0 in
  let s = s land b1 + (s lsr 2) land b1 in
  let s = s land b2 + (s lsr 4) land b2 in
  (* Add up the four counts found in the four bytes. Each of these counts is
     at most 8, so the sum is at most 32, and fits in a byte. *)
  let s = s + s lsr 8 in
  let s = s + s lsr 16 in
  s land 0xff

let[@inline] pop64 s =
  pop32 s + pop32 (s lsr 32)
    (* Because [pop32] examines only the lower 32 bits, we pass [s] to
       [pop32] without bothering to mask off the higher 32 bits. *)

let pop s =
  match Sys.word_size with
  | 32 -> pop32 s
  | 64 -> pop64 s
  | _ -> assert false

(* -------------------------------------------------------------------------- *)

(* [fill_down s] fills a bit set with 1's, starting at the most significant
   set bit. In other words, it replaces all 0's with 1's, except for the
   leading zeros. *)

let[@inline] fill_down s =
  let s = s lor (s lsr 1) in
  let s = s lor (s lsr 2) in
  let s = s lor (s lsr 4) in
  let s = s lor (s lsr 8) in
  let s = s lor (s lsr 16) in
  let s = s lor (s lsr 32) in (* needed only on 64-bit machines *)
  s

(* -------------------------------------------------------------------------- *)

(* Construction. *)

let empty =
  0

let singleton =
  bit

let[@inline] add x s =
  (bit x) lor s

let[@inline] remove x s =
  (lnot (bit x)) land s

let[@inline] union s1 s2 =
  s1 lor s2

let[@inline] inter s1 s2 =
  s1 land s2

let[@inline] diff s1 s2 =
  s1 land lnot s2

(* [above x s] is the subset { y ∈ s | x < y }. *)

let[@inline] above x s =
  (* A mask with only [x] set. *)
  let mask = bit x in
  (* A mask with all elements less than or equal to [x] set. *)
  let mask = mask lor (mask - 1) in
  (* Remove these elements. *)
  diff s mask

(* -------------------------------------------------------------------------- *)

(* Cardinality. *)

let[@inline] is_empty s =
  s = 0

let[@inline] is_singleton s =
  s <> 0 &&
  (* Test whether only one bit is set in [ss]. We do this by turning
     off the rightmost bit, then comparing to zero. *)
  s land (s - 1) = 0

let cardinal =
  pop

(* -------------------------------------------------------------------------- *)

(* Tests. *)

let[@inline] equal (s1 : int) (s2 : t) : bool =
  s1 = s2       (* generic equality, used at type [int] *)

let[@inline] compare (s1 : int) (s2 : t) : int =
  compare s1 s2 (* generic ordering, used at type [int] *)

let[@inline] disjoint s1 s2 =
  is_empty (inter s1 s2)

let[@inline] subset s1 s2 =
  s1 land s2 = s1

let[@inline] mem x s =
  subset (singleton x) s

let[@inline] quick_subset s1 s2 =
  not (disjoint s1 s2)

(* -------------------------------------------------------------------------- *)

(* Extraction. *)

(* [lsb s] retains only the least significant bit in the bit set [s].
   For example, the bit pattern 0110 is transformed to 0010. The bit
   set 0 is mapped to itself.

   If the bit set [s] is nonempty, then [lsb s] can be described as a
   singleton set whose element is [minimum s]. *)

let[@inline] lsb s =
  s land (-s)

let minimum s =
  if is_empty s then
    raise Not_found
  else
    tib (lsb s)

let maximum s =
  if is_empty s then
    raise Not_found
  else if mem (bound - 1) s then
    bound - 1
  else
    (* We compute the maximum element of the set [s] as follows. First, we
       compute [fill_down s], whose bit pattern is a nonempty sequence of 0's
       followed with a nonempty sequence of 1's. Then, we negate this pattern:
       we obtain a nonempty sequence of 1's followed with a nonempty sequence
       of 0's. We use [minimum] to obtain the minimum element of this set, and
       subtract 1. *)
    (* This approach may seem complex, but allows us to rely on [minimum],
       which (hopefully) we have implemented in an efficient way. *)
    minimum (lnot (fill_down s)) - 1

let choose =
  minimum

(* -------------------------------------------------------------------------- *)

(* Iteration. *)

let rec iter yield s =
  if s <> 0 then
    let x = lsb s in
    let s = s lxor x in
    yield (tib x);
    iter yield s

let rec fold yield s accu =
  if s = 0 then
    accu
  else
    let x = lsb s in
    let s = s lxor x in
    let accu = yield (tib x) accu in
    fold yield s accu

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

let rec iter_delta delta yield s =
  if s <> 0 then
    let x = lsb s in
    let s = s lxor x in
    yield (delta + tib x);
    iter_delta delta yield s

let rec fold_delta delta yield s accu =
  if s = 0 then
    accu
  else
    let x = lsb s in
    let s = s lxor x in
    let accu = yield (delta + tib x) accu in
    fold_delta delta yield s accu

(* -------------------------------------------------------------------------- *)

(* Decomposition. *)

let[@inline] compare_minimum ss1 ss2 =
  compare (lsb ss1 - 1) (lsb ss2 - 1)
  (* [lsb] produces a singleton set containing just the minimum element --
     or an empty set.

     If we had an unsigned integer comparison, we would use it directly.
     However, [compare] performs a signed comparison. If one the input
     sets is [min_int], which represents a singleton set where the most
     significant bit set, then [compare] would return an incorrect result.

     By subtracting one, we fix this corner case:
     - [min_int - 1] is [max_int], which is larger than anything
     - [0 - 1] is [-1], which is smaller than anything
     - for all (singleton) bit sets other than [min_int] and [0],
       the ordering is preserved. *)

let[@inline] big_union ss =
  List.fold_left union empty ss

let[@inline] extract_unique_prefix s1 s2 =
  assert (not (is_empty s2));
  let mask = lsb s2 - 1 in
  inter s1 mask, diff s1 mask

let[@inline] extract_shared_prefix s1 s2 =
  let s1' = diff s1 s2 in
  let s2' = diff s2 s1 in
  let mask = (lsb s1' - 1) land (lsb s2' - 1) in
  inter s1 mask, (diff s1 mask, diff s2 mask)

include Partition.Make(struct
  type nonrec t = t
  let is_empty = is_empty
  let compare = compare
  let compare_minimum = compare_minimum
  let big_union = big_union
  let extract_unique_prefix = extract_unique_prefix
  let extract_shared_prefix = extract_shared_prefix
end)

(* -------------------------------------------------------------------------- *)

(* Odds and ends. *)

let[@inline] shift s delta =
  assert (0 <= delta && delta <= bound);
  (s lsl delta, s lsr (bound - delta))
