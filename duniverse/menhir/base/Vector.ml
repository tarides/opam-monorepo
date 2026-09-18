(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

module A = struct
  include Array
  let empty = [||]
  let alloc = make
  let grow n x a k =
    let b = Array.make n x in
    Array.blit a 0 b 0 k;
    b
  let unsafe_blit = blit
end

(* -------------------------------------------------------------------------- *)

(**[validate_segment n ofs len] checks that the offset [ofs] and the
   length [len] determine a valid interval inside an array or vector of
   length [n]. *)

let[@inline never] invalid_segment n ofs len =
  Printf.ksprintf invalid_arg
    "invalid segment (ofs = %d, len = %d) in a sequence of length %d"
    ofs len n

let[@inline] validate_segment n ofs len =
  if not (0 <= len && 0 <= ofs && ofs + len <= n) then
    invalid_segment n ofs len

(* -------------------------------------------------------------------------- *)

(* Types. *)

type length = int
type capacity = int
type index = int

(* In [create] and in [set_higher_capacity], the allocation of an array of
   size [capacity] is delayed, because we do not have an array element at
   hand. To tolerate this, we accept the possibility that, sometimes,
   [capacity] is nonzero, while the [data] array is still empty. *)

type 'a vector = {

  (* The logical length of the vector. *)
  mutable length   : int;

  (* The desired physical capacity of the vector. We impose the invariant
     [length = 0 || A.length data = capacity]. That is, unless the vector
     is logically empty, [capacity] is the length of the [data] array. *)
  mutable capacity : int;

  (* The data array. *)
  mutable data     : 'a A.t;

}

type 'a t =
  'a vector

(* -------------------------------------------------------------------------- *)

(* Local copies of [min] and [max], with annotations. *)

let[@inline] min (x : int) (y : int) = if x <= y then x else y
let[@inline] max (x : int) (y : int) = if x >= y then x else y

(* -------------------------------------------------------------------------- *)

(* [check v] checks that the invariant holds. *)

let (* public *) check v =
  let { length; capacity; data } = v in
  assert (0 <= length);
  assert (length <= capacity);
  assert (length = 0 || A.length data = capacity);
  assert (A.length data = 0 || A.length data = capacity);
  (* The following assertion follows from the previous ones: *)
  assert (length <= A.length data)

(* -------------------------------------------------------------------------- *)

(* Error messages for our defensive checks. *)

(* We set [defensive] unconditionally to [true]. (We could make [defensive]
   a parameter of this module, but that would add overhead and complication.
   We could also offer two variants of the module, an optimistic one and a
   defensive one, but that would also add complication.) *)

(* Being defensive allows us to use [A.unsafe_get] and [A.unsafe_set], thus
   bypassing array bounds checks. In most places, this is safe, because our
   defensive checks are strong enough. In [get] and [set], our defensive
   checks are strong enough if the data structure is used sequentially, but
   can be insufficient if there is a data race. Thus, racy accesses to a
   vector can break memory safety! but we accept this risk. *)

let defensive =
  true

let[@inline] fail format =
  Printf.ksprintf invalid_arg format

let[@inline never] capacity_failure capacity =
  fail "invalid capacity %d" capacity

let[@inline never] length_failure n =
  fail "invalid length %d" n

let[@inline never] index_failure v i =
  fail "index %d is out of range [0, %d)" i v.length

(* [validate length data] checks [length <= A.length data]. This property is
   part of our invariant, and can be violated only through racy accesses. *)

(* Every time we read both the [length] and [data] fields of a vector, we
   call [validate length data], so as to protect the user against violations
   caused by data races. (That said, [unsafe_borrow], [unsafe_get], and
   [unsafe_set], which read just the [data] field, are still unsafe.) *)

let[@inline never] violation length data =
  fail "vector length is %d, but data array has length %d (racy access?)"
    length (A.length data)

let[@inline] validate length data =
  if defensive && not (length <= A.length data) then
    violation length data

(* -------------------------------------------------------------------------- *)

(* Construction. *)

let[@inline] (* public *) create () =
  let length = 0
  and capacity = 0
  and data = A.empty in
  { length; capacity; data }

let[@inline] (* private *) init n f =
  let length = n
  and capacity = n in
  let data = A.init capacity f in
  { length; capacity; data }

let[@inline] (* private *) make n x =
  let length = n
  and capacity = n in
  let data = A.make capacity x in
  { length; capacity; data }

let[@inline] (* private *) unsafe_of_array_segment a o k =
  assert (0 <= o && 0 <= k && o + k <= A.length a);
  (* The length of the array segment is the capacity of the new vector. *)
  let length = k in
  let capacity = length in
  let data = A.sub a o k in
  { length; capacity; data }

let[@inline] (* public *) of_array a =
  (* The length of the original array is the capacity of the new vector. *)
  unsafe_of_array_segment a 0 (A.length a)

let[@inline] (* public *) copy v =
  (* The length of the original vector is the capacity of the new vector. *)
  let { length; data; _ } = v in validate length data;
  unsafe_of_array_segment data 0 length

let[@inline] (* private *) unsafe_steal_array a =
  let k = A.length a in
  let length = k in
  let capacity = length in
  let data = a in (* no copy *)
  { length; capacity; data }

let[@inline] (* public *) of_list xs =
  unsafe_steal_array (Array.of_list xs)

(* -------------------------------------------------------------------------- *)

(* Access. *)

let[@inline] (* public *) length v =
  v.length

let[@inline] (* public *) is_empty v =
  length v = 0

let[@inline] (* public *) unsafe_borrow v =
  v.data

let (* public *) to_array v =
  let { length; data; _ } = v in validate length data;
  A.sub data 0 length

(* In [unsafe_get] and [unsafe_set], our use of [A.unsafe_get] and
   [A.unsafe_set] is NOT completely safe. We have validated the
   index [i], but, if there is a data race, then by the time we read
   [v.data], we might find that [i] is not a valid index in this
   array. That would break memory safety! but we accept this risk. *)

(* [get] and [set] inherit this risk. *)

let[@inline] (* private *) unsafe_get v i =
  A.unsafe_get v.data i (* not entirely safe *)

let[@inline] (* private *) unsafe_set v i x =
  A.unsafe_set v.data i x (* not entirely safe *)

(* -------------------------------------------------------------------------- *)

(* Popping, peeking, truncating, clearing. *)

let (* public *) pop v =
  let { length; data; _ } = v in validate length data;
  if length > 0 then begin
    let i = length - 1 in
    v.length <- i;
    A.unsafe_get data i (* safe *)
  end
  else
    raise Not_found

let (* public *) pop_last =
  pop

let (* public *) pop_opt v =
  let { length; data; _ } = v in validate length data;
  if length > 0 then begin
    let i = length - 1 in
    v.length <- i;
    Some (A.unsafe_get data i)
  end
  else
    None

let (* public *) pop_last_opt =
  pop_opt

let (* public *) drop v =
  let { length; _ } = v in
  if length > 0 then
    let i = length - 1 in
    v.length <- i

let (* public *) remove_last =
  drop

let (* public *) top v =
  let { length; data; _ } = v in validate length data;
  if length > 0 then
    A.unsafe_get data (length - 1) (* safe *)
  else
    raise Not_found

let (* public *) get_last =
  top

let (* public *) top_opt v =
  let { length; data; _ } = v in validate length data;
  if length > 0 then
    Some (A.unsafe_get data (length - 1))
  else
    None

let (* public *) find_last =
  top_opt

let[@inline] (* private *) truncate v n =
  let { length; _ } = v in
  if n < length then
    v.length <- n

let[@inline] (* public *) clear v =
  v.length <- 0

let (* public *) reset v =
  v.length <- 0;
  v.capacity <- 0;
  v.data <- A.empty

(* -------------------------------------------------------------------------- *)

(* Changing the vector's capacity and/or re-allocating the [data]
   array to match its capacity, are the most tricky aspects of this
   data structure. *)

(* One must keep in mind the invariant that if [v.length] is nonzero
   then [v.capacity] is the length of the [data] array. *)

(* [set_lower_capacity] decreases the vector's capacity. *)

let[@inline] set_lower_capacity v new_capacity =
  assert (new_capacity < v.capacity);
  v.capacity <- new_capacity;
  (* If the [data] array is nonempty, then it must be truncated so as to
     match the new capacity. If it is empty, then [v.length] must be zero,
     so neither [v.length] nor [v.data] needs to be updated. *)
  let { length; data; _ } = v in
  (* No need to call [validate], as we do not access the [data] array. *)
  if 0 < A.length data then begin
    v.length <- min length new_capacity;
    v.data <- A.sub data 0 new_capacity
  end

(* [really_set_higher_capacity] increases the vector's capacity and
   immediately re-allocates the [data] array so as to match the new
   capacity. The value [dummy] is used to initialize unused slots. *)

let really_set_higher_capacity v new_capacity dummy =
  let { length; data; _ } = v in validate length data;
  assert (length <= new_capacity);
  v.capacity <- new_capacity;
  let new_data = A.grow new_capacity dummy data length in
  v.data <- new_data;
  new_data

(* [set_higher_capacity] increases the vector's capacity. The [data]
   array is re-allocated only if a dummy value is at hand. *)

let set_higher_capacity v new_capacity =
  let { data; _ } = v in
  if A.length data = 0 then
    (* The allocation of an array of size [capacity] is delayed,
       because we do not have a value of type ['a] at hand. *)
    v.capacity <- new_capacity
  else
    let dummy = A.unsafe_get data 0 in (* safe *)
    let _data = really_set_higher_capacity v new_capacity dummy in
    ()

let[@inline] (* private *) set_capacity v new_capacity =
  let { capacity; _ } = v in
  if new_capacity < capacity then
    set_lower_capacity v new_capacity
  else if new_capacity > capacity then
    set_higher_capacity v new_capacity

(* [next_capacity] decides by how much to increase the vector's capacity. *)

(* We jump to size at least 8 in all situations; we grow by a factor of 2
   under a certain threshold, and by a factor of 3/2 beyond this threshold. *)

(* This strategy is taken from the standard library's [Dynarray] module. *)

(* Whereas [Dynarray] ensures that the result of [next_capacity] is at most
   [Sys.max_array_length], we do not. On 64 bit machines, the value of
   [Sys.max_array_length] is so high (about 128,000 terabytes) that this
   limit is unlikely to be exceeded. If (in the future) we wanted to take
   explicit precautions about this limit, we should make it a field in the
   module [A], so as to remain independent of the details of OCaml's arrays. *)

let[@inline] next_capacity capacity =
  max 8 (
    if capacity <= 512 then capacity * 2
    else capacity + capacity / 2
  )

let[@inline] (* private *) ensure_capacity v request =
  let { capacity; _ } = v in
  if request > capacity then begin
    (* Ensure that the vector's capacity is at least [request]. We use
       [set_higher_capacity], so the [data] array is not necessarily grown. *)
    let new_capacity = max (next_capacity capacity) request in
    assert (new_capacity >= request);
    set_higher_capacity v new_capacity
  end

let[@inline] (* private *) ensure_extra_capacity v delta =
  ensure_capacity v (length v + delta)

let (* public *) fit_capacity v =
  let { length; capacity; _ } = v in
  if length < capacity then
    set_lower_capacity v length

(* [really_ensure_capacity v request dummy] ensures that the requested
   capacity [request] really is available now in the [data] array, not
   just recorded in the [capacity] field. *)

let[@inline never] (* private *) really_ensure_capacity v request dummy =
  let { capacity; _ } = v in
  let new_capacity =
    if request <= capacity then capacity
    else max (next_capacity capacity) request
  in
  assert (new_capacity >= request);
  really_set_higher_capacity v new_capacity dummy

(* -------------------------------------------------------------------------- *)

(* Pushing. *)

(* We separate the slow path (the unusual case) so that the fast path
   (the common case) can be marked [@inline]. On the fast path, one test
   suffices. *)

(* Calling [validate] is not necessary here; the code is written in
   such a way that our accesses to the [data] array are safe. *)

let[@inline] (* public *) push v x =
  let { length; data; _ } = v in
  (* Ensure that sufficient space exists in the [data] array. *)
  (* [x] is used as a dummy value. *)
  let new_length = length + 1 in
  let data =
    if new_length <= A.length data then data
    else really_ensure_capacity v new_length x
  in
  (* A physical array slot now exists. *)
  A.unsafe_set data (new_length - 1) x; (* safe *)
  v.length <- new_length

let (* public *) add_last =
  push

let[@inline] (* private *) unsafe_push_array_segment v a ofs len =
  assert (0 <= ofs && 0 <= len && ofs + len <= A.length a);
  let { length; data; _ } = v in
  let new_length = length + len in
  (* Ensure that sufficient space exists in the [data] array. *)
  (* If there is insufficient space, then it must be the case
     that [len] is nonzero, so reading [a.(0)] is safe. *)
  let data =
    if new_length <= A.length data then data
    else
      really_ensure_capacity v new_length
        (assert (0 < len); A.unsafe_get a 0 (* safe *))
  in
  (* Physical array slots now exist. *)
  v.length <- new_length;
  A.unsafe_blit a ofs data length len

let[@inline] (* public *) push_array v a =
  let ofs = 0
  and len = A.length a in
  unsafe_push_array_segment v a ofs len

let (* public *) append_array =
  push_array

let[@inline] (* public *) push_vector v v' =
  let { length; data; _ } = v' in validate length data;
  unsafe_push_array_segment v data 0 length
    (* This works even if [v] and [v'] are the same vector. In all cases, we
       are reading from a data array and writing to a data array (which may
       or may not be the same array), and the source and destination ranges
       are disjoint. *)

let (* public *) append =
  push_vector

let (* public *) push_list v xs =
  let len = List.length xs in
  let { length; data; _ } = v in
  let new_length = length + len in
  (* Ensure that sufficient space exists in the [data] array. *)
  (* If there is insufficient space, then it must be the case
     that [len] is nonzero, so calling [List.hd xs] is safe. *)
  let data =
    if new_length <= A.length data then data
    else
      really_ensure_capacity v new_length
        (assert (0 < len); List.hd xs (* safe *))
  in
  (* Physical array slots now exist. *)
  v.length <- new_length;
  (* We want to blit the list [xs], whose length is [len], into the array
     [data] at offset [length]. This can be done with a loop. *)
  let xs = ref xs
  and dst = ref length in
  for _ = 0 to len - 1 do
    match !xs with [] -> assert false | x :: rest ->
    A.unsafe_set data !dst x; (* safe *)
    dst := !dst + 1;
    xs := rest
  done;
  assert (!xs = [])

let (* public *) append_list =
  push_list

let (* public *) push_seq v xs =
  push_list v (List.of_seq xs)
    (* I do not feel the need to optimize this function for speed. *)

let (* public *) append_seq =
  push_seq

let (* public *) of_seq xs =
  let v = create() in push_seq v xs; v
    (* I do not feel the need to optimize this function for speed. *)

(* In [push_iter], assuming that we are not allowed to call [iter] twice,
   the best and simplest implementation is to just iterate [push]. Using
   temporary storage would not help. *)

(* If we are allowed to call [iter] twice, then we can call it once to
   count how many elements must be appended, and call it again to
   actually write these elements into the vector. This might be more
   efficient in some cases, but that is not entirely clear. *)

(* We want to be compatible with [Dynarray], whose documentation does
   not indicate whether [push_iter] may call [iter] more than once. For
   maximum compatibility, we must assume that this is not permitted. *)

let[@inline] (* public *) push_iter v iter c =
  iter (push v) c

let (* public *) append_iter =
  push_iter

(* -------------------------------------------------------------------------- *)

(* Operations on array segments. *)

module ArraySegment = struct

let[@inline] iter f a i j =
  assert (0 <= i && i <= j && i <= A.length a);
  for i = i to j-1 do f (A.unsafe_get a i) done

let[@inline] iter_down f a i j =
  assert (0 <= i && i <= j && i <= A.length a);
  for i = j-1 downto i do f (A.unsafe_get a i) done

let[@inline] iteri f a i j =
  assert (0 <= i && i <= j && i <= A.length a);
  for i = i to j-1 do f i (A.unsafe_get a i) done

let[@inline] fold_left f accu a i j =
  assert (0 <= i && i <= j && i <= A.length a);
  let accu = ref accu in
  for i = i to j-1 do accu := f !accu (A.unsafe_get a i) done;
  !accu

let[@inline] fold_right f a i j accu =
  assert (0 <= i && i <= j && i <= A.length a);
  let accu = ref accu in
  for i = j-1 downto i do accu := f (A.unsafe_get a i) !accu done;
  !accu

let[@inline] to_list a i j =
  assert (0 <= i && i <= j && i <= A.length a);
  let accu = ref [] in
  for i = j-1 downto i do accu := A.unsafe_get a i :: !accu done;
  !accu

let rec to_seq a i j =
  assert (0 <= i && i <= j && i <= A.length a);
  if i = j then
    Seq.empty
  else
    fun () -> Seq.Cons (A.unsafe_get a i, to_seq a (i + 1) j)

let rec to_seq_rev a i j =
  assert (0 <= i && i <= j && i <= A.length a);
  if i = j then
    Seq.empty
  else
    fun () ->
      let j = j - 1 in
      Seq.Cons (A.unsafe_get a j, to_seq_rev a i j)

let[@inline] exists f a i j =
  assert (0 <= i && i <= j && i <= A.length a);
  let exception Break in
  try
    for i = i to j-1 do if f (A.unsafe_get a i) then raise Break done;
    false
  with Break ->
    true

let[@inline] for_all f a i j =
  assert (0 <= i && i <= j && i <= A.length a);
  let exception Break in
  try
    for i = i to j-1 do if not (f (A.unsafe_get a i)) then raise Break done;
    true
  with Break ->
    false

let[@inline] equal equal a1 i1 j1 a2 i2 j2 =
  assert (0 <= i1 && i1 <= j1 && i1 <= A.length a1);
  assert (0 <= i2 && i2 <= j2 && i2 <= A.length a2);
  let length1 = j1 - i1
  and length2 = j2 - i2 in
  length1 = length2 &&
  let exception Break in
  try
    let delta = i2 - i1 in
    for i = i1 to j1-1 do
      if not (equal (A.unsafe_get a1 i) (A.unsafe_get a2 (i + delta))) then
        raise Break
    done;
    true
  with Break ->
    false

let[@inline] compare compare a1 i1 j1 a2 i2 j2 =
  assert (0 <= i1 && i1 <= j1 && i1 <= A.length a1);
  assert (0 <= i2 && i2 <= j2 && i2 <= A.length a2);
  let length1 = j1 - i1
  and length2 = j2 - i2 in
  let exception Break of int in
  try
    let delta = i2 - i1 in
    for i = i1 to i1 + min length1 length2 - 1 do
      let c = compare (A.unsafe_get a1 i) (A.unsafe_get a2 (i + delta)) in
      if c <> 0 then
        raise (Break c)
    done;
    Stdlib.Int.compare length1 length2
  with Break c ->
    c

let rec find f a i j =
  assert (0 <= i && i <= j && i <= A.length a);
  if i = j then
    raise Not_found
  else if f (A.unsafe_get a i) then
    i
  else
    find f a (i + 1) j

(* -------------------------------------------------------------------------- *)

(* Our stable sort algorithm is taken from OCaml's [Stdlib.Array] and adapted
   to use our array module [A] and to sort an array segment. *)

(* [disjoint a1 ofs1 len1 a2 ofs2 len2] tests whether the array segments
   described by [a1], [ofs1], [len1] and [a2], [ofs2], [len2] are disjoint.
   It is used only in assertions. *)

let disjoint a1 ofs1 len1 a2 ofs2 len2 =
  a1 != a2 ||
  ofs1 + len1 <= ofs2 ||
  ofs2 + len2 <= ofs1

(* [suffix a1 ofs1 len1 a2 ofs2 len2] tests whether the array segment
   described by [a1], [ofs1], [len1] is a suffix of the array segment
   described by [a2], [ofs2], [len2]. It is used only in assertions. *)

let suffix a1 ofs1 len1 a2 ofs2 len2 =
  a1 == a2 &&
  ofs1 + len1 = ofs2 + len2 &&
  len1 <= len2

(* If the input data is already sorted, or close to sorted, it may be the
   case that the calls from [merge] (below) to [blit] copy an array segment
   to itself. We recognize this situation, where there is nothing to do. *)

let[@inline] terminal_blit src srcofs dst dstofs len =
  if src == dst && srcofs = dstofs then
    ()
  else
    A.unsafe_blit src srcofs dst dstofs len

(* [merge cmp src1 src1ofs src1len src2 src2ofs src2len dst dstofs] merges
   the sorted array segments described by [src1], [src1ofs], [src1len] and
   [src2], [src2ofs], [src2len]. The resulting data is written into the
   array segment described by [dst], [dstofs], and [src1len + src2len].
   One of the source segments (say, the first one) must be disjoint with
   the destination segment. The other source segment (say, the second one)
   can be either disjoint with the destination segment or a suffix of the
   destination segment. (This works because the data in the second source
   segment is then moved left: it is read before it is overwritten.) *)

(* [src1len] and [src2len] must be nonzero. *)

(* This is a stable merge: when [s1] and [s2] are equal, [s1] is favored. *)

let merge cmp src1 src1ofs src1len src2 src2ofs src2len dst dstofs =
  assert (0 < src1len && 0 < src2len);
  assert (
    let dstlen = src1len + src2len in
    (* Either both source segments are disjoint with the destination segment, *)
    disjoint src1 src1ofs src1len dst dstofs dstlen &&
    disjoint src2 src2ofs src2len dst dstofs dstlen ||
    (* or the first source segment is a suffix of the destination segment, *)
      suffix src1 src1ofs src1len dst dstofs dstlen &&
    disjoint src2 src2ofs src2len dst dstofs dstlen ||
    (* or the second source segment is a suffix of the destination segment. *)
    disjoint src1 src1ofs src1len dst dstofs dstlen &&
      suffix src2 src2ofs src2len dst dstofs dstlen
  );
  let src1r = src1ofs + src1len
  and src2r = src2ofs + src2len in
  let rec loop i1 s1 i2 s2 d =
    if cmp s1 s2 <= 0 then begin
      A.unsafe_set dst d s1;
      let i1 = i1 + 1 in
      if i1 < src1r then
        loop i1 (A.unsafe_get src1 i1) i2 s2 (d + 1)
      else
        terminal_blit src2 i2 dst (d + 1) (src2r - i2)
    end else begin
      A.unsafe_set dst d s2;
      let i2 = i2 + 1 in
      if i2 < src2r then
        loop i1 s1 i2 (A.unsafe_get src2 i2) (d + 1)
      else
        terminal_blit src1 i1 dst (d + 1) (src1r - i1)
    end
  in
  loop src1ofs (A.unsafe_get src1 src1ofs) src2ofs (A.unsafe_get src2 src2ofs) dstofs

(* Although [merge] (above) works in all situations, we can make it much
   faster in the special case where the data in the first source segment is
   less than or equal to the data in the second source segment. Indeed, in
   that case, two calls to [blit] suffice. The cost of recognizing this
   special case is two reads, a comparison, and a conditional. *)

let[@inline] optimistic_merge
    cmp src1 src1ofs src1len src2 src2ofs src2len dst dstofs =
  assert (0 < src1len && 0 < src2len);
  let last1  = A.unsafe_get src1 (src1ofs + src1len - 1)
  and first2 = A.unsafe_get src2 src2ofs in
  if cmp last1 first2 <= 0 then begin
    A.unsafe_blit src1 src1ofs dst dstofs src1len;
    A.unsafe_blit src2 src2ofs dst (dstofs + src1len) src2len
  end
  else
    merge cmp src1 src1ofs src1len src2 src2ofs src2len dst dstofs

(* Even better: in the special case where the second source segment is a
   suffix of the destination segment, [optimistic_merge] is simplified: the
   second call to [blit] has no effect, as it copies the second source
   segment to itself. Thus, it can be removed. *)

let[@inline] magic_optimistic_merge
    cmp src1 src1ofs src1len src2 src2ofs src2len dst dstofs =
  (* Check that the second source segment
     is a suffix of the destination segment. *)
  assert (src2 == dst && dstofs + src1len = src2ofs);
  assert (0 < src1len && 0 < src2len);
  let last1  = A.unsafe_get src1 (src1ofs + src1len - 1)
  and first2 = A.unsafe_get src2 src2ofs in
  if cmp last1 first2 <= 0 then
    A.unsafe_blit src1 src1ofs dst dstofs src1len
  else
    merge cmp src1 src1ofs src1len src2 src2ofs src2len dst dstofs

(* [isortto cmp src srcofs dst dstofs len] sorts the array segment described
   by [src], [srcofs], [len]. The resulting data is written into the array
   segment described by [dst], [dstofs], [len]. The source and destination
   segments must either coincide or be disjoint. This is an insertion sort. *)

let isortto cmp src srcofs dst dstofs len =
  assert (
    src == dst && srcofs = dstofs ||
    disjoint src srcofs len dst dstofs len
  );
  for i = 0 to len - 1 do
    let e = A.unsafe_get src (srcofs + i) in
    let j = ref (dstofs + i - 1) in
    while !j >= dstofs && cmp (A.unsafe_get dst !j) e > 0 do
      A.unsafe_set dst (!j + 1) (A.unsafe_get dst !j);
      decr j
    done;
    A.unsafe_set dst (!j + 1) e
  done

(* The cutoff determines where we switch from merge sort to insertion sort.
   OCaml uses 5. I find 8 to be slightly better, but the difference is very
   small: about 2%. The best setting is likely machine-dependent. *)

let cutoff = 8

(* [sortto cmp src srcofs dst dstofs len] sorts the array segment described
   by [src], [srcofs], [len]. The resulting data is written into the array
   segment described by [dst], [dstofs], [len]. The destination segment must
   be disjoint from the source segment. This is a merge sort, with an
   insertion sort at the leaves. It is a stable sort. *)

let rec sortto cmp src srcofs dst dstofs len =
  assert (disjoint src srcofs len dst dstofs len);
  if len <= cutoff then
    isortto cmp src srcofs dst dstofs len
  else begin
    let len1 = len / 2 in
    let len2 = len - len1 in
    (* The second half of [src] can be larger by one slot. *)
    assert (len1 <= len2 && len2 <= len1 + 1);
    (* Sort the second half of [src] and move it to the second half of [dst]. *)
    sortto cmp src (srcofs + len1) dst (dstofs + len1) len2;
    (* Sort the first half of [src] and move it to the second half of [src]. *)
    (* This requires [len1 <= len2]. *)
    sortto cmp src srcofs src (srcofs + len2) len1;
    (* Merge the two sorted halves, moving the data to [dst]. *)
    (* This is an in-place merge: the second source segment is contained
       within the destination segment! *)
    (* This is a stable sort, because the first half of the original
       data (now moved and sorted) is the first argument to [merge]. *)
    magic_optimistic_merge cmp
      src (srcofs + len2) len1 dst (dstofs + len1) len2 dst dstofs
  end

(* [unsafe_stable_sort_segment cmp a ofs len] sorts the array segment
   described by [a], [ofs], [len]. This array segment is sorted in place.
   This function is named [unsafe] because it does not validate [ofs] and
   [len]. *)

let unsafe_stable_sort cmp a ofs len =
  let base = ofs in
  if len <= cutoff then
    isortto cmp a base a base len
  else begin
    let len1 = len / 2 in
    let len2 = len - len1 in
    (* The second half of [a] can be larger by one slot. *)
    assert (len1 <= len2 && len2 <= len1 + 1);
    (* Allocate a temporary array that fits the second half of [a]. *)
    let t = A.alloc len2 (A.unsafe_get a base) in
    (* Sort the second half of [a] and move it to [t]. *)
    sortto cmp a (base + len1) t 0 len2;
    (* Sort the first half of [a] and move it to the second half of [a]. *)
    (* This requires [len1 <= len2]. *)
    sortto cmp a base a (base + len2) len1;
    (* Merge the two sorted halves, moving the data to [a]. *)
    (* This is an in-place merge: the first source segment is contained
       within the destination segment! *)
    (* This is a stable sort, because the first half of the original
       data (now moved and sorted) is the first argument to [merge]. *)
    optimistic_merge cmp a (base + len2) len1 t 0 len2 a base
  end

end (* ArraySegment *)

(* -------------------------------------------------------------------------- *)

(* Iterating, searching, showing. *)

(* Calling [validate] ensures that our use of [unsafe_get] is safe. *)

let (* public *) iter f v =
  let { length; data; _ } = v in validate length data;
  ArraySegment.iter f data 0 length

let (* public *) iter_down f v =
  let { length; data; _ } = v in validate length data;
  ArraySegment.iter_down f data 0 length

let (* public *) iteri f v =
  let { length; data; _ } = v in validate length data;
  ArraySegment.iteri f data 0 length

let (* public *) map f v =
  let { length; data; _ } = v in validate length data;
  init length (fun i -> f (A.unsafe_get data i) (* safe *))

let (* public *) mapi f v =
  let { length; data; _ } = v in validate length data;
  init length (fun i -> f i (A.unsafe_get data i) (* safe *))

let (* public *) fold_left f accu v =
  let { length; data; _ } = v in validate length data;
  ArraySegment.fold_left f accu data 0 length

let (* public *) fold_right f v accu =
  let { length; data; _ } = v in validate length data;
  ArraySegment.fold_right f data 0 length accu

let (* public *) to_list v =
  let { length; data; _ } = v in validate length data;
  ArraySegment.to_list data 0 length

let (* public *) to_seq v =
  let { length; data; _ } = v in validate length data;
  ArraySegment.to_seq data 0 length

let (* public *) to_seq_rev v =
  let { length; data; _ } = v in validate length data;
  ArraySegment.to_seq_rev data 0 length

let (* public *) exists f v =
  let { length; data; _ } = v in validate length data;
  ArraySegment.exists f data 0 length

let (* public *) for_all f v =
  let { length; data; _ } = v in validate length data;
  ArraySegment.for_all f data 0 length

let (* public *) filter f v =
  let { length; data; _ } = v in validate length data;
  let v' = create() in
  for i = 0 to length - 1 do
    let x = A.unsafe_get data i (* safe *) in
    if f x then push v' x
  done;
  v'

let (* public *) filter_map f v =
  let { length; data; _ } = v in validate length data;
  let v' = create() in
  for i = 0 to length - 1 do
    let x = A.unsafe_get data i (* safe *) in
    match f x with Some y -> push v' y | None -> ()
  done;
  v'

let (* public *) equal equal v1 v2 =
  let { length = length1; data = data1; _ } = v1 in validate length1 data1;
  let { length = length2; data = data2; _ } = v2 in validate length1 data1;
  ArraySegment.equal equal data1 0 length1 data2 0 length2

let (* public *) compare compare v1 v2 =
  let { length = length1; data = data1; _ } = v1 in validate length1 data1;
  let { length = length2; data = data2; _ } = v2 in validate length2 data2;
  ArraySegment.compare compare data1 0 length1 data2 0 length2

let (* public *) find f v =
  let { length; data; _ } = v in validate length data;
  ArraySegment.find f data 0 length

let (* public *) show show v =
  let opening, separator, closing = "[|", "; ", "|]" in
  let b = Buffer.create 32 in
  Buffer.add_string b opening;
  let first = ref true in
  iter (fun x ->
    if not !first then Buffer.add_string b separator;
    Buffer.add_string b (show x);
    first := false
  ) v;
  Buffer.add_string b closing;
  Buffer.contents b

(* -------------------------------------------------------------------------- *)

(* Wrap the public functions that need defensive checks. *)

let (* public *) init n f =
  if defensive && n < 0 then length_failure n;
  init n f

let (* public *) make n x =
  if defensive && n < 0 then length_failure n;
  make n x

let[@inline] (* public *) get v i =
  if defensive && not (0 <= i && i < v.length) then index_failure v i;
  unsafe_get v i

let[@inline] (* public *) set v i x =
  if defensive && not (0 <= i && i < v.length) then index_failure v i;
  unsafe_set v i x

let (* public *) truncate v n =
  if defensive && n < 0 then length_failure n;
  truncate v n

let (* public *) set_capacity v new_capacity =
  if defensive && new_capacity < 0 then capacity_failure new_capacity;
  set_capacity v new_capacity

let (* public *) ensure_capacity v request =
  if defensive && request < 0 then capacity_failure request;
  ensure_capacity v request

let (* public *) ensure_extra_capacity v delta =
  if defensive && delta < 0 then capacity_failure delta;
  ensure_extra_capacity v delta

let[@inline] (* public *) push_array_segment v a ofs len =
  if defensive then validate_segment (A.length a) ofs len;
  unsafe_push_array_segment v a ofs len

let (* public *) append_array_segment =
  push_array_segment

(* -------------------------------------------------------------------------- *)

(* An emulation of the [Stack] API. *)

module Stack = struct

  type 'a t = 'a vector

  (* Our functions [pop] and [top] raise [Not_found] if the vector is empty. *)
  exception Empty = Not_found

  let create = create
  let[@inline] push x v = push v x (* reversed arguments *)
  let pop = pop
  let pop_opt = pop_opt

  (* Our [drop] does nothing if the vector is empty. *)
  let drop v = if is_empty v then raise Empty else drop v

  let top = top
  let top_opt = top_opt
  let clear = clear
  let copy = copy
  let is_empty = is_empty
  let length = length

  (* The stack API offers iteration from top to bottom. *)
  let iter = iter_down

  (* [Stack.fold] has the type of a [fold_left] function,
     but iterates from top to bottom. *)
  let fold f accu v =
    let { length; data; _ } = v in validate length data;
    let accu = ref accu in
    for i = length - 1 downto 0 do
      accu := f !accu (A.unsafe_get data i)
    done;
    !accu

  (* [Stack.to_seq] takes a snapshot (at no cost) of the stack when
     it is called, so the stack can be modified, without affecting
     iteration. We simulate this (at a cost) by making a copy. *)
  let to_seq v = to_seq_rev (copy v)
  let add_seq = push_seq
  let of_seq = of_seq

end

(* -------------------------------------------------------------------------- *)

(* An emulation of the [Array] API. *)

(* Array segments provided by the user are validated with respect to
   the logical length of the vector, not with respect to the length of
   the vector's data array. *)

let (* public *) concat vs =
  let v = create() in
  let n = List.fold_left (fun n a -> n + length a) 0 vs in
  set_capacity v n;
  List.iter (push_vector v) vs;
  assert (length v = n);
  v

let (* public *) sub v ofs len =
  let { length; data; _ } = v in validate length data;
  if defensive then validate_segment length ofs len;
  unsafe_steal_array (A.sub data ofs len)

let (* public *) fill v ofs len x =
  let { length; data; _ } = v in validate length data;
  if defensive then validate_segment length ofs len;
  A.fill data ofs len x

let (* public *) blit v1 ofs1 v2 ofs2 len =
  let { length = length1; data = data1; _ } = v1 in validate length1 data1;
  let { length = length2; data = data2; _ } = v2 in validate length2 data2;
  if defensive then validate_segment length1 ofs1 len;
  if defensive then validate_segment length2 ofs2 len;
  A.unsafe_blit data1 ofs1 data2 ofs2 len

let (* public *) stable_sort cmp v =
  let { length; data; _ } = v in validate length data;
  ArraySegment.unsafe_stable_sort cmp data 0 length

let (* public *) sort =
  stable_sort

let (* public *) fast_sort =
  stable_sort
