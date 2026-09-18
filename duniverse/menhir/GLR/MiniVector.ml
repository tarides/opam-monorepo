(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

module A = struct
  include Array
  let empty = [||]
  let grow n x a k =
    let b = Array.make n x in
    Array.blit a 0 b 0 k;
    b
end

(* -------------------------------------------------------------------------- *)

(* Types. *)

type length = int
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

let[@inline] max (x : int) (y : int) = if x >= y then x else y

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

(* -------------------------------------------------------------------------- *)

(* Access. *)

let[@inline] (* public *) length v =
  v.length

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

(* -------------------------------------------------------------------------- *)

(* Changing the vector's capacity and/or re-allocating the [data]
   array to match its capacity, are the most tricky aspects of this
   data structure. *)

(* One must keep in mind the invariant that if [v.length] is nonzero
   then [v.capacity] is the length of the [data] array. *)

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
