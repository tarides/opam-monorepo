(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers an implementation of vectors. A vector is a mutable
   data structure, which stores a sequence of values. *)

(* -------------------------------------------------------------------------- *)

(* Types. *)

(**The type of a vector. *)
type 'a vector

(**A synonym for the type of a vector. *)
type 'a t = 'a vector

(**An integer value of type [length] represents the length of a sequence.
   For example, it can be the length of an array, the length of a vector,
   or the length of a segment of an array of vector. A length is nonnegative. *)
type length = int

(**An integer value of type [capacity] represents the capacity of a vector.
   A capacity is nonnegative. *)
type capacity = int

(**An integer value of type [index] represents an index into a sequence of
   elements.*)
type index = int

(* -------------------------------------------------------------------------- *)

(** {1:creating Creating} *)

(**[create()] creates a new vector of length 0 and capacity 0. *)
val create : unit -> 'a vector

(**[make n x] creates a new vector of length [n] and capacity [n]
   and initializes this vector by storing the value [x] everywhere.
   [n] must be a valid capacity. *)
val make : length -> 'a -> 'a vector

(**[init n f] creates a new vector of length [n] and capacity [n]
   and initializes it, from left to right, by computing and storing
   the value [f i] at index [i]. [n] must be a valid capacity. *)
val init : length -> (index -> 'a) -> 'a vector

(**[copy v] creates a new vector whose length and capacity are [length v]
   and initializes it with a copy of the data stored in the vector [v]. *)
val copy : 'a vector -> 'a vector

(**[sub v i n] produces a new vector whose elements are the elements of
   the vector segment determined by vector [v], index [i], and length [n].
   [i] and [n] must describe a valid segment of the vector [v]. *)
val sub : 'a vector -> index -> length -> 'a vector

(**[concat vs] produces a new vector whose sequence of elements is
   the concatenation of the sequences of elements of the vectors
   in the list [vs]. *)
val concat : 'a vector list -> 'a vector

(* -------------------------------------------------------------------------- *)

(** {1:access Reading and writing} *)

(**[get v i] fetches the element that lies at index [i] in the vector [v].
   [i] must be comprised in the semi-open interval [\[0, length v)]. *)
val get : 'a vector -> index -> 'a

(**[set v i x] overwrites the element that lies at index [i] in the vector
   [v] with the value [x]. [i] must be comprised in the semi-open interval
   [\[0, length v)]. *)
val set : 'a vector -> index -> 'a -> unit

(**If the vector [v] is nonempty, [top v] returns its last element.
   If the vector [v] is empty, [top v] raises [Not_found]. *)
val top : 'a vector -> 'a

(**[get_last] is a synonym for [top]. *)
val get_last : 'a vector -> 'a (* synonym *)

(**If the vector [v] is nonempty, [top_opt v] returns its last element.
   If the vector [v] is empty, [top_opt v] returns [None]. *)
val top_opt : 'a vector -> 'a option

(**[find_last] is a synonym for [top_opt]. *)
val find_last : 'a vector -> 'a option (* synonym *)

(**[fill v i n x] writes the value [x] into every slot of the vector
   segment determined by vector [v], index [i], and length [n].
   [i] and [n] must describe a valid segment of the vector [v]. *)
val fill : 'a vector -> index -> length -> 'a -> unit

(**[blit v i v' i' n] copies data from the vector segment determined by
   vector [v], index [i], and length [n] into the vector segment
   determined by vector [v'], index [i'], and length [n]. It works
   correctly even if the source and destination segments overlap.
   [i] and [n] must describe a valid segment of the vector [v].
   [i'] and [n] must describe a valid segment of the vector [v']. *)
val blit : 'a vector -> index -> 'a vector -> index -> length -> unit

(** {2:access_unsafe Unsafe access} *)

(**[unsafe_get v i] fetches the element that lies at index [i] in the vector
   [v]. [i] must be comprised in the semi-open interval [\[0, length v)]. {b
   No bounds check is performed.} If the index [i] is out of bounds, memory
   safety can be compromised. Use at your own risk! *)
val unsafe_get : 'a vector -> index -> 'a

(**[unsafe_set v i x] overwrites the element that lies at index [i]
   in the vector [v] with the value [x].
   [i] must be comprised in the semi-open interval [\[0, length v)].
   {b No bounds check is performed.} If the index [i] is out of bounds,
   memory safety can be compromised. Use at your own risk! *)
val unsafe_set : 'a vector -> index -> 'a -> unit

(**[unsafe_borrow v] returns the data array that is part of the internal
   representation of the vector [v]. The length of this data array is at
   least [length v], and can be greater than [length v]. Beyond this
   guarantee, the length of this data array is unspecified; it is not
   necessarily the capacity of the vector. {b As long as the vector [v] is
   not modified by other means,} the segment of the data array delimited by
   the semi-open interval [\[0, length v)] can be safely read and
   written. *)
val unsafe_borrow : 'a vector -> 'a array

(* -------------------------------------------------------------------------- *)

(** {1:pushing Pushing} *)

(**[push v x] extends the vector [v] with the element [x]. The length of the
   vector [v] is increased by one. If necessary, the capacity of the vector
   [v] is increased. *)
val push : 'a vector -> 'a -> unit

(**[add_last] is a synonym for [push]. *)
val add_last : 'a vector -> 'a -> unit (* synonym *)

(**[push_array v a] extends the vector [v] with the elements of the array [a].
   The length of the vector [v] is increased by the length of the array [a].
   If necessary, the capacity of the vector [v] is increased. *)
val push_array : 'a vector -> 'a array -> unit

(**[append_array] is a synonym for [push_array]. *)
val append_array : 'a vector -> 'a array -> unit (* synonym *)

(**[push_array_segment v a i n] extends the vector [v] with the elements
   of the array segment determined by array [a], index [i], and length
   [n]. The length of the vector [v] is increased by [n]. If necessary,
   the capacity of the vector [v] is increased. [i] and [n] must describe
   a valid segment of the array [a]. *)
val push_array_segment : 'a vector -> 'a array -> index -> length -> unit

(**[append_array_segment] is a synonym for [push_array_segment]. *)
val append_array_segment : 'a vector -> 'a array -> index -> length -> unit (* synonym *)

(**[push_vector v v'] extends the vector [v] with the elements of the vector
   [v']. The length of the vector [v] is increased by the length of the array
   [v']. If necessary, the capacity of the vector [v] is increased. *)
val push_vector : 'a vector -> 'a vector -> unit

(**[append] is a synonym for [push_vector]. *)
val append : 'a vector -> 'a vector -> unit (* synonym *)

(**[push_list v xs] extends the vector [v] with the elements of the list [xs].
   The length of the vector [v] is increased by the length of the list [xs].
   If necessary, the capacity of the vector [v] is increased. *)
val push_list : 'a vector -> 'a list -> unit

(**[append_list] is a synonym for [push_list]. *)
val append_list : 'a vector -> 'a list -> unit (* synonym *)

(**[push_seq v xs] extends the vector [v] with the elements of the sequence
   [xs]. The length of the vector [v] is increased by the length of the
   sequence [xs]. If necessary, the capacity of the vector [v] is increased.
   The sequence [xs] is demanded just once. *)
val push_seq : 'a vector -> 'a Seq.t -> unit

(**[append_seq] is a synonym for [push_seq]. *)
val append_seq : 'a vector -> 'a Seq.t -> unit (* synonym *)

(** [push_iter v iter c] pushes each element of the collection [c]
    in turn onto the vector [v]. The function [iter] is used to
    iterate over the elements of [c]. In other words,
    [push_iter v iter c] is equivalent to [iter (push v) c]. *)
val push_iter :
  'a vector ->
  (('a -> unit) -> 'c -> unit) ->
  'c -> unit

(**[append_iter] is a synonym for [push_iter]. *)
val append_iter : (* synonym *)
  'a vector ->
  (('a -> unit) -> 'c -> unit) ->
  'c -> unit

(* -------------------------------------------------------------------------- *)

(** {1:popping Popping} *)

(**If the vector [v] is nonempty, [pop v] removes and returns its last
   element. The length of the vector is decreased by one; its capacity is
   unchanged. If the vector [v] is empty, [pop v] raises [Not_found]. *)
val pop : 'a vector -> 'a

(**[pop_last] is a synonym for [pop]. *)
val pop_last : 'a vector -> 'a (* synonym *)

(**If the vector [v] is nonempty, [pop_opt v] removes and returns its last
   element. The length of the vector is decreased by one; its capacity is
   unchanged. If the vector [v] is empty, [pop_opt v] returns [None]. *)
val pop_opt : 'a vector -> 'a option

(**[pop_last_opt] is a synonym for [pop_opt]. *)
val pop_last_opt : 'a vector -> 'a option (* synonym *)

(**If the vector [v] is nonempty, [drop v] removes its last element.
   If the vector [v] is empty, [drop v] has no effect.
   [drop v] is equivalent to [if is_empty v then () else ignore (pop v)]. *)
val drop : 'a vector -> unit

(**[remove_last] is a synonym for [drop]. *)
val remove_last : 'a vector -> unit (* synonym *)

(* -------------------------------------------------------------------------- *)

(** {1:iterating Iterating} *)

(**While iteration is ongoing, the vector must not be modified. For example,
   in [iter f v], the function [f] is not allowed to modify the vector [v].
   This rule applies to all iteration functions. *)

(**[iter f v] applies the function [f] in turn, from left to right, to each
   element [x] of the vector [v]. *)
val iter : ('a -> unit) -> 'a vector -> unit

(**[iter_down f v] applies the function [f] in turn, from right to left, to
   each element [x] of the vector [v]. *)
val iter_down : ('a -> unit) -> 'a vector -> unit

(**[iteri f v] applies the function [f] in turn, from left to right, to each
   index [i] and corresponding element [x] of the vector [v]. *)
val iteri : (int -> 'a -> unit) -> 'a vector -> unit

(**[fold_left f s v] applies the function [f] in turn, from left to right, to
   each element [x] of the vector [v]. A state, whose initial value is [s], is
   threaded through this sequence of function invocations, and is eventually
   returned. *)
val fold_left : ('s -> 'a -> 's) -> 's -> 'a vector -> 's

(**[fold_right f v s] applies the function [f] in turn, from right to left, to
   each element [x] of the vector [v]. A state, whose initial value is [s], is
   threaded through this sequence of function invocations, and is eventually
   returned. *)
val fold_right : ('a -> 's -> 's) -> 'a vector -> 's -> 's

(* -------------------------------------------------------------------------- *)

(** {1:transforming Transforming} *)

(**While transformation is ongoing, the vector must not be modified. For
   example, in [map f v], the function [f] is not allowed to modify the
   vector [v]. This rule applies to all transformation functions. *)

(**[map f v] applies the function [f] in turn, from left to right, to each
   element [x] of the vector [v], and constructs a new vector of the results
   of these calls. *)
val map : ('a -> 'b) -> 'a vector -> 'b vector

(**[mapi f v] applies the function [f] in turn, from left to right, to each
   index [i] and corresponding element [x] of the vector [v], and constructs a
   new vector of the results of these calls. *)
val mapi : (index -> 'a -> 'b) -> 'a vector -> 'b vector

(**[filter f v] applies the function [f] in turn, from left to right, to each
   element [x] of the vector [v], and constructs a new vector containing just
   the elements [x] such that [f x] returned [true]. *)
val filter : ('a -> bool) -> 'a vector -> 'a vector

(**[filter_map f v] applies the function [f] in turn, from left to right, to
   each element [x] of the vector [v], and constructs a new vector containing
   just the values [y] such that [f x] returned [Some y]. *)
val filter_map : ('a -> 'b option) -> 'a vector -> 'b vector

(* -------------------------------------------------------------------------- *)

(** {1:searching Searching} *)

(**While searching is in progress, the vector must not be modified. For
   example, in [exists f v], the function [f] is not allowed to modify the
   vector [v]. This rule applies to all search functions. *)

(**[exists f v] determines whether at least one element [x] of the vector [v]
   satisfies the predicate [f]. The vector is scanned from left to right. *)
val exists : ('a -> bool) -> 'a vector -> bool

(**[for_all f v] determines whether all elements [x] of the vector [v] satisfy
   the predicate [f]. The vector is scanned from left to right. *)
val for_all : ('a -> bool) -> 'a vector -> bool

(**[find f v] finds the leftmost element [x] of the vector [v] such that
   [f x] is true, and returns its index. If no such element exists, then
   [find f v] raises [Not_found]. *)
val find : ('a -> bool) -> 'a vector -> int

(* -------------------------------------------------------------------------- *)

(** {1:comparing Comparing and sorting} *)

(**While comparison is in progress, the vector must not be modified. For
   example, in [equal eq v v'], the function [eq] is not allowed to modify
   the vector [v]. This rule applies to all comparison functions. *)

(**Provided [eq] is an equality on elements, [equal eq] is the pointwise
   equality of vectors. In other words, [equal eq v v'] determines whether
   the sequences of elements of the vectors [v] and [v'] are pointwise
   equal, using the function [eq] to test whether two elements are equal. *)
val equal : ('a -> 'a -> bool) -> 'a vector -> 'a vector -> bool

(**Provided [cmp] is a preorder on elements, [compare cmp] is the
   lexicographic preorder on vectors. In other words, [compare cmp v v']
   compares the sequences of elements of the vectors [v] and [v'], according
   to the lexicographic preorder, and using the function [cmp] to compare
   two elements.

   {b Caution:} [compare] behaves like [List.compare],
   not like [Dynarray.compare].
   [Dynarray.compare] implements a preorder on vectors
   that is not is the lexicographic preorder. *)
val compare : ('a -> 'a -> int) -> 'a vector -> 'a vector -> int

(**[sort cmp v] sorts the vector [v], in place,
   according to the preorder [cmp].

   [sort] is currently a synonym for {!stable_sort}. *)
val sort : ('a -> 'a -> int) -> 'a vector -> unit

(**[stable_sort cmp v] sorts the vector [v], in place,
   according to the preorder [cmp]. This is a stable sort:
   if two elements are equivalent according to [cmp] then
   their relative order in the sequence is preserved.
   This is a merge sort algorithm; it is the same algorithm
   as in [Array.stable_sort]. *)
val stable_sort : ('a -> 'a -> int) -> 'a vector -> unit

(**[fast_sort cmp v] sorts the vector [v], in place,
   according to the preorder [cmp].

   [fast_sort] is currently a synonym for {!stable_sort}. *)
val fast_sort : ('a -> 'a -> int) -> 'a vector -> unit

(* -------------------------------------------------------------------------- *)

(** {1:converting Converting} *)

(**[of_array a] returns a new vector whose elements are the elements of
   the array [a]. The length and capacity of the new vector are the length
   of the array [a]. *)
val of_array : 'a array -> 'a vector

(**[of_list xs] returns a new vector whose elements are the elements of
   the list [xs]. The length and capacity of the new vector are the length
   of the list [xs]. *)
val of_list : 'a list -> 'a vector

(**[of_seq xs] returns a new vector whose elements are the elements of the
   sequence [xs]. The length and capacity of the new vector are the length
   of the sequence [xs]. *)
val of_seq : 'a Seq.t -> 'a vector

(**[to_array v] creates a new array whose elements are the elements of the
   vector [v]. The length of the new array is the length of the vector [v]. *)
val to_array : 'a vector -> 'a array

(**[to_list v] creates a new list whose elements are the elements of the
   vector [v]. The length of the new list is the length of the vector [v]. *)
val to_list : 'a vector -> 'a list

(**[to_seq v] creates a sequence whose elements are the elements of the
   vector [v]. The length of this sequence is the length of the vector
   [v]. This sequence can be demanded as many times as one wishes.
   However, this sequence is valid only as long as the vector [v] is not
   modified. As soon as [v] is modified, this sequence must no longer be
   used. *)
val to_seq : 'a vector -> 'a Seq.t

(**[to_seq_rev v] creates a sequence whose elements are the elements of
   the vector [v], in reverse order. The length of this sequence is the
   length of the vector [v]. This sequence can be demanded as many times
   as one wishes. However, this sequence is valid only as long as the
   vector [v] is not modified. As soon as [v] is modified, this sequence
   must no longer be used. *)
val to_seq_rev : 'a vector -> 'a Seq.t

(* -------------------------------------------------------------------------- *)

(** {1:showing Showing} *)

(**[show f v] returns a textual representation of the contents of the
   vector [v]. The user-supplied function [f] is used to obtain a textual
   representation of each element. *)
val show : ('a -> string) -> 'a vector -> string

(**/**)
(**[check] is used only during testing. *)
val check : 'a vector -> unit
(**/**)

(* -------------------------------------------------------------------------- *)

(** {1:length Length} *)

(**[length v] is the (logical) length of the vector [v]. *)
val length : 'a vector -> length

(**[s_empty v] is equivalent to [length v = 0]. *)
val is_empty : 'a vector -> bool

(**If [n] is less than [length v], then [truncate v n] sets the length of the
   vector [v] to [n]. Otherwise, nothing happens. In either case, the capacity
   of the vector [v] is unchanged. This is a constant-time operation. *)
val truncate : 'a vector -> length -> unit

(**[clear v] is equivalent to [truncate v 0]. The length of the vector [v]
   becomes zero. Its capacity is unchanged. *)
val clear : 'a vector -> unit

(* -------------------------------------------------------------------------- *)

(** {1:capacity Capacity} *)

(**[reset v] sets the length and the capacity of the vector [v] to zero. *)
val reset : 'a vector -> unit

(**[ensure_capacity v c] ensures that the capacity of the vector [v] is at
   least [c]. If necessary, the capacity of the vector [v] is increased. *)
val ensure_capacity : 'a vector -> capacity -> unit

(**[ensure_extra_capacity v delta] ensures that the capacity of the vector
   [v] is at least [length v + delta]. If necessary, the capacity of the
   vector [v] is increased. The increment [delta] must be nonnegative. *)
val ensure_extra_capacity : 'a vector -> capacity -> unit

(**[fit_capacity v] ensures that the capacity of the vector [v] matches its
   length. If necessary, the capacity of the vector [v] is decreased. *)
val fit_capacity : 'a vector -> unit

(**[set_capacity v c] ensures that the capacity of the vector [v] is exactly
   [c]. If [c] is less than [length v], then the vector [v] is truncated:
   some elements are lost. Otherwise, the elements of the vector [v] are
   preserved, and its capacity is decreased or increased as necessary. *)
val set_capacity : 'a vector -> capacity -> unit

(* -------------------------------------------------------------------------- *)

(** {1:stack The Stack API} *)

(**This module offers the same API as the standard library module
   [Stdlib.Stack], but is implemented using vectors. *)
module Stack : sig
  type 'a t = 'a vector
  exception Empty
  val create : unit -> 'a t
  val push : 'a -> 'a t -> unit
  val pop : 'a t -> 'a
  val pop_opt : 'a t -> 'a option
  val drop : 'a t -> unit
  val top : 'a t -> 'a
  val top_opt : 'a t -> 'a option
  val clear : 'a t -> unit
  val copy : 'a t -> 'a t
  val is_empty : 'a t -> bool
  val length : 'a t -> int
  val iter : ('a -> unit) -> 'a t -> unit
  val fold : ('s -> 'a -> 's) -> 's -> 'a t -> 's
  val to_seq : 'a t -> 'a Seq.t
  val add_seq : 'a t -> 'a Seq.t -> unit
  val of_seq : 'a Seq.t -> 'a t
end
