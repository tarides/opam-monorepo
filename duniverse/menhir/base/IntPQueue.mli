(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

module Plain : sig
(**This is a priority queue whose keys are {i low} nonnegative integers. *)

(**This queue is optimized for throughput, that is, speed. When an element
   is extracted out of the queue, the queue may retain a pointer to this
   element. This creates a memory leak: that is, the existence of this
   pointer can prevent the garbage collector from freeing this element. In
   practice, we do not expect this to be a problem, especially in scenarios
   where the queue itself is not long-lived. *)

(**A priority is a nonnegative integer. *)
type priority =
  int

(**A priority queue is an abstract mutable data structure. It can be thought
   of as a bag of elements, where each element carries a certain priority. *)
type 'a t

(**[create()] creates an empty priority queue.

   Time complexity: {m O(1)}. *)
val create: unit -> 'a t

(**[add q x i] inserts the element [x] with priority [i] into the queue [q].

   Time complexity: {m O(1)} (amortized). *)
val add: 'a t -> 'a -> priority -> unit

(**[extract q] extracts an element out of the queue [q] and returns it. This
   element has minimum priority among all of the elements that are currently
   present in the queue. If the queue is empty, [None] is returned.

  Time complexity: {m O(p)}. If the queue is used in a {i monotonic} manner
  (that is, if the priority that is used in every call to {!add} is at least
  as high as the priority of the last element that was returned by
  {!extract}) then the time complexity of {m n} calls to {!extract} is only
  {m O(n+p)}. Indeed, in this monotonic scenario, the cost of scanning the
  queue's main array, so as to find the next element with minimum priority,
  is shared between all invocations of {!extract}. *)
val extract: 'a t -> 'a option

(**[extract' q] extracts an element out of the queue [q] and returns a pair
   of this element and its priority. This element has minimum priority among
   all of the elements that are currently present in the queue. If the queue
   is empty, [None] is returned.

  Time complexity: see {!extract}. *)
val extract': 'a t -> ('a * priority) option

(**[is_empty q] tests whether the queue [q] is empty.

   Time complexity: {m O(1)}. *)
val is_empty: 'a t -> bool

(**[cardinal q] returns the number of elements in the queue [q].

   Time complexity: {m O(1)}. *)
val cardinal: 'a t -> int

(**[repeat q yield] repeatedly extracts an element with minimum priority out
   of [q] and passes it to [yield] (which may insert new elements into [q]),
   until [q] is exhausted.

   Time complexity: the total cost of {m n} calls to {!extract} and {m n}
   invocations of the function [yield]. *)
val repeat: 'a t -> ('a -> unit) -> unit

(**[iter q yield] enumerates the elements of the queue [q], by increasing
   order of priority, by passing them to the function [yield]. This function
   must not modify the queue [q]. *)
val iter: 'a t -> ('a -> unit) -> unit

(**[reset q] empties the queue [q], freeing up the space that the queue
   occupies in memory. The queue [q] becomes identical to a queue that
   has just been created by {!create}.

   Time complexity: {m O(1)}. *)
val reset: 'a t -> unit

(**/**)

(**[check] is used only during testing. *)
val check : 'a t -> unit
end
module Boxed : sig
(**This is a priority queue whose keys are {i low} nonnegative integers.
   It supports removing or updating the priority of a specific element of
   the queue. *)

(**This queue is optimized for throughput, that is, speed. When an element
   is extracted out of the queue, the queue may retain a pointer to this
   element. This creates a memory leak: that is, the existence of this
   pointer can prevent the garbage collector from freeing this element. In
   practice, we do not expect this to be a problem, especially in scenarios
   where the queue itself is not long-lived. *)

(**A priority is a nonnegative integer. *)
type priority =
  int

(**A box carries a payload, that is, a value of type ['a]. This payload
   cannot be modified. Furthermore, a box has a priority, a nonnegative
   integer value. The priority of a box is modified by certain operations,
   such as {!add} and {!update}. At any point in time, a box is either
   isolated or a member of a priority queue. *)
type 'a box

(**A priority queue is an abstract data structure. It can be thought of as a
   set of boxes, each of which carries a payload and a priority. *)
type 'a t

(**[box x] creates a new box whose payload is [x] and whose priority is
   unspecified and irrelevant. This box is isolated.

   Time complexity: {m O(1)}. *)
val box: 'a -> 'a box

(**[payload box] returns the payload of the box [box].

   Time complexity: {m O(1)}. *)
val payload: 'a box -> 'a

(**[priority box] returns the current priority of the box [box]. If this box
   is currently a member of a queue [q], then this is the priority of this
   box in the queue [q]. If this box is currently isolated, then this is the
   box's last known priority, as set by {!add} or {!update}.

   Time complexity: {m O(1)}. *)
val priority: 'a box -> priority

(**[busy box] determines whether the box [box] is currently a member of
   some priority queue. In other words, a box is busy iff it is {i not}
   isolated.

   Time complexity: {m O(1)}. *)
val busy: 'a box -> bool

(**[mem q box] determines whether the box [box] is currently a member of
   the priority queue [q].

   Time complexity: {m O(1)}. *)
val mem: 'a t -> 'a box -> bool

(**[create()] creates an empty priority queue.

   Time complexity: {m O(1)}. *)
val create: unit -> 'a t

(**[add q box i] sets the priority of the box [box] to [i] and inserts this
   box into the queue [q]. This box must be isolated, that is, not already a
   member of a priority queue.

   Time complexity: {m O(1)} (amortized). *)
val add: 'a t -> 'a box -> priority -> unit

(**[extract q] extracts a box out of the queue [q] and returns it. This box
   has minimum priority among all of the boxes that are currently present in
   the queue. If the queue is empty, [None] is returned.

  Time complexity: {m O(p)}. If the queue is used in a {i monotonic} manner
  (that is, if the priority that is used in every call to {!add} is at least
  as high as the priority of the last box that was returned by {!extract})
  then the time complexity of {m n} calls to {!extract} is only {m O(n+p)}.
  Indeed, in this monotonic scenario, the cost of scanning the queue's main
  array, so as to find the next box with minimum priority, is shared between
  all invocations of {!extract}. *)
val extract: 'a t -> 'a box option

(**[remove q box] extracts the box [box] out of the priority queue [q].
   This box must be a member of the queue [q]. It becomes isolated.

   Time complexity: {m O(1)}. *)
val remove: 'a t -> 'a box -> unit

(**[update q box i] sets the priority of the box [box] to [i]. This box
   must be a member of the queue [q], and remains a member of this queue.

   Provided its precondition is respected,
   [update box i] is equivalent to [remove q box; add q box i].

   If its precondition is violated, then [update q box i] cannot be expected
   to fail: in some cases, it can appear to silently succeed. To avoid this
   problem, it is recommended to write [assert (mem q box); update q box i].

   Time complexity: {m O(1)}. *)
val update: 'a t -> 'a box -> priority -> unit

(**[add_or_update q box i] inserts the box [box] with priority [i] into the
   queue [q], if this box was isolated, or sets the priority of this box to
   [i], if this box was already a member of the queue [q]. This box must not
   be a member of some queue other than [q].

   Provided its precondition is respected,
   [add_or_update q box i] is equivalent to
   [if mem box q then update q box i else add q box i].

   If its precondition is violated, then [add_or_update q box i] cannot be
   expected to fail: in some cases, it can appear to silently succeed. To
   avoid this problem, it is recommended to write
   [assert (not (busy box) || mem q box); add_or_update q box i].

   Time complexity: {m O(1)}. *)
val add_or_update: 'a t -> 'a box -> priority -> unit

(**[is_empty q] tests whether the queue [q] is empty.

   Time complexity: {m O(1)}. *)
val is_empty: 'a t -> bool

(**[cardinal q] returns the number of boxes in the queue [q].

   Time complexity: {m O(1)}. *)
val cardinal: 'a t -> int

(**[repeat q yield] repeatedly extracts an element with minimum priority out
   of [q] and passes it to [yield] (which may insert new elements into [q]),
   until [q] is exhausted.

   Time complexity: the total cost of {m n} calls to {!extract} and {m n}
   invocations of the function [yield]. *)
val repeat: 'a t -> ('a box -> unit) -> unit

(**[iter q yield] enumerates the boxes in the queue [q], by increasing order
   of priority, by passing them to the function [yield]. This function must
   not modify the queue [q]. *)
val iter: 'a t -> ('a box -> unit) -> unit

(**[reset q] empties the queue [q]. The queue [q] becomes identical to a
   queue that has just been created by {!create}. Every box that was a
   member of the queue [q] becomes isolated.

   A typical situation where [reset q] is useful (and cheap) is one where
   the queue [q] has been used for a while, is now empty, and will be used
   again in the future. Calling [reset q] at this point frees up the space
   occupied by the queue in memory and destroys any pointers from the queue
   to elements that have been stored in the queue earlier.

   Time complexity: {m O(n)}, where {m n} is the number of boxes currently
   in the queue. *)
val reset: 'a t -> unit

(**/**)

(**[check] is used only during testing. *)
val check: 'a t -> unit
end
