(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This module offers an implementation of priority queues. It is
   parameterized over a totally ordered type of elements [E] and
   over an implementation [V] of vectors for this type of elements. *)
module Make
(E: sig
  type t
  val compare: t -> t -> int
end)
(_ : sig
  type 'a vector
  val create : unit -> 'a vector
  val length : 'a vector -> int
  val get : 'a vector -> int -> 'a
  val set : 'a vector -> int -> 'a -> unit
  val push : 'a vector -> 'a -> unit
  val pop : 'a vector -> 'a
end)
: sig

  (** The type of priority queues. *)
  type queue
  type t = queue

  (** The type of elements. *)
  type element = E.t

  (**[create()] returns a fresh empty priority queue.*)
  val create : unit -> queue

  (**[is_empty q] is equivalent to [length q = 0]. *)
  val is_empty : queue -> bool

  (**[add q x] inserts the element [x] into the queue [q]. *)
  val add : queue -> element -> unit

  exception Empty

  (** [extract q] extracts and returns an element of the queue [q] that has
      minimum priority. If the queue is empty, [None] is returned. *)
  val extract : queue -> element option

end
