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

(**This module offers a priority queue, represented as a leftist heap. *)

module Make (Key : sig
  type t
  val compare: t -> t -> int
end) : sig

  (**A key. *)
  type key =
    Key.t

  (**An immutable priority queue. *)
  type 'a heap

  type 'a t =
    'a heap

  (**[empty] is the empty queue. *)
  val empty : 'a t

  (**[singleton k v] is a singleton queue containing the key-value
     pair [(k, v)]. *)
  val singleton : key -> 'a -> 'a t

  (**[insert k v q] inserts the key-value pair [(k, v)] into the queue
     [q]. The result is a new queue. *)
  val insert : key -> 'a -> 'a t -> 'a t

  (**[merge q1 q2] merges the queues [q1] and [q2]. The result is a
     new queue. *)
  val merge : 'a t -> 'a t -> 'a t

  (**[pop q] extracts a key-value pair whose key is minimal out of the queue
     [q]. [None] is returned only in the case where [q] is empty. *)
  val pop: 'a t -> ((key * 'a) * 'a t) option

  type 'a pop2 =
    | Head of key * 'a * key * 'a * 'a t
    | Tail of key * 'a
    | Done

  (**[pop2 q] extracts two key-value pairs whose keys are minimal out of the
     queue [q]. [Tail] is returned only in the case where [q] has only one
     element; [Done] is returned only in the case where [q] is empty. *)
  val pop2 : 'a t -> 'a pop2

  (**/**)

  val check: 'a t -> unit

end
