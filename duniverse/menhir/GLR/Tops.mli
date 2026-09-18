(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This module implements the set of top nodes of the GSS. *)

module Make (T : sig

  (**We exploit the fact that LR(1) states are integers. *)
  type state = private int

  (**The type of semantic values is arbitrary. *)
  type semv

end) : sig
  open T
  type node = (state, semv) GSS.node

  type t

  (**[create n] creates a fresh empty set whose date is zero. The parameter
     [n] is the number of states of the LR(1) automaton. *)
  val create : int -> t

  (**[register tops node] inserts the node [node] into the set [tops]. *)
  val register : t -> node -> unit

  (**[find tops state] determines whether a node whose state is [state] exists
     in the set [tops]. *)
  val find : t -> state -> node option

  (**[iter tops] enumerates the nodes in the set [tops]. *)
  val iter : t -> (node -> unit) -> unit

  (**[for_all tops p] determines whether all nodes in the set [tops]
     satisfy the property [p]. *)
  val for_all : t -> (node -> bool) -> bool

  (**[iter_prev tops] enumerates the nodes in the previous generation, that
     is, the nodes that were in the set [tops] when [bump] was last called. *)
  val iter_prev : t -> (node -> unit) -> unit

  (**[cardinal tops] returns the number of nodes in the set [tops]. *)
  val cardinal : t -> int

  (**Assuming that the set [tops] has cardinal 1, [extract_singleton tops]
     returns its unique inhabitant. The set [tops] becomes empty. *)
  val extract_singleton : t -> node

  (**[elements_prev tops] returns a list of the nodes in the previous
     generation, that is, the nodes that were in the set [tops] when [bump]
     was last called. *)
  val elements_prev : t -> node list

  (**[date tops] returns the current date of the set [tops]. *)
  val date : t -> int

  (**[bump tops] increments the current date, [date tops], by one. The set
     [tops] becomes empty and ready to accept nodes at the new date. The
     previous elements of the set can still be accessed, until [bump] is
     called again, via [iter_prev] and [elements_prev]. *)
  val bump : t -> unit

  (**[advance tops date] sets the current date, [date tops], to [date]. The
     set [tops] must be empty. The date [date] must be greater than or equal
     to [date tops]. *)
  val advance : t -> int -> unit

  (**[check tops] whether [tops] seems valid. It is used during debugging. *)
  val check : t -> unit

  (**[present tops node] tests whether the node [node] is present in the set
     [tops]. It is used for debugging only. *)
  val present : t -> node -> bool

end
