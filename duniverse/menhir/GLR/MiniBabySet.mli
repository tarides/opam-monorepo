(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This module offers a stripped-down weight-balanced binary tree library. *)

(* We expose the parameterized type ['a tree] so as to avoid imposing the use
   of functors and recursive modules on the client. The ordering of elements
   is not fixed at functor application time; instead, an ordering must be
   passed to [add], [mem], and [find]. *)

(* The functions [add_absent] and [find] do not exist in the libraries
   Stdlib.Set and Baby. *)

(**A set. *)
type 'a tree

(**The empty set. *)
val empty: 'a tree

(**Insertion of an element into a set. *)
val add: ('a -> 'a -> int) -> 'a -> 'a tree -> 'a tree

(**Insertion of an element that is known to be absent. *)
val add_absent: ('a -> 'a -> int) -> 'a -> 'a tree -> 'a tree

(**A singleton set. *)
val singleton: 'a -> 'a tree

(**A membership test. *)
val mem: ('a -> 'a -> int) -> 'a -> 'a tree -> bool

(**A heterogeneous membership test. *)
val find: ('a -> 'b -> int) -> 'a -> 'b tree -> 'b option

(**Iteration. *)
val iter: ('a -> unit) -> 'a tree -> unit

(**[for_all p s] determines whether all elements of the set [s]
   satisfy the property [p]. *)
val for_all: ('a -> bool) -> 'a tree -> bool

(**[is_singleton s] tests whether the set [s] is a singleton set. *)
val is_singleton: 'a tree -> bool

(**[extract_singleton s] extracts the unique element of the set [s].
   The set [s] must be a singleton. *)
val extract_singleton: 'a tree -> 'a

(**[cardinal s] returns the cardinal of the set [s]. *)
val cardinal: 'a tree -> int

(**A well-formedness check. (Checks balance, not BST ordering.) *)
val check: 'a tree -> unit
