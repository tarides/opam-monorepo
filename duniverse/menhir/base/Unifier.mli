(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module provides a simple-minded implementation of first-order
   unification over an arbitrary term signature. *)

(* -------------------------------------------------------------------------- *)

(**The term signature is described by the user as follows. *)
module type STRUCTURE = sig

  (**The type ['a structure] should be understood as a type of shallow terms
     whose leaves have type ['a]. *)
  type 'a structure

  (**[map f s] applies the transformation [f] to every leaf of the shallow
     term [s]. *)
  val map: ('a -> 'b) -> 'a structure -> 'b structure

  (**[iter f s] applies the action [f] to every leaf of the shallow term [s]. *)
  val iter: ('a -> unit) -> 'a structure -> unit

  exception Iter2

  (**If the shallow terms [s1] and [s2] have the same constructor at their
     roots then [iter2 f s1 s2] applies the action [f] to every pair of
     matching leaves of the shallow terms [s1] and [s2]. Otherwise, it raises
     [Iter2]. *)
  val iter2: ('a -> 'b -> unit) -> 'a structure -> 'b structure -> unit

end

(* -------------------------------------------------------------------------- *)

(**[Make(S)] constructs a unifier for the term signature [S]. *)
module Make (S : STRUCTURE) : sig

  (**A unification variable. *)
  type variable

  (**[fresh s] creates a fresh variable that carries the structure [s]. *)
  val fresh: variable S.structure option -> variable

  (**[structure x] returns the structure that is currently carried by the
     variable [x]. *)
  val structure: variable -> variable S.structure option

  (**The exception [Unify (x, y)] is raised when an attempt to unify the
     variables [x] and [y] yields to a structural mismatch. *)
  exception Unify of variable * variable

  (**The exception [Occurs (x, y)] is raised when an attempt to unify the
     variables [x] and [y] yields to a cycle in the structure. *)
  exception Occurs of variable * variable

  (**[unify x y] attempts to unify the terms represented by the variables [x]
     and [y]. The creation of cycles is not permitted; an eager occurs check
     rules them out. *)
  val unify: variable -> variable -> unit

  (**This is the type of deep terms over the signature [S]. *)
  type term =
    | TVar of int (* the variable's unique identity *)
    | TNode of term S.structure

  (**[decode x] turns the variable [x] into the deep term that it represents.
     Sharing is lost, so this operation can in the worst case have exponential
     cost. *)
  val decode: variable -> term

end
