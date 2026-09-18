(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Sort

(**This module implements sort unification. *)

(**A unification variable. *)
type variable

(**The constant [star] represents the base sort. *)
val star: variable

(**[arrow xs] returns a variable that represents the sort [xs -> *]. *)
val arrow: variable list -> variable

(**[fresh()] returns a fresh unconstrained variable,
   which represents an as-yet-unknown sort. *)
val fresh: unit -> variable

(**[domain] is the opposite of [arrow]. If [x] has been unified with an arrow,
   then [domain x] returns its domain. Otherwise, it returns [None]. Use with
   caution; this function allows obtaining information that may not be stable
   (because unification has not finished yet). *)
val domain: variable -> variable list option

(**The exception [Unify (x, y)] is raised when an attempt to unify the
   variables [x] and [y] yields to a structural mismatch. *)
exception Unify of variable * variable

(**The exception [Occurs (x, y)] is raised when an attempt to unify the
   variables [x] and [y] yields to a cycle in the structure. *)
exception Occurs of variable * variable

(**[unify x y] attempts to unify the sorts represented by the variables [x]
   and [y]. The creation of cycles is not permitted; an eager occurs check
   rules them out. *)
val unify: variable -> variable -> unit

(**A value of type [sort] is a sort where some unification variables can
   still possibly occur. *)
type usort

(**Once unification is over, a unification variable can be decoded.
   This yields a sort. *)
val decode: variable -> usort

(**[ground sort] grounds the sort [sort]. The result is a ground sort,
   where all sort variables have been replaced with the base sort [*]. *)
val ground: usort -> sort

(**[unground] is an injection of the type [sort] into the type [usort].
   It produces a sort that does not contain any unification variables.
   The composition [unground] and [print] allows printing a ground sort. *)
val unground: sort -> usort

(**[print] converts a sort to a string. *)
val print: usort -> string
