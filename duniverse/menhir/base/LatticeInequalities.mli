(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers a solver of inequalities of the form [p ≤ y] or [x ≤ y]
   over a lattice of properties. It offers a thin layer of comfort on top of
   [Fix.DataFlow]. *)

open Fix

(**When [Make] is applied, a constraint collection phase begins. *)
module Make (M : IMPERATIVE_MAPS) (P : MINIMAL_SEMI_LATTICE) () : sig

  type variable =
    M.key

  type property =
    P.property

  (**[source p y] records an inequality [p ≤ y] between the
     constant [p] and the variable [y]. *)
  val source: property -> variable -> unit

  (**[edge x y] records an inequality [x ≤ y] between the
     variable [x] and the variable [y]. *)
  val edge: variable -> variable -> unit

  (**When [Solve] is applied, the constraint collection phase ends, and
     the least solution of the constraints is computed. The function
     [solution] has type [variable -> property option]. The value [None]
     represents bottom. *)
  module Solve () :
    SOLUTION
    with type variable = variable
     and type property = property option

end
