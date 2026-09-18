(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module constructs a rich view of the grammar,
   which obeys the signature [GRAMMAR]. *)

open PlainSyntax
open MiddleAPI

(**The functor [Make] transforms a grammar in [PlainSyntax] format
   into a rich internal view. *)
module Make (G : sig
  val grammar: grammar
end)
(X : sig
  (**If [topological_numbering] is [false] then the productions are numbered
     in an arbitrary order. If it is [true] and if the grammar is acyclic then
     they are numbered so as to satisfy the following property: if [A → α]
     receives a smaller number than [B → β] then A => B is possible but B => A
     is not. This property is exploited by the GLR parsing algorithm. *)
  val topological_numbering: bool
end)
() : GRAMMAR
