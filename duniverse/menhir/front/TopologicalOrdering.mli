(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module computes a topological ordering of the nonterminal symbols
   with respect to the strict partial order A => B. This is possible if
   and only if the grammar is acyclic. *)

open PlainSyntax

module Make (X : sig val grammar: grammar end) : sig

  (**If the grammar is acyclic then [nonterminals] is a list of the nonterminal
     symbols of the grammar [grammar] that respects the relation A => B. That
     is, if A appears before B in the list then A => B is permitted but B => A
     is impossible. If the grammar is cyclic then [nonterminals] is [None]. *)
  val nonterminals: nonterminal list option

end
