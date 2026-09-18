(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module computes which symbols generate a word other than ε, that is,
   a word of length greater than 0. The same analysis can be obtained, at a
   greater cost, by computing FIRST sets: a symbol generates a word other
   than ε if and only if its FIRST set is nonempty. *)

open PlainSyntax
open GenericAnalysis

(* The signature of this module is more polymorphic than [ANALYSIS]. *)
module A : sig
  type property = bool
  val shortcut: _ -> property option
  val terminal: _ -> property
  val disjunction: property -> (unit -> property) -> property
  val conjunction: _ -> property -> (unit -> property) -> property
  val epsilon: property
end

module Make
(G : sig val grammar: grammar end)
: RESULT
  with type property = bool
   and type nonterminal = nonterminal
   and type terminal = terminal
   and type symbol = symbol
   and type production = branch
