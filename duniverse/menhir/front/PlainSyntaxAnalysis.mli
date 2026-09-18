(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open PlainSyntax
open GenericAnalysis

module Make
(P : Fix.PROPERTY)
(G : sig val grammar: grammar end)
(A : ANALYSIS
  with type property := P.property
  and type terminal := terminal
  and type nonterminal := nonterminal
  and type symbol := symbol)
: RESULT
  with type property = P.property
   and type terminal = terminal
   and type nonterminal = nonterminal
   and type symbol = symbol
   and type production = branch
