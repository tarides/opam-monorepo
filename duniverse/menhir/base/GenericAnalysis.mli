(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers a generic grammar analysis algorithm, based on a least
   fixed point computation. The grammar is described by four types and four
   functions. The desired analysis is described by just four functions and a
   constant. *)

open Fix

(**A abstract description of a grammar. *)
module type GRAMMAR = sig

  type nonterminal
  type terminal
  type symbol
  type production

  (**[classify symbol] determines whether the symbol [symbol] is a
     terminal symbol or a nonterminal symbol. *)
  val classify: symbol -> [`N of nonterminal | `T of terminal]

  (**[rhs prod] is the right-hand side of the production [prod]. *)
  val rhs: production -> symbol array

  (**[productions nt] is the list of the productions of
     the nonterminal symbol [nt]. *)
  val productions: nonterminal -> production list

  (**[ignore prod] indicates whether the production [prod] should be
     ignored during the analysis. *)
  val ignore: production -> bool

end

(**A description of an analysis. *)
module type ANALYSIS = sig

  type terminal
  type nonterminal
  type symbol
  type property

  (* An analysis is specified by the following functions. *)

  (**If [shortcut nt] is [Some p], then the definition of the nonterminal
     symbol [nt] is ignored by the analysis, and this symbol is mapped by
     the analysis to the property [p]. *)
  val shortcut: nonterminal -> property option

  (**[terminal] maps a terminal symbol to a property. *)
  val terminal: terminal -> property

  (**[disjunction] abstracts a binary alternative. That is, when we analyze
     an alternative between several productions, we compute a property for
     each of them independently, then we combine these properties using
     [disjunction]. *)
  val disjunction: property -> (unit -> property) -> property

  (**[conjunction] abstracts a binary sequence. That is, when we analyze a
     sequence, we compute a property for each member independently, then we
     combine these properties using [conjunction]. In general, conjunction
     needs access to the first member of the sequence (a symbol), not just
     to its analysis (a property).

     The property [P.bottom] must be a neutral element for [disjunction]. We
     use [P.bottom] in the analysis of an alternative with zero branches. *)
  val conjunction: symbol -> property -> (unit -> property) -> property

  (**[epsilon] abstracts the empty sequence. It must be a neutral element
     for [conjunction]. *)
  val epsilon: property

end

(**The result of applying an analysis to a grammar. *)
module type RESULT = sig

  type property
  type terminal
  type nonterminal
  type symbol
  type production

  (* The results of the analysis take the following form. *)

  (* The results are computed on demand, that is, when the functions
     listed below are invoked. *)

  (**To every nonterminal symbol, we associate a property. *)
  val nonterminal: nonterminal -> property

  (**To every symbol, we associate a property. *)
  val symbol: symbol -> property

  (**To every production, we associate a property. *)
  val production: production -> property

  (**To every suffix of every production, we associate a property.
     The offset [i], which determines the beginning of the suffix,
     must be contained between [0] and [n], inclusive, where [n]
     is the length of the production. *)
  val production_suffix: production -> int -> property

end

(* -------------------------------------------------------------------------- *)

(**[Make(M)(P)(G)(A)] applies the analysis [A] to the grammar [G]. *)
module Make
(M : IMPERATIVE_MAPS)
(P : PROPERTY)
(G : GRAMMAR with type nonterminal = M.key)
(A : ANALYSIS
 with type property := P.property
  and type terminal := G.terminal
  and type nonterminal := M.key
  and type symbol := G.symbol)
: RESULT
  with type property = P.property
   and type terminal = G.terminal
   and type nonterminal = G.nonterminal
   and type symbol = G.symbol
   and type production = G.production
