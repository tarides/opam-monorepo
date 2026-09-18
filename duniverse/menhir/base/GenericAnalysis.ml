(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

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

  type terminal
  type nonterminal
  type symbol
  type production
  type property

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

(* We perform memoization only at nonterminal symbols. We assume that the
   analysis of a symbol is the analysis of its definition (as opposed to,
   say, a computation that depends on the occurrences of this symbol in
   the grammar). Therefore, for example, the computation of FOLLOW sets
   is not an instance of this analysis.  *)

open Fix

module Make
(M : IMPERATIVE_MAPS)
(P : PROPERTY)
(G : GRAMMAR with type nonterminal = M.key)
(A : ANALYSIS
 with type property := P.property
  and type terminal := G.terminal
  and type nonterminal := M.key
  and type symbol := G.symbol)
= struct

  include P
  include G

  (* The following analysis functions are parameterized by [get], which
     makes a recursive call to the analysis at a nonterminal symbol. *)

  (* Analysis of a symbol. *)

  let symbol (get : G.nonterminal -> property)  sym : property =
    match G.classify sym with
    | `T t ->
        A.terminal t
    | `N nt ->
        (* Recursive call to the analysis, via [get]. *)
        get nt

  (* Analysis of (a suffix of) a production [prod], starting at index [i]. *)

  (* Because [A.conjunction] is lazy in its second argument, it is possible
     to stop early. *)

  let rec production_suffix get rhs n i : property =
    if i = n then
      A.epsilon
    else
      let sym = rhs.(i) in
      A.conjunction sym
        (symbol get sym)
        (fun () -> production_suffix get rhs n (i+1))

  (* Analysis of a production [prod], starting at index 0. *)

  let production get prod : property =
    assert (not (G.ignore prod));
    let rhs = G.rhs prod in
    let n = Array.length rhs in
    production_suffix get rhs n 0

  (* Analysis of a disjunction of productions. *)

  (* Because [A.disjunction] is lazy in its second argument, it is possible
     to stop early. *)

  let rec disjunction get prods : property =
    match prods with
    | [] ->
        P.bottom
    | prod :: prods ->
        if G.ignore prod then
          disjunction get prods
        else
          A.disjunction
            (production get prod)
            (fun () -> disjunction get prods)

  (* Analysis of a nonterminal symbol. *)

  let nonterminal nt get : property =
    match A.shortcut nt with
    | Some p ->
        p
    | None ->
        (* Disjunction over all productions of this nonterminal symbol. *)
        disjunction get (G.productions nt)

  (* The analysis is the least fixed point of [nonterminal]. *)

  module F =
    Fix.Make(M)(P)

  let nonterminal =
    F.lfp nonterminal

  let get =
    nonterminal

  (* The analysis is performed on demand, when and where [nonterminal]
     is called by the user. *)

  (* The following auxiliary functions are published
     for the convenience of the user. *)

  let symbol sym =
    symbol get sym

  let production_suffix prod i =
    assert (not (G.ignore prod));
    let rhs = G.rhs prod in
    let n = Array.length rhs in
    production_suffix get rhs n i

  let production prod =
    production_suffix prod 0

end (* Make *)
