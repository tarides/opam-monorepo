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
open PlainSyntaxAccessors
open GenericAnalysis

module Make
(P : Fix.PROPERTY)
(G : sig val grammar: grammar end)
(A : ANALYSIS
  with type property := P.property
  and type terminal := terminal
  and type nonterminal := nonterminal
  and type symbol := symbol)
= struct
  open G

  module G = struct
    type nonterminal = string
    type terminal = string
    type symbol = string
    type production = branch
    let classify symbol =
      if is_nonterminal grammar symbol then `N symbol else `T symbol
    let rhs branch =
      List.map (fun producer -> producer.prod_symbol) branch.producers
      |> Array.of_list
    let productions nt =
      let rule = StringMap.find nt grammar.rules in
      rule.branches
    let ignore _prod = false
  end

  module String = struct
    include String
    let hash = Hashtbl.hash (* [String.hash] appears in OCaml 5.0 *)
  end

  include
    GenericAnalysis.Make
      (Fix.Glue.HashTablesAsImperativeMaps(String))
      (P)
      (G)
      (A)

end (* Make *)
