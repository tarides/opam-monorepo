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

(* Let us write A -> alpha when there exists a production A -> alpha, and let
   us write beta => gamma when the sentential form beta expands (in one step)
   to gamma. *)

(* We build the graph of the relation A => B. (Let us refer to this relation
   as R.) Then, under the assumption that the grammar is acyclic, we construct
   a list of the nonterminal symbols that respects this relation. *)

module Make (X : sig val grammar: grammar end) = struct
  open X

  module Nullable =
    Nullable.Make(X)

  (* [successors nt] enumerates the successors of the nonterminal symbol [nt]
     in the relation _ => _ defined above. *)

  (* A similar function exists in [LoopDetection]. *)

  let successors nt yield =
    (* For every production of the symbol [nt], *)
    let rule = StringMap.find nt grammar.rules in
    rule.branches |> List.iter @@ fun branch ->
    (* for every position [i] in the right-hand side, *)
    let rhs = Array.of_list branch.producers in
    let n = Array.length rhs in
    let nullable_prefix = ref true in
    let i = ref 0 in
    while !nullable_prefix && !i < n do
      let producer = rhs.(MInt.postincrement i) in
      let symbol = producer.prod_symbol in
      if is_terminal grammar symbol then
        nullable_prefix := false
      else
        let nt' = symbol in
        (* If the symbol at position [i] is a nonterminal symbol [nt'] and
           if the prefix and suffix are both nullable then yield [nt']. *)
        if Nullable.production_suffix branch !i then
          yield nt';
        nullable_prefix := Nullable.symbol nt'
    done

  (* We now number every nonterminal symbol, in a way that respects the
     relation R. That is, if A appears before B in the list then A => B
     is possible but B => A is impossible. *)

  (* Using [defensive_fix] guarantees that every symbol is visited at
     most once and that cycles are detected. *)

  module M =
    Fix.Memoize.ForType(String)

  let nonterminals =
    ref []

  let visit : nonterminal -> unit =
    M.defensive_fix @@ fun visit nt ->
    (* To visit [nt], first visit its successors, *)
    successors nt visit;
    (* then add [nt] itself to the list. *)
    nonterminals := nt :: !nonterminals

  (* Visit all symbols. This traversal must either raise [Cycle] or
     produce a topologically sorted list of symbols. *)

  let nonterminals =
    try
      foreach_nonterminal grammar visit;
      Some !nonterminals
    with M.Cycle _ ->
      None

end
