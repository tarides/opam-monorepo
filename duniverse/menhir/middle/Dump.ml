(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf
open MiddleAPI

module Make (A : LR1_AUTOMATON) () = struct
module Lr0 = A.Lr0
open Lr0.G

include Fix.Numbering.Operations(A)

let sharp =
  Terminal.sharp

(* -------------------------------------------------------------------------- *)

(* Compute the known stack suffix at every node. *)

module Short =
  StackSymbols.Short(A)()

(* -------------------------------------------------------------------------- *)

(* These functions write a description of (various aspects of) the node [node]
   to the output channel [out]. *)

let print_number out node =
  fprintf out "State %d:\n"
    (A.encode node)

let print_known_stack_suffix out node =
  fprintf out
    "## Known stack suffix:\n\
     ##%s\n"
    (Symbol.print_array' false (Short.node_shape node))

let print_items out node =
  fprintf out "## LR(1) items:\n%s"
    (Lr0.ALR1.print "" (A.state node))

let print_transitions out node =
  fprintf out "## Transitions:\n";
  A.transitions node |> SymbolMap.iter @@ fun symbol node ->
  fprintf out "-- On %s shift to state %d\n"
    (Symbol.print false symbol) (A.encode node)

let print_reductions out node =
  match A.test_default_reduction node with
  | Some (prod, ts) ->
      (* There is a default reduction. *)
      (* Because end-of-stream conflicts have been resolved, either [ts] is
         the singleton set that contains just the token [#], or it is a set of
         non-[#] terminal symbols. *)
      assert (
        TerminalSet.equal ts (TerminalSet.singleton sharp) ||
        not (TerminalSet.mem sharp ts)
      );
      let keyword = if TerminalSet.mem sharp ts then "Without" else "After" in
      fprintf out "## Default reduction:\n";
      fprintf out "-- %s reading the next token, %s\n"
        keyword (Production.describe false prod);
  | None ->
      (* There is no default reduction. *)
      fprintf out "## Reductions:\n";
      (* 2020/11/21: for better readability, we now group the symbols that
         lead to reducing the same production. *)
      let reductions = Reductions.reverse (A.reductions node) in
      reductions |> ProductionMap.iter @@ fun prod toks ->
      fprintf out "-- On %s\n" (TerminalSet.print toks);
      fprintf out "--   %s\n" (Production.describe false prod)

let print_conflicts out node =
  let transitions = A.transitions node
  and reductions = A.reductions node in
  let conflict_symbols = Reductions.conflict_symbols transitions reductions in
  if not (TerminalSet.is_empty conflict_symbols) then
    fprintf out "** Conflict on %s\n"
      (TerminalSet.print conflict_symbols)

let print_eos_conflict out node =
  let transitions = A.transitions node
  and reductions = A.reductions node in
  if Reductions.has_eos_conflict transitions reductions then begin
    (* Conceptually suppress the reduce action at [#]. *)
    let prods, reductions = TerminalMap.find_and_remove sharp reductions in
    (* Compute the set of terminal symbols involved in
       transitions and in the remaining reductions. *)
    let ts =
      TerminalSet.union
        (SymbolMap.terminals transitions)
        (TerminalMap.domain reductions)
    in
    (* This set cannot be empty; otherwise there would be no conflict. *)
    assert (not (TerminalSet.is_empty ts));
    (* The list [prods] can have several elements. Pick one. *)
    assert (prods <> []);
    let prod = List.hd prods in
    (* Report an end-of-stream conflict. *)
    fprintf out "** End-of-stream conflict on %s\n"
      (TerminalSet.print ts);
    fprintf out
      "**   There is a tension between\n\
       **   (1) %s\n\
       **   without even requesting a lookahead token, and\n\
       **   (2) testing whether the lookahead token is a member of the above set.\n"
      (Production.describe true prod)
  end

let describe out node =
  print_number out node;
  print_known_stack_suffix out node;
  print_items out node;
  print_transitions out node;
  print_reductions out node;
  print_conflicts out node;
  print_eos_conflict out node

(* -------------------------------------------------------------------------- *)

(* The main function. *)

let dump filename =
  IO.write filename @@ fun out ->
  (* Describe every node. *)
  iter @@ fun node ->
  describe out node;
  fprintf out "\n"

end (* Make *)
