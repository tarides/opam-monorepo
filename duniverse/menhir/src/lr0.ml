(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Channels
open Grammar

(* -------------------------------------------------------------------------- *)

(* If requested, print the reference graph of the grammar. *)

let () =
  if Settings.reference_graph then
    let module R = ReferenceGraph.Make(Grammar) in
    R.print (Settings.base ^ ".dot")

(* -------------------------------------------------------------------------- *)

(* If [--random-sentence] was specified on the command line, obey it. *)

let () =
  Settings.random_sentence |> Option.iter @@ fun (nt, goal, style) ->
  match Nonterminal.lookup nt with
  | exception Not_found ->
      Report.Just.error [] "the nonterminal symbol %s does not exist." nt
  | nt ->
      let module R = RandomSentenceGenerator.Make(Grammar) in
      let sentence = R.nonterminal nt goal in
      print_endline (Sentence.print style (Some nt, sentence));
      exit 0

(* ------------------------------------------------------------------------ *)

(* Perform loop detection. *)

let () =
  monitor @@ fun c ->
  Time.time "Detecting loops" @@ fun () ->
  let module L = LoopDetection.Make(Grammar) in
  L.detect_cycle c;
  L.detect_hidden_left_recursion c

(* ------------------------------------------------------------------------ *)

(* Construct the LR(0) automaton. *)

include LR0Construction.Make(Grammar)

let () =
  Report.log (getA 1)
    "Built an LR(0) automaton with %d states." n
