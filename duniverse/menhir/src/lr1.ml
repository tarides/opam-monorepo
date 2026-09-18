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
open MiddleAPI
open Grammar

(* -------------------------------------------------------------------------- *)

(* This channel is used to report and count warnings and errors. *)

let main =
  create()

(* -------------------------------------------------------------------------- *)

(* At log level 1, test whether the grammar is in the class SLR(1). *)

(* This feature is not very useful; it is here mainly for fun. *)

let () =
  if Report.live (getG 1) then
    let module SLR = SLR.Make(Lr0) in
    let count = SLR.count_violations() in
    if count = 0 then
      Report.log (getG 1) "The grammar is SLR(1)."
    else
      Report.log (getG 1)
        "The grammar is not SLR(1) -- %d states have a conflict."
        count

(* -------------------------------------------------------------------------- *)

(* 0. Run one of of our LR(1) construction algorithms. *)

let print_mode mode =
  match mode with
  | `Canonical ->
      "canonical"
  | `InclusionOnly ->
      "no-pager"
  | `Pager ->
      "pager"
  | `LALR ->
      "lalr"

let () =
  Report.log (getA 1)
    "The construction mode is %s."
    (print_mode Settings.construction_mode)

module A0 =
  LR1Construction.Make(Lr0)(Settings)()

let () =
  Report.log (getA 1)
    "Built an LR(1) automaton with %d states."
    A0.n

(* -------------------------------------------------------------------------- *)

(* 1. Perform silent conflict resolution. *)

(* A conflict can be silently resolved (that is, resolved without a warning)
   if it is a shift/reduce conflict and the user has indicated how it should
   be solved by prodiving precedence declarations. *)

module Precedence =
  Precedence.Make(Front)(Grammar)

module A1 =
  SilentConflictResolution.Run(A0)(Precedence)

let () =
  A1.log (getA 1) (* info *)

(* -------------------------------------------------------------------------- *)

(* 2. Eliminate the unreachable states and renumber the reachable states. *)

module A2 =
  Renumber.Run(A1)

let () =
  A2.diagnostics (getA 1) (* info *)

(* Warn about unused precedence declarations. *)

let () =
  if not Settings.ignore_all_unused_precedence_levels then
    Precedence.diagnostics main (* warnings *)

let () =
  A1.warnings main (* warnings *)

(* -------------------------------------------------------------------------- *)

(* Explain the conflicts that remain after silent conflict resolution. *)

let () =
  if Settings.explain then
    Time.time "Explaining conflicts" @@ fun () ->
    let module C = ConflictExplanation.Run(A2)(Settings) in
    C.write (getA 2) (Settings.base ^ ".conflicts")

(* -------------------------------------------------------------------------- *)

(* If [--dump] is present, dump a description of this automaton, after
   silent conflict resolution and before severe conflict resolution. *)

let () =
  if Settings.dump then
    Time.time "Dumping the automaton" @@ fun () ->
    let module D = Dump.Make(A2)() in
    D.dump (Settings.base ^ ".automaton")

(* -------------------------------------------------------------------------- *)

(* 3. Perform severe conflict resolution. *)

(* When --GLR is selected, this step is skipped. *)

(* This requires a module-level conditional construct,
   which in OCaml today must be encoded in an ugly way. *)

module type AUTOMATON =
  LR1_AUTOMATON with module Lr0 = Lr0

let a3 =
  if Settings.enabled_GLR then
    (* Skip severe conflict resolution; define [A3] as [A2]. *)
    (module A2 : AUTOMATON)
  else
    (* Perform severe conflict resolution. *)
    let module A3 =
      SevereConflictResolution.Run(A2)(Precedence)
        (struct let main = main end)
    in
    A3.diagnostics(); (* warnings *)
    (module A3 : AUTOMATON)

module A3 = (val a3)

(* -------------------------------------------------------------------------- *)

(* Compute in which states each production may be reduced,
   and warn about productions that are definitely never reduced. *)

module S =
  ReductionSites.Run(A3)

let () =
  S.diagnostics main (* warnings *)

(* The function [S.reduction_sites] returns a list of nodes. We wrap it,
   later on, so as to produce a set of nodes. Note that it is important
   that the types [A3.node], [A4.node] and [A5.node] are the same type. *)

(* -------------------------------------------------------------------------- *)

(* 4. Insert extra reductions on error
      so as to obey [%on_error_reduce] declarations. *)

module A4 =
  ExtraReductions.Run(A3)

let () =
  A4.warn main (* warnings *)

let () =
  A4.info (getA 1) (* info *)

(* -------------------------------------------------------------------------- *)

(* 5. Decide which states can have a default reduction. *)

module A5 =
  DefaultReductionIntroduction.Run(A4)

let () =
  A5.diagnostics (getC 1) (* info *)

(* -------------------------------------------------------------------------- *)

(* The automaton is now final. It will no longer be transformed. *)

(* If [--dump-resolved] is present, dump a description of this automaton. *)

let () =
  if Settings.dump_resolved then
    Time.time "Dumping the automaton" @@ fun () ->
    let module D = Dump.Make(A5)() in
    D.dump (Settings.base ^ ".automaton.resolved")

(* -------------------------------------------------------------------------- *)

(* If any errors have been emitted up to this point, stop now. This includes
   the case where warnings have been emitted and [--strict] has been passed. *)

let () =
  Report.exit_if main

(* -------------------------------------------------------------------------- *)

(* This is us. *)

module  Final = A5
include Final
include Fix.Numbering.Operations(Final)

let print node =
  Printf.sprintf "%d" (encode node)

(* -------------------------------------------------------------------------- *)

(* Now implement the facilities that are needed to satisfy the
   signature [LR1], as opposed to just [LR1_AUTOMATON]. *)

(* -------------------------------------------------------------------------- *)

(* Instantiate [Set] and [Map] on the type [node]. *)

(* Because nodes are numbered between 0 and [n], instead of implementing
   [NodeSet] by using balanced binary trees, one could perhaps use some form
   of bit sets. However, the sparse bit sets in [Bitsets.SparseBitSet] can be
   slow when [n] becomes large, so I prefer to be cautious. *)

module Node = struct
  type nonrec t = t
  let compare = compare
end

module NodeSet = struct

  include Set.Make(Node)

  (* [union] does not guarantee physical equality between its second
     argument and its result when a logical equality holds. We wrap it
     so as to obtain this property. *)

  let leq_join s1 s2 =
    if subset s1 s2 then s2 else union s1 s2

  let print s =
    Printf.sprintf "{ %s }" (
      MString.separated_iter print ", " (fun f -> iter f s)
    )

end

module NodeMap =
  Map.Make(Node)

(* -------------------------------------------------------------------------- *)

(* Accessors. *)

(* The computations that follow (up to the end of this file) are not timed. *)

let reduction_sites =
  Production.tabulate @@ fun prod ->
  NodeSet.of_list (S.reduction_sites prod)

let start_node nt =
  let prod = Production.start_production nt in
  ProductionMap.find prod entry

let core node =
  Lr0.ALR1.core (state node)

let incoming_symbol node =
  Lr0.incoming_symbol (core node)

let is_start node =
  incoming_symbol node = None

let get_start node =
  assert (is_start node);
  Lr0.get_start (core node)

let has_default_reduction node =
  match test_default_reduction node with
  | Some _ ->
      true
  | None ->
      false

let has_default_reduction_on_sharp node =
  match test_default_reduction node with
  | Some (_, toks) when TerminalSet.mem Terminal.sharp toks ->
      assert (TerminalSet.cardinal toks = 1);
      true
  | _ ->
      false

let has_conflict_or_eos_conflict node =
  let transitions = transitions node
  and reductions = reductions node in
  Reductions.has_conflict transitions reductions ||
  Reductions.has_eos_conflict transitions reductions

let deterministic () =
  MRef.with_state true @@ fun deterministic ->
  iter @@ fun node ->
  if has_conflict_or_eos_conflict node then
    deterministic := false

(* -------------------------------------------------------------------------- *)

(* The map [targets] maps a symbol [symbol] to the set of all nodes
   whose incoming symbol is [symbol]. *)

let[@inline] lookup symbol targets =
  try SymbolMap.find symbol targets with Not_found -> NodeSet.empty

let targets : NodeSet.t SymbolMap.t =
  fold (fun node targets ->
    match incoming_symbol node with
    | None ->
        targets
    | Some symbol ->
        SymbolMap.add symbol (NodeSet.add node (lookup symbol targets)) targets
  ) SymbolMap.empty

let targets symbol =
  lookup symbol targets

(* -------------------------------------------------------------------------- *)

(* The array [predecessors] maps a node to the set of its predecessors. *)

(* [Predecessors.Make] yields a [predecessors] function that returns a list
   of nodes. Although this is by no means crucial, I find it more elegant to
   return a set of nodes. *)

include Predecessors.Make(Final)

let predecessors =
  tabulate @@ fun node ->
  NodeSet.of_list (predecessors node)
