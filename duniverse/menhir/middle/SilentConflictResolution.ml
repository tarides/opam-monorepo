(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open MiddleAPI

module Run
(A : LR1_AUTOMATON)
(Precedence : sig
  open A.Lr0.G

  (**A choice indicates how a shift/reduce conflict should be resolved. *)
  type choice =
    | ChooseShift
    | ChooseReduce
    | ChooseNeither
    | DontKnow

  (**[shift_reduce t prod] determines how a shift/reduce conflict between the
     terminal symbol [t] and the production [prod] should be resolved. *)
  val shift_reduce: Terminal.t -> Production.t -> choice

end)
= struct
include A
open Lr0.G

let start_time =
  Time.start()

include Fix.Numbering.Operations(A)

(* -------------------------------------------------------------------------- *)

(* In the following, we perform a depth-first traversal of the automaton [A].
   As we go, we perform silent conflict resolution. This operation removes
   some transitions and therefore can make some nodes unreachable. Unreachable
   nodes are not visited, so a conflict in an unreachable is not detected or
   reported. *)

(* A conflict that is not resolved by silent conflict resolution is severe. *)

(* We do not re-number nodes so as to exclude unreachable nodes. This can be
   done in a separate pass. *)

(* -------------------------------------------------------------------------- *)

(* Mutable state. *)

(* These arrays are indexed by node indices. *)

(* The array [_transitions] is initialized here with the data supplied by
   the function [A.transitions]. During the traversal, some transitions
   are removed, because of conflict resolution. *)

let _transitions : A.node SymbolMap.t array =
  init A.transitions

(* The array [_reductions] is initialized here with the data supplied by
   the function [A.reductions]. During the traversal, some reductions
   are removed, because of conflict resolution. *)

let _reductions : Reductions.t array =
  init A.reductions

(* (New as of 2012/01/23.) The array [forbid_default_reduction] records
   whether a shift/reduce conflict was solved in favor of neither action
   (%nonassoc). This is later used to forbid a default reduction. *)

(* This array is initialized here with the data supplied by the function
   [A.forbid_default_reduction]. During the traversal, more default reductions
   can become forbidden. *)

let _forbid_default_reduction : bool array =
  init A.forbid_default_reduction

(* [severe_shift_reduce] and [severe_reduce_reduce] count the nodes that have
   severe shift/reduce and severe reduce/reduce conflicts. *)

(* [eos] counts the nodes that have an end-of-stream conflict. *)

(* [silently_solved] counts the shift/reduce conflicts that could be
   silently resolved. *)

let severe_shift_reduce =
  ref 0

let severe_reduce_reduce =
  ref 0

let eos =
  ref 0

let silently_solved =
  ref 0

(* -------------------------------------------------------------------------- *)

(* Read accessors. *)

(* These accessors are public: they are part of the signature [LR1_AUTOMATON].*)

let[@inline] transitions node =
  _transitions.(A.encode node)

let[@inline] reductions node =
  _reductions.(A.encode node)

let[@inline] forbid_default_reduction node =
  _forbid_default_reduction.(A.encode node)

(* Write accessors. *)

let[@inline] kill_transition node (t : Terminal.t) =
  let i = A.encode node in
  _transitions.(i) <- SymbolMap.remove (Symbol.T t) _transitions.(i)

let[@inline] filter_reductions node f =
  let i = A.encode node in
  _reductions.(i) <- TerminalMap.filter f _reductions.(i)

let[@inline] kill_default_reduction node =
  _forbid_default_reduction.(A.encode node) <- true

(* -------------------------------------------------------------------------- *)

(* A view of the automaton [A] as a graph. *)

(* This view relies on the [transitions] array, as opposed to the function
   [A.transitions]. This means that, once an edge has been removed, it can
   no longer be followed. *)

module ForwardEdges = struct
  type node = A.node
  type label = Symbol.t
  let foreach_outgoing_edge node yield =
    SymbolMap.iter yield (transitions node)
  let foreach_root yield =
    ProductionMap.iter (fun _prod node -> yield node) A.entry
end

(* -------------------------------------------------------------------------- *)

(* [resolve] is invoked at the node [node] when there are reduction actions
   described by [t] and [prods] and there is an outgoing transition along [t].
   Therefore, there is at least a shift/reduce conflict. *)

(* [resolve] sets the flags [hasSR] and [hasRR] to record severe shift/reduce
   and reduce/reduce conflicts. Furthermore, it returns a Boolean flag, which
   indicates whether the reduction actions should be kept ([true]) or silently
   removed ([false]). *)

let rec resolve hasSR hasRR node t prods : bool =
  assert (not (Terminal.equal t Terminal.sharp));
  assert (prods <> []);
  if List.length prods = 1 then
    resolve1 hasSR hasRR node t prods
  else
    resolve2 hasSR hasRR node t prods

(* [resolve1] deals with the case where only one reduction action exists.
   Therefore, this is a single shift/reduce conflict. If the user-provided
   precedence declarations tell us how to solve it, follow them and modify
   the automaton by removing a transition, a reduction, or both. *)

and resolve1 hasSR _hasRR node t prods : bool =
  let open Precedence in
  let prod = List.hd prods in
  match shift_reduce t prod with
  | ChooseShift ->
      prefer_shift node t prods
  | ChooseReduce ->
      prefer_reduce node t
  | ChooseNeither ->
      prefer_neither node t prods
  | DontKnow ->
      hasSR := true;
      record_severe_conflict node t

(* [resolve2] deals with the case where two or more reduction actions exist.
   This is a shift/reduce/reduce conflict. If the priorities are such that
   each shift/reduce conflict is separately solved in favor of shifting, or
   separately solved in favor of neither action, then we resolve the entire
   composite conflict in the same way. Otherwise, we view this conflict as
   severe. *)

and resolve2 hasSR hasRR node t prods =
  let open Precedence in
  let choices = List.map (shift_reduce t) prods in
  if List.for_all ((=) ChooseShift) choices then
    prefer_shift node t prods
  else if List.for_all ((=) ChooseNeither) choices then
    prefer_neither node t prods
  else begin
    hasSR := true;
    hasRR := true;
    record_severe_conflict node t
  end

(* Auxiliary functions. *)

and prefer_shift _node _t prods : bool =
  silently_solved := !silently_solved + List.length prods;
  false (* remove these reductions *)

and prefer_reduce node t : bool =
  (* Keep the reduce action and remove the transition. The automaton is
     modified in place. This can have the subtle effect of making some
     nodes unreachable. Any conflicts in these nodes are then ignored. *)
  silently_solved := !silently_solved + 1;
  kill_transition node t;
  true (* keep these reductions *)

and prefer_neither node t prods : bool =
  silently_solved := !silently_solved + List.length prods;
  kill_transition node t;
  kill_default_reduction node;
  false (* remove these reductions *)

and record_severe_conflict _node _t : bool =
  true (* keep these reductions *)

(* -------------------------------------------------------------------------- *)

(* [discover] is invoked during the traversal when a node is discovered. *)

(* It is in charge of detecting and resolving conflicts at this node. *)

(* The code detects multi-way shift/reduce/reduce conflicts. *)

let discover node =
  (* Apply a filter to the reductions table at this node. *)
  let hasSR, hasRR = ref false, ref false in
  let () =
    filter_reductions node @@ fun t prods ->
    if SymbolMap.mem (Symbol.T t) (transitions node) then
      (* There is a transition and at least one reduction. *)
      resolve hasSR hasRR node t prods
    else if List.length prods >= 2 then (
      (* There is no transition and there are multiple reductions.
         This is a pure reduce/reduce conflict. *)
      hasRR := true;
      record_severe_conflict node t
    )
    else
      (* There is no transition and there is one reduction. No conflict. *)
      true (* keep this reduction *)
  in
  (* Detect an end-of-stream conflict. *)
  let transitions = transitions node
  and reductions = reductions node in
  if Reductions.has_eos_conflict transitions reductions then
    incr eos;
  (* Record statistics about conflicts. *)
  if !hasSR then incr severe_shift_reduce;
  if !hasRR then incr severe_reduce_reduce

(* -------------------------------------------------------------------------- *)

(* Perform a depth-first traversal of the automaton. *)

let () =
  let module M = DFS.MarkArray(A) in
  let module D = struct
    let traverse _source _symbol _target = ()
    let discover = discover
  end in
  (* [let module _] is not allowed in OCaml 4.08. *)
  let module U = DFS.Run(ForwardEdges)(M)(D) in
  ()

(* -------------------------------------------------------------------------- *)

(* Diagnostics. *)

(* Even though we does not resolve end-of-stream conflicts (in this module),
   we warn about them, so as to make the user aware of them. We resolve them
   in [SevereConflictResolution]. *)

let log c =
  match !silently_solved with
  | 0 ->
      ()
  | 1 ->
      Report.log c "One shift/reduce conflict was silently solved."
  | _ ->
      Report.log c "%d shift/reduce conflicts were silently solved."
        !silently_solved

let subject n =
  assert (n > 0);
  if n = 1 then "one state has" else Printf.sprintf "%d states have" n


let warn c counter kind =
  if !counter > 0 then
    Report.warning c [] "%s %s." (subject !counter) kind

let warnings c =
  warn c severe_shift_reduce "shift/reduce conflicts";
  warn c severe_reduce_reduce "reduce/reduce conflicts";
  warn c eos "end-of-stream conflicts"

(* -------------------------------------------------------------------------- *)

let () =
  Time.stop start_time "Resolving silent conflicts"

end (* Run *)
