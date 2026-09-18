(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* 2015/10/19: original implementation. 2016/07/13: use priority levels to
   choose which production should be reduced, when several productions are
   eligible. *)

(* If a state can reduce some productions whose left-hand symbol has been
   marked [%on_error_reduce], and if one such production [prod] is preferable
   to every other (according to the priority rules of [%on_error_reduce]
   declarations), then every error action in this state is replaced with a
   reduction of [prod]. This is done even though this state may have outgoing
   shift transitions: thus, we are forcing one interpretation of the past,
   among several possible interpretations. *)

(* This transformation is somewhat similar to the introduction of default
   reductions, except we do not require the absence of outgoing terminal
   transitions. Furthermore, this transformation simply modifies the reduction
   table, so the various back-ends need not be aware of this feature, whereas
   they are aware of default reductions. *)

(* This transformation can be performed before default reductions are
   introduced; it does not affect which default reductions will be
   permitted. *)

open MiddleAPI

module Run (A : LR1_AUTOMATON) = struct
include A
open Lr0.G

let start_time =
  Time.start()

include Fix.Numbering.Operations(A)

(* -------------------------------------------------------------------------- *)

(* Mutable state. *)

(* The array [_reductions] is indexed by node indices. *)

(* The array [_reductions] is initialized here with the data supplied by the
   function [A.reductions]. Later on, some reductions are added. *)

let _reductions : Reductions.t array =
  init A.reductions

(* A count of the states that receive extra reductions. *)

let extra =
  ref 0

(* A count of the states where there is more than one eligible production and
   one production is preferable to every other production (so priority plays a
   role). *)

let prioritized =
  ref 0

(* The set of the nonterminal symbols that appear in the left-hand side of an
   extra reduction. *)

let extra_nts =
  ref NonterminalSet.empty

(* -------------------------------------------------------------------------- *)

(* Read accessors. *)

(* These accessors are public: they are part of the signature [LR1_AUTOMATON].*)

let[@inline] reductions node =
  _reductions.(A.encode node)

(* Write accessors. *)

let[@inline] add_reduction node t prod =
  let i = A.encode node in
  assert (not (TerminalMap.mem t _reductions.(i)));
  _reductions.(i) <- TerminalMap.add t [prod] _reductions.(i)

(* -------------------------------------------------------------------------- *)

(* [acceptable node t] determines whether [node] has a shift or reduce action
   on the terminal symbol [t]. *)

(* We do not allow [t] to be [error]. *)

let[@inline] acceptable node t =
  not (Terminal.equal t Terminal.error) && (
    SymbolMap.mem (Symbol.T t) (A.transitions node) ||
    TerminalMap.mem t (A.reductions node)
  )

(* -------------------------------------------------------------------------- *)

(* [productions node] is a list of all the productions that can be reduced at
   [node]. *)

let[@inline] productions node : Production.t list =
  let reductions = Reductions.reverse (reductions node) in
  ProductionMap.fold (fun prod _ prods -> prod :: prods) reductions []

(* -------------------------------------------------------------------------- *)

(* [extra_reductions node] creates extra reductions at the node [node]. *)

let extra_reductions node =
  (* Compute the productions which this node can reduce. *)
  let prods = productions node in
  (* Keep only those whose left-hand symbol is marked [%on_error_reduce]. *)
  let prods = List.filter OnErrorReduce.reduce prods in
  (* Check whether one of them is preferable to every other one. If not, give
     up. This means that either no production is marked [%on_error_reduce] or
     several of them are marked and none of them is preferable. *)
  MList.best OnErrorReduce.preferable prods |> Option.iter @@ fun prod ->
  (* Inserting extra reductions is possible. Replace every error action with
     a reduction of [prod]. *)
  let triggered = ref false in
  let () =
    Terminal.iter_real @@ fun t ->
    if not (acceptable node t) then begin
      add_reduction node t prod;
      triggered := true
    end
  in
  (* If at least one error action has been replaced with a reduction, update
     [extra], [prioritized], and [extra_nts]. *)
  if !triggered then begin
    incr extra;
    if List.length prods > 1 then
      incr prioritized;
    extra_nts := NonterminalSet.add (Production.nt prod) !extra_nts
  end

(* -------------------------------------------------------------------------- *)

(* Create extra reductions at all nodes. *)

(* Just like a default reduction, an extra reduction should be forbidden
   (it seems) if [forbid_default_reduction] is set. *)

let () =
  iter @@ fun node ->
  if not (A.forbid_default_reduction node) then
    extra_reductions node

(* -------------------------------------------------------------------------- *)

(* Diagnostics. *)

let info c =
  if !extra > 0 then (
    Report.log c "Extra reductions on error were added in %d states." !extra;
    Report.log c "Priority played a role in %d of these states." !prioritized
  )

let warn c =
  (* Warn about useless %on_error_reduce declarations. *)
  OnErrorReduce.iter @@ fun nt ->
  if not (NonterminalSet.mem nt !extra_nts) then
    Report.warning c []
      "the declaration %%on_error_reduce %s is never useful."
      (Nonterminal.print false nt)

(* -------------------------------------------------------------------------- *)

let () =
  Time.stop start_time "Introducing extra reductions"

end (* Run *)
