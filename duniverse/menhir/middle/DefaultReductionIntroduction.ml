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

module Run (A : LR1_AUTOMATON) = struct
include A
open Lr0.G

let start_time =
  Time.start()

include Fix.Numbering.Operations(A)

(* A node [node] can have a default reduction if it has no outgoing shift
   transitions and can reduce only one production.

   The introduction of default reductions reduces the size of the tables (or
   of the code) that describe the parser, but can alter the parser's behavior
   in the presence of errors.

   The check for default reductions subsumes the check for the case where a
   node admits a reduce action with lookahead symbol [#]. Indeed, because
   end-of-stream conflicts have been resolved ([SevereConflictResolution]), if
   a node has such a reduction action then it has no other possible action; as
   a result, it will receive a default reduction on [#], which means that the
   parser will perform this reduction without requesting the next input
   symbol. This reduction will lead the parser into a new state which has the
   same property, and so on, so the parser will rewind the entire stack and
   accept the input once the stack becomes empty.

   (New as of 2012/01/23.) A state where a shift/reduce conflict was solved in
   favor of neither (due to a use of the %nonassoc directive) must not perform
   a default reduction. Indeed, this would effectively mean that the failure
   that was requested by the user is forgotten and replaced with a reduction.
   This surprising behavior is present in ocamlyacc and was present in earlier
   versions of Menhir. See e.g. http://caml.inria.fr/mantis/view.php?id=5462

   There is a chance that we might run into trouble if the ideas described in
   the above two paragraphs collide, that is, if we forbid a default reduction
   (due to a shift/reduce conflict solved by %nonassoc) in a node where we
   would like to have default reduction on "#". This situation seems unlikely
   to arise, so I will not do anything about it for the moment. (Furthermore,
   someone who uses precedence declarations is looking for trouble anyway.)

   Between 2012/05/25 and 2015/09/25, if [--canonical] has been specified,
   then we disallow default reductions on a normal token, because we do not
   want to introduce any spurious actions into the automaton. We do still
   allow default reductions on "#", since they are needed for the automaton to
   terminate properly. From 2015/09/25 on, we again always allow default
   reductions, as they seem to be beneficial when explaining syntax errors. *)

(* -------------------------------------------------------------------------- *)

(* [no_shift transitions] tests whether the transition table [transitions]
   contains no shift transitions, that is, no transitions labeled with
   terminal symbols. It can contain goto transitions, which are labeled
   with nonterminal symbols. *)

let no_shift transitions =
  SymbolMap.for_all (fun sym _ -> Symbol.is_nonterminal sym) transitions

let has_shift transitions =
  not (no_shift transitions)

(* [no_eos_conflicts ts] tests whether the set [ts] either is the singleton
   set that contains just the token [#] or is a set of non-[#] terminal
   symbols. *)

let no_eos_conflict ts =
  TerminalSet.equal ts (TerminalSet.singleton Terminal.sharp) ||
  not (TerminalSet.mem Terminal.sharp ts)

let has_eos_conflict ts =
  not (no_eos_conflict ts)

(* -------------------------------------------------------------------------- *)

(* Compute which nodes should have a default reduction,
   and tabulate the results. *)

let count =
  ref 0

exception Abandon

let test_default_reduction node =
  (* If a default reduction at this node is forbidden, bail out. *)
  if A.forbid_default_reduction node then
    raise Abandon;
  (* Unless there is a single reduction and no shift transition, bail out. *)
  let reductions = Reductions.reverse (A.reductions node) in
  if not (ProductionMap.is_singleton reductions)
  || has_shift (A.transitions node) then
    raise Abandon;
  (* Good: there is just a reduction, of [prod] on [ts]. *)
  let prod, ts = ProductionMap.choose reductions in
  (* In deterministic (LR) mode, we can assume that end-of-stream conflicts
     have been resolved already, so [no_eos_conflict ts] must hold. In
     non-deterministic (GLR) mode, this is not guaranteed. We test this
     condition, and introduce a default reduction only if it holds. *)
  if has_eos_conflict ts then
    raise Abandon;
  (* Good: there is no end-of-stream conflict. *)
  (* Introduce a default reduction of [prod] on [ts]. *)
  incr count;
  prod, ts

let test_default_reduction node =
  try Some (test_default_reduction node) with Abandon -> None

let test_default_reduction =
  tabulate test_default_reduction

let diagnostics c =
  Report.log c
    "%d out of %d states have a default reduction."
    !count A.n

(* -------------------------------------------------------------------------- *)

let () =
  Time.stop start_time "Introducing default reductions"

end (* Run *)
