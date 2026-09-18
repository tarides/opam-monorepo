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

  (**[reduce_reduce prod1 prod2] determines how a reduce/reduce conflict
     between the productions [prod1] and [prod2] should be resolved. *)
  val reduce_reduce:
    Production.t -> Production.t -> Production.t option

end)
(E : sig
  open Report

  (**This channel is used to emit warnings, both during conflict
     resolution and when [diagnostics()] is invoked. *)
  val main: channel

end)
= struct
include A
open Lr0.G
open E

let start_time =
  Time.start()

include Fix.Numbering.Operations(A)

(* -------------------------------------------------------------------------- *)

(* Mutable state. *)

(* These arrays are indexed by node indices. *)

(* The array [_reductions] is initialized here with the data supplied by
   the function [A.reductions]. During the traversal, some reductions
   are removed, because of conflict resolution. *)

let _reductions : Reductions.t array =
  init A.reductions

(* -------------------------------------------------------------------------- *)

(* Read accessors. *)

(* These accessors are public: they are part of the signature [LR1_AUTOMATON].*)

let[@inline] reductions node =
  _reductions.(A.encode node)

(* Write accessors. *)

let[@inline] filter_reductions node f =
  let i = A.encode node in
  _reductions.(i) <- TerminalMap.fold f _reductions.(i) TerminalMap.empty

let[@inline] kill_reduction node t =
  let i = A.encode node in
  let _prods, reductions = TerminalMap.find_and_remove t _reductions.(i) in
  _reductions.(i) <- reductions

(* -------------------------------------------------------------------------- *)

(* [best prod prods] chooses which production, out of the nonempty list
   [prod :: prods], should be reduced. If no best choice exists then one
   or more warnings are emitted and an arbitrary choice is made. *)

(* The cause for not knowing which production is best could be:
   1- the productions originate in different source files;
   2- they are derived, via inlining or expansion of parameterized
      non-terminal symbols, from the same production. *)

let rec best candidate = function
  | [] ->
      candidate
  | prod :: prods ->
      match Precedence.reduce_reduce candidate prod with
      | Some candidate ->
          best candidate prods
      | None ->
          let open Production in
          let () =
            Report.warning main (positions candidate @ positions prod)
              "do not know how to resolve a reduce/reduce conflict\n\
               between the following two productions:\n%s\n%s"
              (print candidate) (print prod)
          in
          candidate (* arbitrary *)

(* -------------------------------------------------------------------------- *)

(* Resolve shift/reduce and reduce/reduce conflicts. *)

(* We implicitly assume that all conflicts are severe; we resolve all of them.
   We implicitly assume that all states are reachable; we resolve conflicts
   at every state. *)

(* We resolve conflicts by removing reductions. *)

let shift_reduce, reduce_reduce =
  ref 0, ref 0

let () =
  iter @@ fun node ->
  let transitions = transitions node in
  filter_reductions node @@ fun t prods reductions ->
  assert (prods <> []);
  if SymbolMap.mem (Symbol.T t) transitions then (
    (* There is a transition along [t], so this is a (possibly multi-way)
       shift/reduce conflict. Resolve it in favor of shifting by suppressing
       all reductions. *)
    shift_reduce := List.length prods + !shift_reduce;
    reductions
  )
  else
    (* There is no transition along [t].
       Test whether multiple reductions are enabled. *)
    match prods with
    | [] | [_] ->
        (* There is just one reduction. No conflict. *)
        TerminalMap.add t prods reductions
    | prod :: ((_ :: _) as prods) ->
        (* We have a reduce/reduce conflict. Resolve in favor of a single
           reduction, which is chosen by the function [best]. *)
        reduce_reduce := List.length prods + !reduce_reduce;
        TerminalMap.add t [ best prod prods ] reductions

(* -------------------------------------------------------------------------- *)

(* Resolve end-of-stream conflicts. *)

(* If a state has both a reduce action at [#] and some other (shift or reduce)
   action, this is an end-of-stream conflict. This conflict is resolved by
   suppressing the reduce action at [#]. *)

(* Because we have already removed some reductions above, we may find fewer
   end-of-stream conflicts than we would if we detected them first. *)

let eos_conflicts =
  ref 0

let () =
  iter @@ fun node ->
  let transitions = transitions node
  and reductions = reductions node in
  if Reductions.has_eos_conflict transitions reductions then (
    eos_conflicts := !eos_conflicts + 1;
    kill_reduction node Terminal.sharp
  )

(* -------------------------------------------------------------------------- *)

(* Diagnostics. *)

let signal_conflicts kind n =
  if n = 1 then
    Report.warning main []  "one %s conflict was arbitrarily resolved." kind
  else if n >= 1 then
    Report.warning main [] "%d %s conflicts were arbitrarily resolved." n kind

let diagnostics () =
  signal_conflicts "shift/reduce" !shift_reduce;
  signal_conflicts "reduce/reduce" !reduce_reduce;
  signal_conflicts "end-of-stream" !eos_conflicts

(* -------------------------------------------------------------------------- *)

let () =
  Time.stop start_time "Resolving severe conflicts"

end (* Run *)
