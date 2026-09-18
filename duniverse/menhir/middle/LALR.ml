(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* In LALR mode, two LR(1) states are merged as soon as they have the same
   LR(0) core. *)

open MiddleAPI

module Make (Lr0 : LR0) = struct
module Lr0 = Lr0
open Lr0.G

type lr1state =
  Lr0.ALR1.t

(* -------------------------------------------------------------------------- *)

(* The LALR automaton has exactly the same states as the LR(0) automaton, up
   to lookahead information. Therefore, we can use the same nodes. Thus, the
   states and the transitions of the LALR automaton are the same as those of
   the LR(0) automaton! *)

type node =
  Lr0.node

type t =
  node

let n =
  Lr0.n

let encode =
  Lr0.encode

let decode =
  Lr0.decode

let entry =
  Lr0.entry

let transitions =
  Lr0.outgoing_edges

(* -------------------------------------------------------------------------- *)

(* This means that we have almost nothing to do: in fact, the only thing that
   we have to do is compute a mapping of LR(0) nodes to LR(1) states. *)

(* This computation can be viewed as a fixed point computation. In fact, it is
   a special kind of fixed point computation: it can be viewed as a forward
   data flow analysis where the graph is the LR(0) automaton and a property is
   an LR(1) state. *)

(* A property is an LR(1) state. The function [leq_join] is used to detect
   stabilization and to merge the contribution of a predecessor state into a
   successor state. We exploit the fact that [Lr0.ALR1.union s1 s2] is
   physically equal to [s2] if [s1] is a subet of [s2]. (Yes, we live on the
   edge.) *)

module P = struct
  type property = lr1state
  let leq_join = Lr0.ALR1.union
end

(* The graph. *)

module G = struct

  type variable = node
  type property = P.property

  (* The root nodes are the entry nodes of the LR(0) automaton. The properties
     associated with these nodes are given by the function [Lr0.ALR1.start]. *)

  let foreach_root yield =
    Lr0.entry |> ProductionMap.iter @@ fun _prod node ->
    yield node (Lr0.ALR1.start node)

  (* The edges are the edges of the LR(0) automaton, and the manner in which
     each edge contributes to the computation of a property is given by the
     function [Lr0.ALR1.transition]. *)

  let foreach_successor node state yield =
    Lr0.foreach_outgoing_edge node @@ fun symbol successor_node ->
    let successor_state : lr1state = Lr0.ALR1.transition symbol state in
    yield successor_node successor_state

end

(* Run the data flow computation. *)

module F = Fix.DataFlow.ForNumberedType(Lr0)(P)(G)
  (* [solution : variable -> property option]. *)
  (* Because every node is reachable, this function never returns [None]. *)

(* -------------------------------------------------------------------------- *)

(* Expose the mapping of nodes to LR(1) states. *)

let state : node -> lr1state =
  Lr0.tabulate @@ fun node ->
  Option.get (F.solution node)

(* -------------------------------------------------------------------------- *)

end (* Make *)
