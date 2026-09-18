(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open MLazy
open MiddleAPI

module Run
(Lr1 : LR1)
(S : SHAPE
 with module G = Lr1.Lr0.G
  and type node := Lr1.node
  and type shape := int)
= struct
module G = S.G
let variant = S.variant
open G

(* The parameter [S] is our stack height oracle. *)

(* We wish to compute, at each node [node], a vector of sets of nodes,
   whose length is [S.node_shape node]. *)

(* Define a data flow graph [F]. *)

(* Its vertices are the stack cells of interest. (2021/10/15) Defining a
   data flow graph where vertices are individual stack cells and properties
   are sets of states is preferable to one where vertices are nodes and
   properties are vectors of sets of states. On large automata, the speed
   difference can be more than 2x. *)

module F = struct

  (* A stack cell is identified by a node [node] in the LR(1) automaton and
     an index [i] into the known suffix of the stack at node [node]. This
     index is comprised between 0 and [node_shape node], excluded. *)

  (* Unlike our usual convention, here, the top stack cell is numbered 0,
     the next cell is numbered 1, and so on. (That is, we count from the
     right towards the left.) *)

  (* Below, we exploit the fact that this type supports OCaml's generic
     equality and hashing. *)

  type variable =
    Lr1.node * int

  (* To each cell, we wish to associate a set of states. *)

  type property =
    Lr1.NodeSet.t

  let leq_join =
    Lr1.NodeSet.leq_join

  (* For each transition in the automaton, the cell at index 0 in the target
     node is a root of the data flow analysis. Indeed, this cell is pushed
     onto the stack (therefore, this cell appears) when this transition is
     followed. *)

  let foreach_root contribute =
    Lr1.iter @@ fun source ->
    let property = Lr1.NodeSet.singleton source in
    Lr1.transitions source |> SymbolMap.iter @@ fun _symbol target ->
    assert (0 < S.node_shape target);
    contribute (target, 0) property

  (* The edges of the data flow graph are the transitions of the automaton.
     Along each transition, the cell at index [i] at the source node flows
     into the cell at index [i+1] at the target node, provided the latter
     cell exists. (The stack at the target is truncated so as to avoid
     obtaining a vector that is longer than expected/necessary.) *)

  (* It is interesting to note that the property flows, but is not
     transformed: this is a graph reachability problem in disguise.
     It is really just a matter of computing which PUSHes reach which
     stack cells. *)

  let foreach_successor (source, i) states contribute =
    Lr1.transitions source |> SymbolMap.iter @@ fun _symbol target ->
    if i + 1 < S.node_shape target then
      contribute (target, i + 1) states

end (* F *)

(* On demand, compute the least fixed point. *)

module T = struct
  type t = F.variable
end

let node_shape () : F.variable -> F.property option =
  let module F = Fix.DataFlow.ForType(T)(F)(F) in
  F.solution

let node_shape : F.variable -> F.property option =
  lazily node_shape ()

(* If every state is reachable, then the least fixed point must be non-[None]
   everywhere, so we may view it as a function that produces a vector of sets
   of states. *)

let node_shape (cell : F.variable) : F.property =
  assert (let (s, i) = cell in 0 <= i && i < S.node_shape s);
  Option.get (node_shape cell)

(* To the end user, we want to propose an API that is based on vectors of
   sets of states. *)

type shape =
  Lr1.NodeSet.t array

let show_shape (v : shape) =
  if Array.length v = 0 then
    "epsilon"
  else
    MString.separated_list Lr1.NodeSet.print "; " (Array.to_list v)

(* Adapt [node_shape] to the external API. *)

let node_shape : Lr1.node -> shape =
  lazily Lr1.tabulate @@ fun node ->
  let n = S.node_shape node in
  Array.init n @@ fun i ->
  let i = n - 1 - i in
  node_shape (node, i)

(* [truncate_join height f nodes] computes a join of the images through [f] of
   the nodes in the set [nodes], truncated at height [height]. *)

let bottom height : shape =
  Array.make height Lr1.NodeSet.empty

let truncate k (v : shape) : shape =
  assert (k <= Array.length v);
  MArray.suffix v k

let leq_join (v1 : shape) (v2 : shape) : shape =
  MArray.leq_join Lr1.NodeSet.leq_join v1 v2

let truncate_join height (f : Lr1.node -> shape) nodes =
  Lr1.NodeSet.fold (fun node accu ->
    leq_join (truncate height (f node)) accu
  ) nodes (bottom height)

(* From the above information, deduce, for each production, the shape
   of the stack when this production is reduced. *)

(* We produce a vector of states whose length is [S.production_shape prod].
   It is up to the user to provide an appropriate height oracle [S]. *)

let production_shape : Production.t -> shape =
  lazily Production.tabulate @@ fun prod ->
  let sites = Lr1.reduction_sites prod in
  let height = S.production_shape prod in
  truncate_join height node_shape sites

(* Compute the shape of the stack when a transition on the nonterminal
   symbol [nt] is taken. *)

(* We produce a vector of states whose length is [S.goto_shape nt].
   It is up to the user to provide an appropriate height oracle [S]. *)

let goto_shape : Nonterminal.t -> shape =
  lazily Nonterminal.tabulate @@ fun nt ->
  let symbol = Symbol.N nt in
  (* Compute the join of the stack shapes at every target of an edge
     labeled with [nt]. *)
  let targets = Lr1.targets symbol in
  let height = S.goto_shape nt in
  truncate_join height node_shape targets

end (* Run *)
