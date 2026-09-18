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

(* We compute a lower bound on the height of the stack at every state, and at
   the same time, we compute which symbols are held in this stack prefix. *)

(* In order to compute a lower bound on the height of the stack at a state
   [s], we examine the LR(0) items that compose [s]. For each item, if the
   bullet is at position [pos], then we can be assured that the height of the
   stack is at least [pos]. Thus, we compute the maximum of [pos] over all
   items (of which there is at least one). *)

(* The set of items that we use is not closed, but this does not matter; the
   items that would be added by the closure would not add any information
   regarding the height of the stack, since the bullet is at position 0 in
   these items. *)

(* Instead of computing just the stack height, we compute, in the same manner,
   which symbols are on the stack at a state [s]. This is an array of symbols
   whose length is the height of the stack at [s]. By convention, the top of
   the stack is the end of the array. *)

(* This analysis is extremely fast: on an automaton with over 100,000 states,
   it takes under 0.01 second. *)

module Short (Lr1 : LR1_AUTOMATON) () = struct

  module Lr0 = Lr1.Lr0
  module G = Lr0.G
    open G
  type node = Lr1.node

  type shape =
    Symbol.t array

  let show_shape symbols =
    Symbol.print_array' false symbols

  (* Compute and tabulate this information at the level of the LR(0)
     automaton. *)

  let node_shape : Lr0.node -> shape =
    let dummy = [||] in
    lazily Lr0.tabulate @@ fun node ->
    Item.Set.fold (fun item accu ->
      let prod, pos = Item.export item in
      let rhs = Production.rhs prod in
      if pos > Array.length accu then Array.sub rhs 0 pos else accu
    ) (Lr0.items node) dummy

  (* Extend it to the LR(1) automaton. *)

  let node_shape (node : Lr1.node) : shape =
    node_shape (Lr0.ALR1.core (Lr1.state node))

  (* Add a trivial definition of [production_shape]. *)

  let production_shape =
    Production.rhs

  (* Add a trivial definition of [goto_shape]. *)

  let goto_shape nt =
    [| Symbol.N nt |]

  let variant =
    `Short

end

(* ------------------------------------------------------------------------ *)

(* The submodule [Long] computes the known suffix of the stack in each state,
   as a vector of symbols, and it computes a suffix that is as long as
   possible, in contrast with the above code, which computes a suffix whose
   length can be predicted based on the LR(0) items in each state. *)

module Long (Lr1 : LR1) () = struct

  module Lr0 = Lr1.Lr0
  module G = Lr0.G
    open G
  type node = Lr1.node

  type shape =
    Symbol.t array

  let show_shape symbols =
    Symbol.print_array' false symbols

  (* Vectors of symbols. *)

  module SymbolVector = struct
    type property =
      Symbol.t array
    let empty, push =
      MArray.(empty, push)
    let leq_join =
      MArray.leq_join_lcs
  end

  include SymbolVector

  (* Define the data flow graph. *)

  (* 2025/09/12: we used to perform this data flow analysis at the level of
     the LR(0) automaton, because we assumed that the paths in the LR(0) and
     the paths in the LR(1) automaton are the same, up to the projection that
     maps a state of the LR(1) automaton down to its LR(0) core. However, this
     is incorrect. Some edges in the LR(1) automaton can be removed by the
     conflict resolution procedure. Then, it is possible that a state in the
     LR(1) automaton can be reached via fewer paths than the corresponding
     state in the LR(0) automaton. By taking this into account, we may be able
     to produce more precise information (that is, a longer known suffix of
     the stack) at this state. We must do so, otherwise we might produce an
     invariant that is not self-consistent; that would later lead to a failure
     in StackLangCheck. (See wiktor.mly.) *)

  module Graph = struct

    type variable =
      Lr1.node

    type property =
      SymbolVector.property

    (* At each start state of the automaton, the stack is empty. *)

    let foreach_root contribute =
      Lr1.entry |> ProductionMap.iter @@ fun _prod root ->
      contribute root empty

    (* The edges of the data flow graph are the transitions of the automaton. *)

    let foreach_successor source stack contribute =
      Lr1.transitions source |> SymbolMap.iter @@ fun symbol target ->
      (* The contribution of [source], through this edge, to [target], is the
         stack at [source], extended with a new cell for this transition. *)
      contribute target (push stack symbol)

  end

  (* Compute the least fixed point. *)

  let node_shape : Lr1.node -> property option =
    let module F = Fix.DataFlow.ForNumberedType(Lr1)(SymbolVector)(Graph) in
    F.solution

  (* Assuming that every state is reachable, the least fixed point
     must be non-[None] everywhere, so we may view it as a function
     that produces a vector of symbols. *)

  let node_shape (node : Lr1.node) : property =
    Option.get (node_shape node)

  (* [join1 f nodes] computes the join of the images through [f] of the
     nodes in the set [nodes]. Because our join does not have a bottom
     element, this set must be nonempty. *)

  let join1 f nodes =
    let node = Lr1.NodeSet.choose nodes in
    let nodes = Lr1.NodeSet.remove node nodes in
    Lr1.NodeSet.fold (fun node accu ->
      leq_join (f node) accu
    ) nodes (f node)

  (* From the above information, deduce, for each production, the shape
     of the stack when this production is reduced. *)

  (* We *can* produce a vector whose length is greater than that
     of the production [prod]. *)

  let production_shape : Production.t -> property =
    Production.tabulate @@ fun prod ->
    let nodes = Lr1.reduction_sites prod in
    if Lr1.NodeSet.is_empty nodes then
      (* This production is never reduced. It is not clear what vector
         should be returned. Using the right-hand side of the production
         seems reasonable. This is what the short invariant does. *)
      Production.rhs prod
    else
      (* Compute a join over the set of nodes where this production
         is reduced. *)
      join1 node_shape nodes

  (* Compute the shape of the stack when a transition on the nonterminal
     symbol [nt] is taken. *)

  let goto_shape : Nonterminal.t -> property =
    Nonterminal.tabulate @@ fun nt ->
    let symbol = Symbol.N nt in
    (* Compute the join of the stack shapes at every target of an edge
       labeled with [nt]. *)
    let targets = Lr1.targets symbol in
    if Lr1.NodeSet.is_empty targets then
      (* No edge is labeled [nt]. *)
      [| symbol |]
    else
      join1 node_shape targets

  let variant =
    `Long

end
