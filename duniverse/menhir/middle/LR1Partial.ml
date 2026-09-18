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

exception Oops

module Run (A : LR1_AUTOMATON)
(X : sig
open A.Lr0.G

  (**A goal node in the LR(1) automaton. *)
  val goal: A.node

  (**A nonempty set of terminal symbols such that in the node [goal]
     there is a conflict on every terminal symbol in this set. *)
  val conflicts: TerminalSet.t

end) = struct
open A.Lr0.G
module Lr0 = A.Lr0

let () =
  assert (not (TerminalSet.is_empty X.conflicts))

(* -------------------------------------------------------------------------- *)

(* [start2item] converts a start node into the single item that it contains.  *)

(* Perhaps this function could be placed elsewhere. For now, it is used here
   and nowhere else. It is related with [Lr0.get_start]. *)

let start2item (node : A.node) : Item.t =
  let state : Lr0.ALR1.t = A.state node in
  let core : Lr0.node = Lr0.ALR1.core state in
  let items : Item.Set.t = Lr0.items core in
  assert (Item.Set.cardinal items = 1);
  Item.Set.choose items

(* -------------------------------------------------------------------------- *)

(* First, we restrict our interest to the nodes of the LR(1) automaton that
   can reach the goal node. Some experiments show that this can involve one
   tenth to one half of all nodes. This optimization is minor, but is easy
   to implement. *)

(* A backward traversal of the LR(1) automaton, starting at the goal node,
   is used. *)

module P =
  Predecessors.Make(A)

let relevant : A.node -> bool =
  let module G = struct
    type node = A.node
    type label = unit
    let foreach_root yield =
      yield X.goal
    let foreach_outgoing_edge node yield =
      P.predecessors node |> List.iter (yield ())
  end in
  let module M = DFS.MarkArray(A) in
  let module D = struct
    let discover _node = ()
    let traverse _source _label _target = ()
  end in
  (* [let module _] is not allowed in OCaml 4.08. *)
  let module U = DFS.Run(G)(M)(D) in
  M.is_marked

(* -------------------------------------------------------------------------- *)

(* Second, all of the states that we shall consider are restricted to
   the set of terminal symbols of interest. This is an important idea:
   by abstracting away some information, we make the construction much
   faster. *)

type state =
  Lr0.ALR1.t

let restrict : state -> state =
  Lr0.ALR1.restrict X.conflicts

(* -------------------------------------------------------------------------- *)

(* We now perform a canonical LR(1) construction (yes, we do!). There is no
   fusion of states. The new automaton is represented as a graph.

   As we go, we record the correspondence between the nodes of the new
   automaton, whose type is [node], defined below, and the nodes of the
   automaton that we have been given, whose type is [A.node]. This allows us
   to tell when we have reached the desired place.

   This also allows us not to follow transitions that do not exist in the
   automaton [A] because they have been eliminated during silent conflict
   resolution. Whenever we follow a transition in the new automaton, we check
   that the corresponding transition is legal in the existing automaton.

   The new automaton is explored breadth-first. Shortest paths from every node
   to one of the start nodes are recorded. *)

(**A node in the new automaton. *)
type node = {

  state: state;
    (**The LR(1) state represented by this node. As explained above,
       this is a restricted state. *)

  ancestor: (Symbol.t * node) option;
    (**A reverse edge on a shortest path to one of the start nodes. *)

  shadow: A.node;
    (**The node in the existing automaton that corresponds
       to this node in the new automaton. *)

}

(* -------------------------------------------------------------------------- *)

(* A queue of pending nodes, whose successors should be explored. *)

let queue : node Queue.t =
  Queue.create()

(* -------------------------------------------------------------------------- *)

(* A mapping of LR(0) state numbers to lists of nodes. *)

let map : node list array =
  Lr0.init @@ fun _ -> []

let[@inline] similar state =
  let k = Lr0.encode (Lr0.ALR1.core state) in
  map.(k)

let[@inline] record_similar state node =
  let k = Lr0.encode (Lr0.ALR1.core state) in
  map.(k) <- node :: map.(k)

(* -------------------------------------------------------------------------- *)

(* The exception [Goal] is raised by [explore] when a goal node is
   reached. *)

exception Goal of node * Terminal.t

(* [explore ancestor shadow state] tests whether there exists already a node
   that corresponds to the state [state]. If so, it does nothing. Otherwise,
   it creates a new node out of [state], [ancestor], and [shadow], and
   enqueues this new node for further exploration. *)

(* [explore] finds all existing nodes that share the same LR(0) core, and
   tests whether one of these nodes coincides with the candidate new node.
   This requires comparing not only the states of the partial, canonical
   automaton, but also their shadows in the existing automaton. This is
   because a single state of the canonical automaton may be reached along
   several different paths, leading to distinct shadows in the existing
   automaton, and we must explore all of these paths in order to ensure that
   we eventually find a goal node. *)

let rec explore ancestor shadow (state : state) : unit =
  if not (List.exists (fun node ->
    Lr0.ALR1.equal state node.state && shadow == node.shadow
  ) (similar state)) then
    create ancestor shadow state

(* [create] creates a new node and tests whether it is a goal node. *)

and create ancestor shadow state =
  let node = { state; ancestor; shadow } in
  record_similar state node;
  Queue.add node queue;
  if shadow == X.goal then
    test_goal_node node

(* [test_goal_node node] tests whether [goal] is a goal node. If it is a
   goal node, the exception [Goal] is raised. We assume that the condition
   [node.shadow == X.goal] has been checked already.

   A node [node] is a goal node if (i) [node] has a conflict involving one
   of the terminal symbols of interest and (ii) [node] corresponds to the
   goal node in the existing automaton: that is, the path that leads to
   [node] in the new automaton leads to the goal node in the existing
   automaton. Note that these conditions do not uniquely define [node]. *)

and test_goal_node node =
  let { shadow; state; _ } = node in
  let transitions = A.transitions shadow
  and reductions = A.reductions shadow in
  (* We accumulate the terminal symbols for which a reduction exists
     in the new automaton and in the existing automaton. We use this
     information to detect a reduce/reduce conflict. *)
  let can_reduce = ref TerminalSet.empty in
  (* For every reduction of the production [prod] on lookahead symbol [t]
     in state [state] in the canonical automaton, *)
  Lr0.ALR1.reverse_reductions state |> ProductionMap.iter @@ fun prod ts ->
  ts |> TerminalSet.iter @@ fun t ->
  (* Test whether this reduction also exists in the existing automaton.
     It could have been suppressed by conflict resolution. *)
  if List.mem prod (TerminalMap.find t reductions) then
    (* It does exist. *)
    (* Test whether there is a shift/reduce conflict. *)
    match SymbolMap.find (Symbol.T t) transitions with
    | (_ : A.node) ->
        (* Yes, there is a shift/reduce conflict at [node] on [t]. *)
        raise (Goal (node, t))
    | exception Not_found ->
        let ts = !can_reduce in
        (* We rely on the fact that [TerminalSet.add t ts] preserves
           physical equality when [t] is a member of [ts]. *)
        let ts' = TerminalSet.add t ts in
        if ts == ts' then
          (* [t] is already in [ts], which means we have already found
             a reduction on [t]. There is a reduce/reduce conflict. *)
          raise (Goal (node, t))
        else
          (* There is no conflict so far. Record that we have found a
             reduction on [t]. *)
          can_reduce := ts'

(* -------------------------------------------------------------------------- *)

(* [init()] populates the queue with the start nodes. *)

let init () =
  Lr0.entry |> ProductionMap.iter @@ fun prod (k : Lr0.node) ->
  let shadow = ProductionMap.find prod A.entry in (* cannot fail *)
  if relevant shadow then
    let ancestor = None
    and state = restrict (Lr0.ALR1.start k) in
    explore ancestor shadow state

(* [loop()] is the main loop. Until a goal node is found, it takes a node
   out the queue, constructs the nodes that correspond to its successors in
   the new automaton, and enqueues them. *)

let loop () =
  MQueue.repeat queue @@ fun node ->
  Lr0.ALR1.transitions node.state |> SymbolMap.iter @@ fun symbol state ->
  match SymbolMap.find symbol (A.transitions node.shadow) with
  | shadow  ->
      if relevant shadow then
        explore (Some (symbol, node)) shadow (restrict state)
  | exception Not_found ->
      (* There is no shadow. This can happen if a shift/reduce conflict has
         been resolved in the existing automaton in favor in reduction. In
         this case, we ignore this transition in the new automaton. *)
      ()

(* If the main loop ends without finding a goal node, then fail. *)

(* When the construction mode is [`Canonical], [`InclusionOnly], or [`Pager],
   this should not happen. Indeed, these construction methods do not introduce
   artificial conflicts. In other words, if the goal node in the existing
   automaton has a conflict, then in the canonical automaton there should also
   exist a node with a conflict. On the other hand, if the construction mode
   is [`LALR] then this can happen. It is folklore knowledge that LALR can
   produce so-called mysterious conflicts, which cannot be explained in terms
   of the grammar, because they do not exist in the canonical automaton. *)

let (goal : node), (t : Terminal.t) =
  match init(); loop() with
  | () ->
      raise Oops
  | exception Goal (goal, t) ->
      goal, t

(* -------------------------------------------------------------------------- *)

(* There remains to build a shortest path from [goal] back to an entry node.  *)

let rec follow path node =
  match node.ancestor with
  | None ->
      start2item node.shadow,
      Array.of_list path
  | Some (symbol, node) ->
      follow (symbol :: path) node

let source, path =
  follow [] goal

let goal : Lr0.CLR1.t =
  Lr0.ALR1.export goal.state

(* -------------------------------------------------------------------------- *)

end (* Run *)
