(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

module P = IntPQueue.Boxed

let fail format =
  Printf.ksprintf invalid_arg format

(* To each visited graph node, the algorithm associates an internal record of
   type [inode], which itself is wrapped in a box of type [box]. Therefore the
   algorithm's space complexity is linear in the size of the graph.

   The mapping of a node to a box is implemented via a hash table, while the
   reverse mappings, from a box to an inode and from an inode to a node, are
   direct: they are performed via record fields.

   The nodes that remain to be examined are stored in a priority queue, where
   the priority of a node is the cost of the shortest known path from the
   start node to it plus the estimated cost of a path from this node to a goal
   node. Lower priority nodes are considered first.

   It is the use of the second summand that makes A* more efficient than
   Dijkstra's standard algorithm for finding shortest paths in an arbitrary
   graph. When [G.estimate] is the constant function zero, A* coincides with
   Dijkstra's algorithm. One should note that A* is faster than Dijkstra's
   algorithm only when a path to some goal node exists. Otherwise, both
   algorithms explore the entire graph, and have similar time requirements. *)

module Make (G : sig

  (**This signature defines an implicit representation of graphs where edges
     have integer costs, there is a set of source nodes, and there is a set of
     goal nodes. Furthermore, we assume that (perhaps through some geometric
     knowledge) it is possible to safely under-estimate the cost of a shortest
     path from a node to the goal set. *)

  (**Graph nodes must support equality and hashing. *)
  type node
  include Hashtbl.HashedType with type t := node

  (**Edge labels. *)
  type label

  (**[sources] enumerates the source node(s). *)
  val sources: (node -> unit) -> unit

  (**[successors node] enumerates the outgoing edges of the node [node]
     in an arbitrary order. Each edge is represented by a triple of the
     edge's label, the edge's cost, and the edge's destination node. *)
  val successors: node -> (label -> int -> node -> unit) -> unit

  (**[estimate node] returns an estimate of the cost of the shortest path from
     the node [node] to some goal node. For algorithms such as A* and IDA* to
     find shortest paths, this estimate must be a correct under-approximation
     of the actual cost. *)
  val estimate: node -> int

end) = struct

type path =
  | Edge of G.label * path
  | Source of G.node

let rec follow labels path =
  match path with
  | Source node ->
      node, labels
  | Edge (label, path) ->
      follow (label :: labels) path

let reverse path =
  follow [] path

(* With each graph node, we associate the following information, which we
   group in a record of type [inode]:
   - [node] is the graph node associated with this inode.
   - [path] is a best known path from from a source node to this node.
   - [cost] is the cost of a best known path from a source to this node.
     In the paper, it is known as g-hat.
   - [estimate] is the estimated cost of the best path from this node to
     the goal set. It is equal to [G.estimate node]. It must be stored
     so that [G.estimate node] is computed only once per node. *)

type inode = {
  node: G.node;
  mutable path: path;
  mutable cost: int;
  estimate: int;
}

(* Set up the priority queue. It contains boxes which contain inodes. *)

(* While a node is in the queue, its priority is the sum of its [cost]
   and [estimate] fields. *)

type box =
  inode P.box

let q : inode P.t =
  P.create()

(* Set up a hash table that maps graph nodes to boxes. *)

module M = struct

  module H =
    Hashtbl.Make(struct include G type t = node end)

  let t : box H.t =
    H.create 4096

  (* [add node box] adds an entry to the hash table. *)
  let[@inline] add node box =
    assert (not (H.mem t node));
    H.add t node box

  (* [box node] looks up the hash table. It can raise [Not_found]. *)
  let[@inline] box node : box =
    H.find t node

  (* [get node] looks up the hash table. It can raise [Not_found]. *)
  let[@inline] get node : inode =
    P.payload (box node)

  (* [remove box] removes a node from the hash table. *)
  let[@inline] remove box =
    H.remove t (P.payload box).node

end (* M *)

module Search (Y : sig
  val yield: G.node -> path -> bool
end) = struct

  (* Wrap the user-supplied function [G.estimate] with a defensive check. *)

  let[@inline] estimate node =
    let e = G.estimate node in
    if e < 0 then fail "cost estimate is negative (%d)" e;
    e

  (* As a precaution, when computing the sum of two costs, detect overflow. *)

  let[@inline] sum cost1 cost2 =
    let cost = cost1 + cost2 in
    assert (0 <= cost);
    cost

  (* When a new node is discovered, a new inode and box are allocated,
     and the box is inserted into the priority queue. *)

  let discover node path cost =
    let estimate = estimate node in
    let box = P.box { node; path; cost; estimate } in
    M.add node box;
    P.add q box (sum cost estimate)

  (* Initialization: discover the sources. *)

  let () =
    G.sources @@ fun node ->
      let path = Source node and cost = 0 in
      discover node path cost

  (* [process box] processes a box that has just been extracted out of
     the priority queue. *)

  exception Stopped

  let process box =
    let { node; path; cost; _ } = P.payload box in
    (* The distance to this node is now fixed. Let the user know about it. *)
    let continue = Y.yield node path in
    if not continue then raise Stopped;
    (* Examine this node's outgoing edges. *)
    G.successors node @@ fun label edge_cost target ->
      (* A defensive check. *)
      if edge_cost < 0 then fail "edge cost is negative (%d)" edge_cost;
      (* Determine the cost of the best known path from the source set,
         through the node [node], through this edge, to the node [target]. *)
      let cost = sum cost edge_cost in
      (* Has the node [target] been discovered yet? *)
      match M.box target with
      | box ->
          (* Yes, [target] has been discovered already. *)
          let itarget = P.payload box in
          if cost < itarget.cost then begin
            itarget.cost <- cost;
            itarget.path <- Edge (label, path);
            (* This path to [target] is shorter than the previously known
               paths. It must be the case that [target] is still in the
               priority queue. Update (decrease) its priority. *)
            assert (P.mem q box);
            let fhat = sum cost itarget.estimate in
            assert (fhat < P.priority box);
            P.update q box fhat
          end
      | exception Not_found ->
          (* No; [target] has not been discovered until now. Allocate a new
             inode and box for it, and insert this box into the queue. *)
          let path = Edge (label, path) in
          discover target path cost

  (* The main loop. *)

  (* As long as the queue is nonempty, extract and process one box. *)

  let main : unit =
    try
      P.repeat q process
    with Stopped ->
      (* The search has been stopped by the user. Every node that is still in
         the priority queue must be removed from the hash table, so that the
         functions [distance] and [path] do not allow the user to obtain
         suboptimal information about it. *)
      P.iter q M.remove

end (* Search *)

(* Present the functor [Search] as a function [search]. *)

(* Ensure that it is called at most once. *)

let called = ref false

let search yield =
  if !called then fail "search: must be called at most once";
  called := true;
  let module S = Search(struct let yield = yield end) in
  ignore S.main (* avoid warning 60 *)

(* These functions offer access to the results of the search. *)

let distance node =
  try (M.get node).cost with Not_found -> max_int

let path node =
  (M.get node).path (* raises [Not_found] if no path was found *)

end (* Make *)
