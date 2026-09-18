(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module implements the A* graph search algorithm, following Hart,
   Nilsson, and Raphael (1968), under the assumption that edge costs are
   low (nonnegative) integers. *)

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

  (**[estimate node] returns an under-approximation of the cost of the
     shortest path from the node [node] to some goal node. A correct
     under-approximation is zero everywhere; if this trivial estimate is used,
     then A* degenerates into Dijkstra's shortest paths algorithm.

     The definition of the goal set is implicit in the definition of the
     function [estimate]. Otherwise, nothing distinguishes a goal node. *)
  val estimate: node -> int

end) : sig
  open G

  (**A path (from a goal node back to a source node) is described by a series
     of labels and ends in a source node. *)
  type path =
    | Edge of label * path
    | Source of node

  (**A path can also be presented as a pair of a source node and a list of
     labels, which describe the edges from the source node to a goal node.
     The function [reverse] computes this presentation. *)
  val reverse: path -> node * label list

  (**[search yield] runs the search algorithm. It must be invoked at most
     once.

     During the search, each newly discovered node [node] is presented to the
     user, in order of increasing distance from the source nodes, by invoking
     [yield node path]. The path [path] is an optimal path from this node back
     to some source node.

     The function [yield] returns a Boolean request: [true] allows the search
     to continue; [false] causes the search to stop.

     The search terminates once the graph has been entirely traversed (that
     is, once every reachable node has been yielded) or once [yield] has
     returned [false]. *)
  val search: (node -> path -> bool) -> unit

  (**The following functions may be called only after the search has ended
     (either spontaneously or because [yield] has returned [false]). *)

  (**[distance node] returns the distance from the set of source nodes to the
     node [node]. If this node has not been reached by the search then
     [max_int] is returned. *)
  val distance: node -> int

  (**[path node] returns a shortest path from the set of source nodes to the
     node [node]. If this node has not been reached by the search then
     [Not_found] is raised. *)
  val path: node -> path

end
