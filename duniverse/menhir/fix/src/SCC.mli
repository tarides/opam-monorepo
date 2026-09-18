(******************************************************************************)
(*                                                                            *)
(*                                    Fix                                     *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

(**This module provides an implementation of Tarjan's algorithm, which
   computes the strongly connected components of a directed graph. *)

(**The algorithm runs when [Run(G)] is called. The time complexity of the
   computation is {m O(V+E)}, where {m V} is the number of vertices of the
   graph [G] and {m E} is the number of its edges. *)
module Run (G : sig

  (**The type of vertices, or nodes. *)
    type node

  (**[n] is the number of nodes in the graph. *)
  val n: int

  (**Each node must have a unique integer index. These indices must range
     from 0 (included) to [n] (excluded). The function [index] maps a node
     to its integer index. *)
  val index: node -> int

  (**[iter] enumerates all nodes in the graph. That is, [iter yield]
     applies the function [yield] to every node in the graph. *)
  val iter: (node -> unit) -> unit

  (**[successors] enumerates the immediate successors of a node. That is, if
     [v] is a node then [successors yield v] applies the function [yield] to
     every node [w] such that there exists an edge from [v] to [w]. *)
  val successors: (node -> unit) -> node -> unit

end) : sig

  open G

  (**[representative] maps each node to a representative element of its
     component. *)
  val representative: node -> node

  (**[scc] maps the representative element of a component to a list of all
     members of this component. It maps a non-representative element to the
     empty list. *)
  val scc: node -> node list

  (**[iter yield] enumerates all components. For each component, the function
     [yield] is applied to this component's representative element and to a
     list of the component's elements. (This must be a nonempty list.) The
     components are enumerated in topological order: that is, a component is
     examined before its successors are examined. *)
  val iter: (node -> node list -> unit) -> unit

  (**[rev_topological_iter yield] enumerates all components. For each
     component, the function [yield] is applied to this component's
     representative element and to a list of the component's elements.
     (This must be a nonempty list.) The components are enumerated in
     reverse topological order: that is, a component is examined after
     its successors have been examined. *)
  val rev_topological_iter: (node -> node list -> unit) -> unit

  (**Like {!iter}, [map yield] enumerates all components, in topological
     order. A list of the results produced by each invocation of [yield]
     is returned. *)
  val map: (node -> node list -> 'a) -> 'a list

  (**like [rev_topological_iter], [rev_map yield] enumerates all components,
     in reverse topological order. It produces a list of results {i in
     topological order}. That is, in comparison with {!map}, the order of
     the traversal differs, but the order of the results in the result list
     is the same. *)
  val rev_map: (node -> node list -> 'a) -> 'a list

end
