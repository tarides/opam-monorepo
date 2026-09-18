(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(* The graph-structured stack (GSS). *)

(* This is an acyclic directed graph (DAG). In a general GLR algorithm, the
   GSS can be cyclic: cycles can appear between nodes that inhabit the same
   generation, that is, nodes that have the same value of the field [date].
   Inside such a cycle, every edge must be labeled with a nullable nonterminal
   symbol. Furthermore, such a cycle can arise only if the grammar has hidden
   left recursion. At this time, we reject such grammars, so our GSS must be
   acyclic. Our code does not explicitly take advantage of the absence of
   cycles in the GSS. *)

(* We assume that the grammar is acyclic (this is in fact implied by the
   absence of hidden left recursion). Following McPeak, we take advantage of
   this assumption to construct semantic values in a bottom-up manner. We do
   not construct parse trees or parse forests, but if we did, they would be
   acyclic. *)

(* The GSS is made of nodes and edges. *)

type ('s, 'v) node = {

  state         : 's;
  (**A state of the LR(1) automaton. *)

  date          : int;
  (**An input offset, measured in tokens. This number can be understood as the
     number of input tokens that were consumed before this node was created.
     It can also be understood as the logical time at which this node was
     created. *)

  mutable edges : ('s, 'v) edge MiniBabySet.tree;
  (**The outgoing edges of this node. When the GSS is understood as a set of
     LR stacks, each edge represents a possible tail of the stack. An initial
     node has no outgoing edges; a non-initial node has at least one outgoing
     edge. Every edge must go towards a node that inhabits the same generation
     or an older generation. New edges can be added to a node at offset [date]
     only while the parser is performing reductions at this offset; once this
     process is over, this field becomes immutable. *)
  (* We represent the set of outgoing edges as a balanced binary tree so as to
     allow searching for a specific edge (more precisely, searching for an
     edge with a specific destination node) in time O(log n). A linked list
     would require searching in worst-case linear time. A micro-benchmark on
     an ambiguous grammar suggests that using a balanced binary tree does not
     make a measurable difference. So, this decision does not seem to buy us
     extra performance in common cases, but it could buy us some protection
     against pathological cases. It does cost some space, since a tree node
     occupies 5 words whereas a list node occupies 3 words. Furthermore, it
     prevents us from following McPeak's suggestion: to inline the first edge
     of the linked list into the record [node]. *)

  mutable ddepth: int;
  (**The deterministic depth of this node, as described by McPeak. This is
     the number of outgoing edges that one can follow without encountering
     a join node. *)

}

and ('s, 'v) edge = {

  (* The source node of an edge is not recorded. *)

  (* The label of an edge is not recorded. It would be the incoming symbol of
     the state [parent.state], where [parent] is the source node of this edge.
     The semantic value [semv] carried by this edge must be consistent with
     this symbol. *)

  node          : ('s, 'v) node;
  (**The destination node of this edge. *)

  mutable semv  : 'v;
  (**The semantic value carried by this edge. Semantic values are constructed
     in a bottom-up manner: therefore this field goes through two distinct
     phases. In the first phase, this field can be updated: the semantic value
     that is stored in this field can be merged with a newly discovered
     semantic value, which represents another way of parsing the same input
     segment. In the second phase, the field becomes immutable and public: the
     semantic value that it contains can be passed to semantic actions. The
     two phases must not overlap: a semantic value that is not yet complete
     must not be made visible to a semantic action. *)

  mutable locked: bool;
  (**This field is [true] once the second phase has been entered. Then [semv]
     can be read by a semantic action, but can no longer be updated.
     Conversely, when this field is [false], [semv] can be updated but cannot
     be read by a semantic action. *)
  (* This field is used for sanity checking only and can be removed once
     debugging is complete. Removing it allows saving at best 3% in time and
     2 words per token, in deterministic benchmarks. For now, we keep it. *)

}
