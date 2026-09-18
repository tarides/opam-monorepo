(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This is the core of the reachability analysis. After the automaton has been
   constructed, this analysis determines exactly under which conditions each
   nonterminal edge in the automaton can be taken. This information can then
   be used to determine how to reach certain states in the automaton; see,
   e.g., [LRijkstra]. *)

(* In this analysis, we explicitly ignore the [error] token. (We display a
   warning if the grammar uses this token.) Thus, we disregard any reductions
   or transitions that take place when the lookahead symbol is [error]. As a
   result, any state whose incoming symbol is [error] is found unreachable. It
   would be too complicated to have to create a first error in order to be
   able to take certain transitions or drop certain parts of the input. *)

(* In the output of the reachability algorithm, the core part is the submodule
   [Graph]. The reachability results form a graph whose nodes and edges
   (transitions) refine the LR(1) automaton. A transition is labelled with the
   shortest word that allows taking this transition. *)

open Grammar

module Run () : sig

  module Word : sig
    type t
    val singleton : Terminal.t -> t
    val elements : t -> Terminal.t list
    val compare : t -> t -> int
    val length : t -> int
  end (* Word *)

  module Graph : sig

    (* Graph nodes. *)
    type node
    include Hashtbl.HashedType with type t := node

    val state : node -> Lr1.node
    val lookaheads : node -> TerminalSet.t

    (* Edge labels. *)
    type label
    val append_word : label -> Word.t -> Word.t

    (* The source node(s). *)

    val sources: (node -> unit) -> unit

    (* [successors n f] presents each of [n]'s successors, in
       an arbitrary order, to [f], together with the cost of
       the edge that was followed. *)
    val successors: node -> (label -> int -> node -> unit) -> unit

  end (* Graph *)

  module Statistics : sig
    val header : string
    val print : out_channel -> time:float -> heap:int -> unit
  end (* Statistics *)

end
