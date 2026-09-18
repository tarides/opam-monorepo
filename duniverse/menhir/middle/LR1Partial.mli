(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**When given an LR(1) automaton and a node in this automaton where there is
   a conflict, this algorithm finds a similar node in the canonical LR(1)
   automaton where there is a similar conflict. This is required in order
   to explain this conflict. *)

open MiddleAPI

(**This exception is raised by [Run] if a path to the goal node is not found. *)
exception Oops

(**The functor [Run] is applied to a possibly non-canonical LR(1) automaton,
   where some transitions and reductions may have been removed due to conflict
   resolution. It searches for a path in the canonical LR(1) automaton that
   explains a conflict in the node [X.goal] on a lookahead symbol that is a
   member of the set [X.conflicts]. If such a path is not found, the exception
   [Oops] is raised. *)
module Run (A : LR1_AUTOMATON)
(X : sig
open A.Lr0.G

  (**A goal node in the LR(1) automaton. *)
  val goal: A.node

  (**A nonempty set of terminal symbols such that in the node [goal]
     there is a conflict on every terminal symbol in this set. *)
  val conflicts: TerminalSet.t

end) : sig
open A.Lr0.G

  (**A node in the canonical LR(1) automaton is a goal node if (i) [node] has
     a conflict involving one of the terminal symbols in the set [X.conflicts]
     and (ii) the path that leads to [node] in the canonical automaton leads
     to the node [goal] in the existing automaton [A]. *)

  (**If a path is found, then three pieces of information are returned: the
     source node of this path; the path itself; the final node of this path. *)

  (**[source] identifies the source node of the path. More precisely, since
     the source node must be a start node, its LR(0) core must contain a
     single item. [source] is this item. It is a start item. *)
  val source: Item.t

  (**[path] is a sequence of edge labels (symbols). It determines a path
     out of the source node in the existing LR(1) automaton [A] and in
     the canonical automaton. In the existing automaton, the final node
     of this path is [X.goal]. *)
  val path: Symbol.t array

  (**[goal] is the state to which this path leads in the canonical automaton.
     The lookahead sets of this state are restricted to [X.conflicts]. *)
  val goal: A.Lr0.CLR1.t

  (**[t] is an arbitrary terminal symbol such that the state [goal] in the
     canonical automaton has a conflict on [t]. *)
  val t: Terminal.t

end
