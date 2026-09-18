(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module computes and publishes information about the shape and
   content of the stack in an LR(1) automaton.

   Using [StackSymbols] and [StackStates], it determines the shape and
   content of the stack in several situations: 1- when a state is about to
   be entered, 2- when a production is about to be reduced, and 3- when a
   goto transition is about to be taken.

   Furthermore, it determines which states should be represented (that is,
   which states need to physically exist on the stack at runtime) and
   which symbols need to keep track of their start or end positions (that
   is, which positions need to physically exist on the stack at runtime).

   This information computed is used in the code back-end and in the Rocq
   back-end. It is not used in the table back-end. *)

open Report
open MiddleAPI

module Make
(Lr1 : LR1)
(X : REPRESENT_SETTINGS)
(E : sig

  (**This channel is used to log information messages. *)
  val info: channel

end)
: sig

  (**The types [shape], [cell], and their operations. *)
  include KINDERGARTEN
    with type symbol := Lr1.Lr0.G.Symbol.t
     and type node := Lr1.node
     and type nodes := Lr1.NodeSet.t

  (**[print_represented c] prints which states are represented
     via the channel [c]. *)
  val print_represented: channel -> unit

  (**In the "short invariant", the length of the known shape of the stack
     at a node [node] is the maximum position of the bullet in the items
     of this node. *)
  module Short : sig

    include SHAPE
      with module G = Lr1.Lr0.G
       and type node := Lr1.node
       and type shape := shape

    (**[print_stack_states c] prints the results of the stack states
       analysis via the channel [c]. *)
    val print_stack_states: channel -> unit

  end

  (**In the "long invariant", the length of the known shape of the stack
     can be longer than in the short invariant. It is obtained via a data
     flow analysis of the LR(1) automaton. *)
  module Long : sig

    include SHAPE
      with module G = Lr1.Lr0.G
       and type node := Lr1.node
       and type shape := shape

    (**[print_stack_states c] prints the results of the stack states
       analysis on the output channel [c]. *)
    val print_stack_states: channel -> unit

  end

end
