(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module performs a static analysis of the LR(1) automaton in order to
   determine which states might be held in the known suffix of the stack at
   each node. *)

open MiddleAPI

(**The functor [Run] performs a simple data flow analysis to determine which
   states may appear in each stack cell at each node. It requires the height
   of the stack at each node to be already known. The parameter [S] provides
   this height information.

   In this computation, there is no distinction between the "short" invariant
   and the "long" invariant; the difference between them resides in the stack
   heights that are received as a parameter.

   This analysis is performed on demand: that is, nothing is computed until
   one of the functions returned by this functor is invoked. *)
module Run
(Lr1 : LR1)
(S : SHAPE
     with module G = Lr1.Lr0.G
      and type node := Lr1.node
      and type shape := int)
   : SHAPE
     with module G = Lr1.Lr0.G
      and type node := Lr1.node
      and type shape = Lr1.NodeSet.t array
