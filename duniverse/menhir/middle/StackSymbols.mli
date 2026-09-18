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

(**This module computes the known suffix of the stack, a sequence of
   symbols, in each of the automaton's states. The length of this
   sequence can be predicted based on the LR(0) items present in this
   state: it is the maximum position of the bullet over all items.
   Every state in the automaton must be reachable. *)
module Short (Lr1 : LR1_AUTOMATON) ()
: SHAPE with module G = Lr1.Lr0.G
         and type node = Lr1.node
         and type shape = Lr1.Lr0.G.Symbol.t array

(**This module computes the known suffix of the stack, a sequence of
   symbols, in each of the automaton's states. The length of this
   sequence is determined by an analysis of the paths in the LR(0)
   automaton. At each state, the sequence computed by [Short] is
   a suffix of the sequence computed by [Long].
   Every state in the automaton must be reachable. *)
module Long (Lr1 : LR1) ()
: SHAPE with module G = Lr1.Lr0.G
         and type node = Lr1.node
         and type shape = Lr1.Lr0.G.Symbol.t array

(* The "long invariant" was used in Menhir until 2012/08/25. However, the
   extra information that it contains, compared to the "short invariant",
   was useless; computing it was a waste of time. As of 2012/08/25, the
   short invariant has been used. As of 2021/05/14, the long invariant
   is re-introduced, for use in the code back-end. *)
