(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module prints an LR(1) automaton as a graph in [.dot] format.
   Each state of the automaton gives rise to a node. Edges are labeled
   with nonterminal and terminal symbols. The reduction actions that
   exist in each state are not shown. *)

open MiddleAPI

module Make (A : MINIMAL_LR1_AUTOMATON) : sig

  (**[print filename] writes a description of the graph of the automaton [A]
     to a new file named [filename]. *)
  val print: string -> unit

end
