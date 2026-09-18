(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module computes predecessor edges in an LR(1) automaton. *)

open MiddleAPI

(**Applying [Make] takes linear time. The function [predecessors] can
   then be queried in constant time. *)
module Make (A : MINIMAL_LR1_AUTOMATON) : sig

  (**[predecessors node] returns an unordered list of the predecessors
     of the node [node] in the automaton. *)
  val predecessors: A.node -> A.node list

end
