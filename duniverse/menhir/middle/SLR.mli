(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module extends the LR(0) automaton with lookahead information in order
   to construct an SLR(1) automaton. The lookahead information is obtained by
   considering the FOLLOW sets.

   This construction is not used by Menhir, but can be used to check whether
   the grammar is in the class SLR(1). *)

open MiddleAPI

module Make (Lr0 : LR0) : sig

  (**[count_violations()] returns the number of states in the SLR(1)
     automaton that have a conflict. If this number is zero then the
     grammar is in the class SLR(1). *)
  val count_violations: unit -> int

end
