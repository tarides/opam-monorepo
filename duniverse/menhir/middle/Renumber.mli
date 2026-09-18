(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module discovers the reachable states of an LR(1) automaton and
   renumbers them in a consecutive manner. Thus, the unreachable states
   disappear.

   This analysis is conservative: a state is deemed reachable if it can be
   reached from an entry state by following shift and goto edges. It may be
   the case that in reality some productions can never be reduced and as a
   consequence some goto edges can never be taken. This is not detected. *)

open Report
open MiddleAPI

module Run (A : LR1_AUTOMATON) : sig

  include LR1_AUTOMATON with module Lr0 = A.Lr0

  (**[diagnostics c] emits an information message about the unreachable nodes
     that have disappeared. The message is emitted via the channel [c]. *)
  val diagnostics: channel -> unit

end
