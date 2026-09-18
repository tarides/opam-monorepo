(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module detects and rejects certain anomalies in the grammar,
   which cause the grammar to be outside of the class LR(1). These
   anomalies include cycles and hidden left recursion. *)

open Report
open MiddleAPI

module Make (G : GRAMMAR) : sig

  (**[detect_cycle main] checks that the grammar does not have a cycle.
     If this check fails, an error is reported via the channel [main]. *)
  val detect_cycle: channel -> unit

  (**[detect_hidden_left_recursion] checks that the grammar does not have
     hidden left recursion. If this check fails, an error is reported via
     the channel [main]. *)
  val detect_hidden_left_recursion: channel -> unit

  (**[detect_hidden_right_recursion] checks that the grammar does not have
     hidden right recursion. If this check fails, an error is reported via
     the channel [main]. *)
  val detect_hidden_right_recursion: channel -> unit

end
