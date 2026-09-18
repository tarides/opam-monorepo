(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module transforms an LR(1) automaton by introducing default
   reductions.

   If a node has an end-of-stream conflict, then a default reduction cannot be
   introduced at this node. In deterministic (LR) mode, end-of-stream
   conflicts have normally already been resolved, prior to this
   transformation, by running [SevereConflictResolution]. In non-deterministic
   (GLR) mode, end-of-stream conflicts can still exist. *)

open Report
open MiddleAPI

module Run (A : LR1_AUTOMATON) : sig

  include LR1_AUTOMATON with module Lr0 = A.Lr0
                         and type node = A.node

  (**[diagnostics c] logs a message on the channel [c] indicating how
     many default reductions have been introduced. *)
  val diagnostics: channel -> unit

end
