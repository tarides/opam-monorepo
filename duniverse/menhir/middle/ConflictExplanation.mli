(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module explains the conflicts in an LR(1) automaton. The automaton
   is not required to be canonical, and it may have been modified already by
   a phase of conflict resolution, causing some transitions and reductions
   to be removed. The explanations are written to a [.conflicts] file on
   disk. If not all conflicts can be explained (which can happen in some
   rare situations) then a message is logged via [log]. *)

open Report
open MiddleAPI

module Run
(A : LR1_AUTOMATON)
(X : CONSTRUCTION_MODE_SETTINGS)
: sig

  (**[write c filename] writes conflict explanations to the file named
     [filename]. If some conflicts cannot be explained then an information
     message is emitted on the channel [c]. *)
  val write: channel -> string -> unit

end
