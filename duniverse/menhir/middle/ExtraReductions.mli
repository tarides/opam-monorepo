(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module introduces extra reductions in an LR(1) automaton in order to
   obey user-provided %on_error_reduce directives. *)

open Report
open MiddleAPI

module Run (A : LR1_AUTOMATON) : sig

  include LR1_AUTOMATON with module Lr0 = A.Lr0
                         and type node = A.node

  (**[warn c] emits information messages on channel [c] about the extra
     reductions that have been introduced. *)
  val info: channel -> unit

  (**[warn c] emits warnings on channel [c] about useless %on_error_reduce
     declarations. *)
  val warn: channel -> unit

end
