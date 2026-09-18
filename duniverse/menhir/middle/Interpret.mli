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

module Make (Lr1 : LR1) (X : sig
  include STRATEGY_SETTINGS
  include TRACE_SETTINGS
end) : sig

  (**[interpret show_cst] is a potentially endless interactive loop. It reads
     sentences from the standard input channel and interprets them. If an empty
     sentence is read then the process is stopped with exit code 0. To allow
     interactive use, each sentence is interpreted as soon as it is read. This
     behavior corresponds to [--interpret] and [--interpret-show-cst]. *)
  val interpret: [`ShowCST | `DoNotShowCST] -> unit

  (**[interpret_error()] reads one sentence from the standard input channel and
     interprets it. It confirms that this sentence ends in an error and displays
     the number of the state that is reached. Then the process is stopped with
     exit code 0. This behavior corresponds to [--interpret-error]. *)
  val interpret_error: unit -> unit

end (* Make *)
