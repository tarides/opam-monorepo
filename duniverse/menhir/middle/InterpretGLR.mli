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

module Make (Lr1 : LR1) : sig

  (**[interpret show_cst] is a potentially endless interactive loop. It reads
     sentences from the standard input channel and interprets them. If an empty
     sentence is read then the process is stopped with exit code 0. To allow
     interactive use, each sentence is interpreted as soon as it is read. This
     behavior corresponds to [--interpret] and [--interpret-show-cst]. *)
  val interpret: [`ShowCST | `DoNotShowCST] -> unit

end (* Make *)
