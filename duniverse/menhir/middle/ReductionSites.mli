(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module computes which states are able to reduce each production
   and warns about productions that can never be reduced. *)

open Report
open MiddleAPI

module Run (A : LR1_AUTOMATON) : sig
  open A.Lr0.G
  open A

  (**[sites prod] is a list of the nodes that are able to reduce
     the production [prod]. *)
  val reduction_sites: Production.t -> node list

  (**[diagnostics c] emits warnings on the channel [c] about productions
     that can never be reduced. *)
  val diagnostics: channel -> unit

end
