(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module writes a description of an LR(1) automaton into an [.automaton]
   file on disk. *)

open MiddleAPI

module Make (A : LR1_AUTOMATON) () : sig

  (**[dump filename] writes a description of the automaton into a new file
     named [filename]. *)
  val dump: string -> unit

end (* Make *)
