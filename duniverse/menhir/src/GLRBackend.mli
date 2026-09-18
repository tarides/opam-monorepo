(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module generates code for a GLR parser. Most of this code is in fact
   data, that is, tables. This is a table-based GLR parser. *)

module Run () : sig

  val program: IL.program

end
