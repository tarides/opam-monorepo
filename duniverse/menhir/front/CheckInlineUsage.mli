(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module checks that the grammar does not use %inline rules. This check
   is performed only when the Rocq back-end has been selected by the user. *)

open Report
open PlainSyntax

(**[check] checks that the grammar does not use %inline rules. *)
val check: channel -> grammar -> unit
