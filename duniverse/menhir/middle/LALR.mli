(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module constructs an LALR automaton. *)

(**At this stage, the precedence declarations play no role. *)

open MiddleAPI

module Make (Lr0 : LR0) : MINIMAL_LR1_AUTOMATON with module Lr0 = Lr0
