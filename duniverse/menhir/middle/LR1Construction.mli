(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module constructs an LR(1) automaton using one of four methods, which
   differ in the amount of fusion of states that they perform. It is a wrapper
   for the three modules [LR1Canonical], [LR1Pager], and [LALR]. *)

open MiddleAPI

module Make (Lr0 : LR0) (X : CONSTRUCTION_MODE_SETTINGS) ()
: LR1_AUTOMATON with module Lr0 = Lr0
