(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module first constructs an LR(1) automaton by using one of several
   construction methods (see [LR1Construction]). Then, this automaton is
   further transformed, in three steps:

   1. Silent conflict resolution (without warnings),
      following the user's precedence declarations.
      This can remove transitions and reductions.

   2. Severe conflict resolution (with warnings).
      This can remove reductions.

   3. Addition of extra reductions,
      following the user's [%on_error_reduce] declarations.

   The severe conflicts are explained after step 1, and before steps 2 and 3. *)

open MiddleAPI

include LR1 with module Lr0 = Lr0
