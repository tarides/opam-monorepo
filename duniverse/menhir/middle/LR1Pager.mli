(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module constructs an LR(1) automaton by using an algorithm that
   extends the canonical construction with the ability to merge states. Two
   modes are provided. In the simpler mode, [`InclusionOnly], two states can
   be merged only if one is a subset of the other. This yields an LR(1)
   automaton whose states exist also in the canonical LR(1) automaton. In the
   more complex and more aggressive mode, [`Pager], a variant of Pager's weak
   compatibility criterion determines whether two states can be merged. In
   either mode, the construction does not create any artificial conflicts:
   that is, if the canonical LR(1) automaton has no conflicts then the
   automaton produced by this algorithm has no conflicts. *)

(**At this stage, the precedence declarations play no role. *)

open MiddleAPI

module Make (Lr0 : LR0)
(X : sig val construction_mode : [`Pager | `InclusionOnly] end)
 : MINIMAL_LR1_AUTOMATON with module Lr0 = Lr0
