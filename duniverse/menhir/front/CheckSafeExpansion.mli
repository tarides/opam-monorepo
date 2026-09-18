(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers a termination test for the expansion of parameterized
   nonterminal symbols. *)

open Report
open Syntax

(**[check main g] accepts a parameterized grammar [g], where all parameters
   must have sort [*]. (Parameters of higher sort must be eliminated prior to
   running this test: see [SelectiveExpansion].) The test succeeds if and only
   if the expansion of this grammar is safe, that is, expansion terminates.
   The channel [main] is used to report errors. *)
val check: channel -> grammar -> unit
