(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Report
open Syntax
open Sort

(**[infer main info g] performs sort inference for the grammar [g]. If the
   grammar is well-sorted, it returns a sort assignment. Otherwise, it fails.
   Errors are reported via the channel [main]. The sort of every symbol is
   printed on the channel [info]. If verbosity is not desired, [null] channels
   can be used. *)
val infer: channel -> channel -> grammar -> sorts
