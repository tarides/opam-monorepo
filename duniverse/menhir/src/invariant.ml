(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Channels

(* This artificial dependency ensures that Freeze runs before we execute
   the costly analyses that follow. *)

module F = Freeze

include StackShape.Make(Lr1)(Settings)(struct let info = getC 1 end)

let () =
  Short.print_stack_states (getC 3);
  print_represented (getC 3);
  Long.print_stack_states (getC 3)
