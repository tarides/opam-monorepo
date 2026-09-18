(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open MiddleAPI

module Make (Lr0 : LR0) (X : CONSTRUCTION_MODE_SETTINGS) () = struct
open X

let start_time =
  Time.start()

(* -------------------------------------------------------------------------- *)

(* Select a construction algorithm based on the command-line settings. *)

(* We would like to use a conditional construct at the level of modules.
   Because it does not exist, we have to go through first-class modules. *)

module type AUTOMATON =
  MINIMAL_LR1_AUTOMATON with module Lr0 = Lr0

let automaton =
  match construction_mode with
  | `Canonical ->
      (module LR1Canonical.Make(Lr0) : AUTOMATON)
  | `InclusionOnly | `Pager as construction_mode ->
      let module X = struct let construction_mode = construction_mode end in
      (module LR1Pager.Make(Lr0)(X) : AUTOMATON)
  | `LALR ->
      (module LALR.Make(Lr0) : AUTOMATON)

include (val automaton : AUTOMATON)

open Lr0.G

(* -------------------------------------------------------------------------- *)

(* The following definitions are independent of which construction algorithm
   is used. *)

type t =
  node

(* Expose an ideal reduction table at each node. This table is obtained
   by inspection of the LR(1) state [state i]. *)

let reductions (i : node) : Reductions.t =
  Lr0.ALR1.reductions (state i)

(* Allow default reductions everywhere. *)

let forbid_default_reduction (_i : node) =
  false

(* At this point, no default reductions exist yet. *)

let test_default_reduction (_i : node) =
  None

(* -------------------------------------------------------------------------- *)

let () =
  Time.stop start_time "Construction of the LR(1) automaton"

end (* Make *)
