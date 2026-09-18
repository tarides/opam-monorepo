(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* A mechanism to turn off warnings and information messages. *)

(* When [!enabled] is false, no messages are printed. *)

let enabled =
  ref true

let enable () =
  enabled := true

let disable () =
  enabled := false

(* -------------------------------------------------------------------------- *)

(* A mechanism to turn warnings into errors. *)

(* When [!strict] is true, warnings are changed into errors. *)

let strict =
  ref false

let mode () =
  if !strict then `WarningIsSignal else `Normal

(* -------------------------------------------------------------------------- *)

let logG, logA, logC =
  ref 0, ref 0, ref 0

(* -------------------------------------------------------------------------- *)

(* A report channel for information messages. *)

let info =
  Report.create `Normal stderr

(* -------------------------------------------------------------------------- *)

(* Accessors. *)

let create () =
  if !enabled then
    Report.create (mode()) stderr
  else
    Report.null

let monitor action =
  if !enabled then
    Report.monitor (mode()) action
  else
    action Report.null

let get max_enabled_level level =
  if !enabled && level <= max_enabled_level then
    (* This verbosity level is enabled. *)
    info
  else
    (* This verbosity level is disabled. *)
    Report.null

let[@inline] getG level =
  get !logG level

let[@inline] getA level =
  get !logA level

let[@inline] getC level =
  get !logC level
