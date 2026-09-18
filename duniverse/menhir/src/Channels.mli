(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module exposes several strictness and verbosity settings. It exposes
   a number of functions that offer access to error reporting channels. Which
   channels are chosen depends on the settings. *)

open Report

(** {2 Settings} *)

(**[disable()] disables all error, warning, and information messages. *)
val disable: unit -> unit

(**[ensable()] undoes the effect of [disable()]. *)
val enable: unit -> unit

(**[strict] determines whether warnings should be changed into errors. *)
val strict: bool ref

(**[logG] is the maximum enabled level of verbosity
   for messages that concern the grammar. *)
val logG : int ref

(**[logA] is the maximum enabled level of verbosity
   for messages that concern the automaton. *)
val logA : int ref

(**[logC] is the maximum enabled level of verbosity
   for messages that concern the generated code. *)
val logC : int ref

(** {2 Channels} *)

(**[create()] creates an error report channel. It is up to the caller to use
   [Report.exit_if] at a suitable time. Using [monitor] should be preferred. *)
val create: unit -> channel

(**[monitor action] passes an error report channel to the action [action()].
   Once this action is complete, if any error has been signaled, the process
   is aborted with exit status 1. *)
val monitor: (channel -> 'a) -> 'a

(**[getG i] returns a suitable information channel for a grammar-related
   message whose verbosity level is [i]. *)
val getG : int -> channel

(**[getA i] returns a suitable information channel for an automaton-related
   message whose verbosity level is [i]. *)
val getA : int -> channel

(**[getC i] returns a suitable information channel for a
   code-generation-related message whose verbosity level is [i]. *)
val getC : int -> channel
