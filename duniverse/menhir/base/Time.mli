(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers facilities for timing various tasks. Timing information
   is acquired via {!time} and is stored in a global log. When the process
   exits, this log is automatically flushed to the output channel that has
   been selected via {!set_output_channel}. *)

(**[time msg task] executes [task()] and returns its result. If the task
   terminates normally then the message [msg], followed with the execution
   time of the task, are appended to the log. *)
val time: string -> (unit -> 'a) -> 'a

(**[set_output_channel c] sets the output channel to [c].
   If [c] is [None] then no output is produced. *)
val set_output_channel: out_channel option -> unit

(**Unfortunately, we cannot provide a concise and elegant way of timing
   a functor application. Therefore, we also provide {!start} and {!stop}
   functions. *)

(**A start time. *)
type start

(**[start()] records a start time. *)
val start: unit -> start

(**[stop start msg] records a stop time [stop]. Then the message [msg],
   followed with the elapsed time [stop -. start], are appended to the log. *)
val stop: start -> string -> unit
