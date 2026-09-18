(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers facilities for displaying error messages.  *)

open Range

(**The functions offered by this module are organized in several groups.

   The simpler functions in the submodule {!Just} do not expect a channel as
   an argument; they have immediate effect.

   The more complex functions, such as {!error}, {!signal}, and {!warning},
   take a channel as an argument. A channel combines an output channel (in the
   usual sense) and a counter of the number of errors that have been signaled.

   To obtain a fresh channel and use it in a correct way, the recommended way
   is to use {!monitor}. The functions {!create} and {!exit_if} offer an
   alternative way, which is slightly more powerful, but more complex. *)

(** {2 Functions that Do Not Need a Channel} *)

module Just : sig

  (**[Just.error ranges format ...] is a short-hand for
     [let c = create `Normal stderr in error c ranges format ...].
     The error message is immediately written to [stderr]
     and the process is aborted with exit status 1. *)
  val error: ranges -> ('a, out_channel, unit, 'b) format4 -> 'a

  (**[Just.warning ranges format ...] is a short-hand for
     [let c = create `Normal stderr in warning c ranges format ...].
     The warning is immediately written to [stderr]. *)
  val warning: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

end

(** {2 Functions that Need a Channel} *)

(**A channel combines an output channel (to which messages are written), an
   error counter, and a mode (which can be used to turn signals into warnings
   or vice-versa). There is a special channel [null] whose output channel is
   /dev/null and whose error counter is always zero. *)
type channel

(**[error c ranges format ...] signals an error and writes an error message on
   the channel [c], then aborts the process with exit status 1: therefore,
   this function does not return.

   The message begins with the list of ranges [ranges]. The remainder of the
   message is determined by the format [("Error: " ^^ format ^^ "\n") ...].
   The string produced by [format ...] should begin with a lowercase letter
   and end with a dot. *)
val error: channel -> ranges -> ('a, out_channel, unit, 'b) format4 -> 'a

(**[signal c ranges format ...] signals an error and writes an error message
   on the channel [c], then returns a value of type [unit]. The error counter
   of the channel [c] is incremented.

   The message begins with the list of ranges [ranges]. The remainder of the
   message is determined by the format [("Error: " ^^ format ^^ "\n") ...].
   The string produced by [format ...] should begin with a lowercase letter
   and end with a dot.

   If the channel's mode is [`SignalIsWarning] then [signal] behaves like
   [warning]. *)
val signal: channel -> ranges -> ('a, out_channel, unit, unit) format4 -> 'a

(**[warning c ranges format ...] writes a warning message on the channel [c],
   then returns a value of type [unit]. The error counter of the channel [c]
   is {b not} incremented.

   The message begins with the list of ranges [ranges]. The remainder of the
   message is determined by the format [("Warning: " ^^ format ^^ "\n") ...].
   The string produced by [format ...] should begin with a lowercase letter
   and end with a dot.

   If the channel's mode is [`WarningIsSignal] then [warning] behaves like
   [signal]. *)
val warning: channel -> ranges -> ('a, out_channel, unit, unit) format4 -> 'a

(**[note c ranges format ...] writes an information message on the channel
   [c], then returns a value of type [unit].

   The message begins with the list of ranges [ranges]. The remainder of the
   message is determined by the format [("Note: " ^^ format ^^ "\n") ...].
   The string produced by [format ...] should begin with a lowercase letter
   and end with a dot. *)
val note: channel -> ranges -> ('a, out_channel, unit, unit) format4 -> 'a

(**[log c format ...] writes an information message on the channel [c], then
   returns a value of type [unit]. The string produced by [format ...] should
   begin with a lowercase letter and end with a dot. A newline character is
   appended to the message. *)
val log: channel -> ('a, out_channel, unit, unit) format4 -> 'a

(**[flush c] flushes the output channel that underlies the channel [c]. *)
val flush: channel -> unit

(** {2 Functions that Create Channels} *)

(**The mode [`WarningIsSignal] causes [warning] to behave like [signal].
   The mode [`SignalIsWarning] causes [signal] to behave like [warning]. *)
type mode =
  [`Normal | `WarningIsSignal | `SignalIsWarning]

(**[monitor mode action] applies the function [action] to a fresh channel
   [c], which is created by [create ?mode stderr]. Once [action] terminates
   (either normally or by raising an exception), [exit_if c] is invoked. Thus,
   if the number of errors that have been signaled on the channel [c] is
   nonzero, then the process is aborted with exit status 1. *)
val monitor : mode -> (channel -> 'a) -> 'a

(**[create mode f] creates a fresh channel whose mode is [mode] and whose
   underlying output channel is [f]. *)
val create: mode -> out_channel -> channel

(**[exit_if c] tests whether the number of errors that have been signaled on
   the channel [c] is nonzero. If so, the process is aborted with exit status
   1. *)
val exit_if: channel -> unit

(** {2 The Null Channel} *)

(**The channel [null] drops all of the messages that are emitted on it; one
   might say that its underlying output channel is /dev/null. Its mode is
   [`Normal]. An operation on this channel has no effect, except [error],
   which prints nothing and aborts the process with exit status 1. *)
val null: channel

(**[live c] tests whether the channel [c] is live, that is, not [null]. *)
val live: channel -> bool
