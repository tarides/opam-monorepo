(*---------------------------------------------------------------------------
   Copyright (c) 2015 The fmt programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [Fmt] TTY setup.

    [Fmt_tty] provides simple automatic setup on channel formatters for:
    {ul
    {- {!Fmt.set_style_renderer}. [`Ansi_tty] is used if the channel
       {{!Unix.isatty}is a tty} and the environment variable
       [TERM] is defined and its value is not ["dumb"]. [`None] is
       used otherwise.}
    {- {!Fmt.set_utf_8}. [true] is used if one of the following
       environment variables has ["UTF-8"] as a case insensitive
       substring: [LANG], [LC_ALL], [LC_CTYPE].}} *)

(** {1:tty_setup TTY setup} *)

val setup : ?style_renderer:Fmt.style_renderer -> ?utf_8:bool ->
  out_channel -> Format.formatter
(** [setup ?style_renderer ?utf_8 outc] is a formatter for [outc] with
    {!Fmt.set_style_renderer} and {!Fmt.set_utf_8} correctly setup. If
    [style_renderer] or [utf_8] are specified they override the automatic
    setup.

    If [outc] is {!stdout}, {!Fmt.stdout} is returned. If [outc] is
    {!stderr}, {!Fmt.stderr} is returned. *)

val setup_std_outputs : ?style_renderer:Fmt.style_renderer -> ?utf_8:bool ->
  unit -> unit
(** [setup_std_outputs ?style_renderer ?utf_8 ()] applies {!setup}
    on {!stdout} and {!stderr}. *)
