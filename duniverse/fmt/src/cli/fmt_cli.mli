(*---------------------------------------------------------------------------
   Copyright (c) 2015 The fmt programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {!Cmdliner} support for [Fmt]. *)

(** {1 Option for setting the style renderer} *)

val style_renderer : ?env:Cmdliner.Cmd.Env.info -> ?docs:string -> unit ->
  Fmt.style_renderer option Cmdliner.Term.t
(** [style_renderer ?env ?docs ()] is a {!Cmdliner} option [--color] that can
    be directly used with the optional arguments of
    {{!Fmt_tty.tty_setup}TTY setup} or to control
    {{!Fmt.set_style_renderer}style rendering}.  The option is
    documented under [docs] (defaults to the default in
    {!Cmdliner.Arg.info}).

    The option is a tri-state enumerated value that when used with
    {{!Fmt_tty.tty_setup}TTY setup} takes over the automatic setup:
    {ul
    {- [--color=never], the value is [Some `None], forces no styling.}
    {- [--color=always], the value is [Some `Ansi_tty], forces ANSI styling.}
    {- [--color=auto] or absent, the value is [None], automatic setup
       takes place.}}

    If [env] is provided, the option default value ([None]) can be
    overridden by the corresponding environment variable. *)
