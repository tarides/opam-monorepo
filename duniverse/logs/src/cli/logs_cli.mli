(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {!Cmdliner} support for {!Logs}.

    See a full {{!ex}example}. *)

(** {1 Options for setting the report level} *)

val level : ?env:Cmdliner.Cmd.Env.info -> ?docs:string -> unit ->
    Logs.level option Cmdliner.Term.t
(** [level ?env ?docs ()] is a term for three {!Cmdliner} options that
    can be used with {!Logs.set_level}.  The options are documented
    under [docs] (defaults to the default of {!Cmdliner.Arg.info}).

    The options work as follows:
    {ul
    {- [-v] or [--verbose], if it appears once, the value of
       the term is [Some Logs.Info] and more than once
       [Some Logs.Debug].}
    {- [--verbosity=LEVEL], the value of the term is [l] where
       [l] depends on on [LEVEL]. Takes over the option [-v].}
    {- [-q] or [--quiet], the value of the term is [None]. Takes
       over the [-v] and [--verbosity] options.}
    {- If both options are absent the default value is
       [Some Logs.warning]}}

    If [env] is provided, the default value in case all options are
    absent can be overridden by the corresponding environment
    variable. *)

(** {1:ex Example}

    The following example shows how to setup {!Logs} and {!Fmt} so
    that logging is performed on standard outputs with ANSI coloring
    if these are [tty]s. The command line interface provides options
    to control the use of colors and the log reporting level.
{[
let hello _ msg =
  Logs.app (fun m -> m "%s" msg);
  Logs.info (fun m -> m "End-user information.");
  Logs.debug (fun m -> m "Developer information.");
  Logs.err (fun m -> m "Something bad happened.");
  Logs.warn (fun m -> m "Something bad may happen in the future.");
  if Logs.err_count () > 0 then 1 else 0

let setup_log ~style_renderer ~level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  Cmd.make (Cmd.info "tool") @@
  let env = Cmd.Env.info "TOOL_VERBOSITY" in
  let+ style_renderer = Fmt_cli.style_renderer ()
  and+ level = Logs_cli.level ~env ()
  and+ msg =
    let doc = "The message to output."  in
    Arg.(value & pos 0 string "Hello horrible world!" & info [] ~doc)
  in
  setup_log ~style_renderer ~level;
  hello msg


let main () = Cmd.eval' cmd
let () = if !Sys.interactive then () else exit (main ())
]}

*)
