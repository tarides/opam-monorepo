(* This code is in the public domain. *)

(* Example setup for a simple command line tool with colorful output. *)

let hello msg =
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
