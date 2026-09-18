(*---------------------------------------------------------------------------
   Copyright (c) 2025 The logs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* See https://github.com/dbuenzli/logs/issues/57 *)

let src = Logs.Src.create "repro case"
module Log = (val Logs.src_log src)

let setup_logs () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level ~all:true (Some Logs.Debug);
  Logs_threaded.enable ();
  ()

let main () =
  setup_logs ();
  (try Logs.app (fun _m -> failwith "uh oh...") with Failure _ -> ());
  Logs.app (fun m -> m "It works!");
  0

let () = if !Sys.interactive then () else exit (main ())
