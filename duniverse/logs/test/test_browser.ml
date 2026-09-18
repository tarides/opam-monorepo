(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Js_of_ocaml

let main _ =
  Logs.set_level @@ Some Logs.Debug;
  Logs.set_reporter @@ Logs_browser.console_reporter ();
  Logs.info (fun m -> m ~header:"START" ?tags:None "Starting main");
  Logs.warn (fun m -> m "Hey be warned by %d." 7);
  Logs.err (fun m -> m "Hey be errored.");
  Logs.debug (fun m -> m "Would you mind to be debugged a bit ?");
  Logs.app (fun m -> m "This is for the application console or stdout.");
  Logs.app (fun m -> m ~header:"HEAD" "Idem but with a header");
  Logs.err (fun m -> m "NO CARRIER");
  Logs.info (fun m -> m "Ending main");
  Js._false

let () = Js.Unsafe.set Dom_html.window "onload" (Dom_html.handler main)
