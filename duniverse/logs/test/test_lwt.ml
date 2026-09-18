(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let ( >>= ) = Lwt.bind

let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () = match level with
      | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
      | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () = over (); Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf;
  in
  { Logs.report = report }

let test_count () =
  let logit () =
    Logs_lwt.warn (fun m -> m "Hey") >>= fun () ->
    Logs_lwt.err (fun m -> m "Ho") >>= fun () ->
    Logs_lwt.warn (fun m -> m "Let's go")
  in
  Test.int (Logs.err_count ()) 1 ~__POS__;
  Test.int (Logs.warn_count ()) 1 ~__POS__;
  Logs.set_level None;
  logit () >>= fun () ->
  Test.int (Logs.err_count ()) 2 ~__POS__;
  Test.int (Logs.warn_count ()) 3 ~__POS__;
  Lwt.return_unit

let main () =
  Test.main @@ fun () ->
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter @@ lwt_reporter ();
  Lwt_main.run @@ begin
  Logs.set_level (Some Logs.Debug);
  Logs_lwt.info (fun m -> m ~header:"START" ?tags:None "Starting main")
  >>= fun () -> Logs_lwt.warn (fun m -> m "Hey be warned by %d." 7)
  >>= fun () -> Logs_lwt.err (fun m -> m "Hey be errored.")
  >>= fun () -> Logs_lwt.debug (fun m -> m "Be debugged a bit ?")
  >>= fun () -> Logs_lwt.app (fun m -> m "Application console or stdout.")
  >>= fun () -> Logs_lwt.info (fun m -> m "Ending main")
  >>= fun () -> test_count ()
end

let () = if !Sys.interactive then () else exit (main ())
