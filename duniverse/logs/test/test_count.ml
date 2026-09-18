(*---------------------------------------------------------------------------
   Copyright (c) 2025 The logs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let test_count =
  Test.test "Logs.{err,warn}_count" @@ fun () ->
  let logit () =
    Logs.warn (fun m -> m "Hey");
    Logs.err (fun m -> m "Ho");
    Logs.warn (fun m -> m "Let's go");
  in
  logit ();
  Test.int (Logs.err_count ()) 1 ~__POS__;
  Test.int (Logs.warn_count ()) 2 ~__POS__;
  Logs.set_level None;
  logit ();
  Test.int (Logs.err_count ()) 2 ~__POS__;
  Test.int (Logs.warn_count ()) 4 ~__POS__;
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
