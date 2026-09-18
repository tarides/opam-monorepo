(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs.format_reporter ());
  ()
