(*---------------------------------------------------------------------------
   Copyright (c) 2019 The logs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let enable () =
  let lock = Mutex.create () in
  let lock () = Mutex.lock lock and unlock () = Mutex.unlock lock in
  Logs.set_reporter_mutex ~lock ~unlock
