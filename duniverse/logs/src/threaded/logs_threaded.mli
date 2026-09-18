(*---------------------------------------------------------------------------
   Copyright (c) 2019 The logs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Thread safe logging. *)

val enable : unit -> unit
(** [enable ()] enables thread safe logging for OCaml {!Thread}s by
    installing mutual exclusion primitives via
    {!Logs.set_reporter_mutex}. *)
