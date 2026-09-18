(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Web browser reporters for {!Logs}. *)

(** {1 Reporters} *)

val console_reporter : unit -> Logs.reporter
(** [console_reporter ()] logs message using the
    {{:https://github.com/DeveloperToolsWG/console-object/blob/master/api.md}
    browser console object} at the corresponding level and uses
    [console.log] for the [App] level.

    The reporter does not process or render information about
    message sources or tags.

    Consult the {{:http://caniuse.com/#search=console}browser support}. *)
