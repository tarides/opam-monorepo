(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {!Format} colorful reporter for {!Logs}. *)

(** {1 Reporter} *)

val reporter :
  ?pp_header:(Logs.level * string option) Fmt.t ->
  ?app:Format.formatter ->
  ?dst:Format.formatter -> unit -> Logs.reporter
(** [reporter] is like {!Logs.format_reporter} except ANSI colors may be
    used in message header rendering if the formatters are configured to do so;
    see {!Fmt.set_style_renderer} and {!Fmt_tty}.

    Consult a full command line {{!Logs_cli.ex}setup example}. *)

(** {1:cheader Colored message headers} *)

val app_style : Fmt.style
(** [app_style] is the style used to render headers at app level. *)

val err_style : Fmt.style
(** [err_style] is the style used to render headers at error level. *)

val warn_style : Fmt.style
(** [warn_style] is the style used to render headers at warning level. *)

val info_style : Fmt.style
(** [info_style] is the style used to render headers at info level. *)

val debug_style : Fmt.style
(** [debug_style] is the style used to render headers at debug level. *)

val pp_header : (Logs.level * string option) Fmt.t
(** [pp_header] is like {!Logs.pp_header} but may use ANSI colors if the
    formatter is configured to do so, see {!Fmt.set_style_renderer} and
    {!Fmt_tty}. *)
