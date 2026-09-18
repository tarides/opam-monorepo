(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a log = ('a, unit Lwt.t) Logs.msgf -> unit Lwt.t

let kmsg k ?(src = Logs.default) level msgf =
  begin match level with
  | Logs.Error -> Logs.incr_err_count ()
  | Logs.Warning -> Logs.incr_warn_count ()
  | _ -> ()
  end;
  match Logs.Src.level src with
  | None -> k ()
  | Some current_level when level > current_level -> k ()
  | Some _ ->
      let (ret, unblock) = Lwt.wait () in
      let k () = Lwt.bind ret k in
      let over () = Lwt.wakeup unblock () in
      Logs.report src level ~over k msgf

let kunit _ = Lwt.return ()
let msg ?src level msgf = kmsg kunit ?src level msgf
let app ?src msgf = kmsg kunit ?src Logs.App msgf
let err ?src msgf = kmsg kunit ?src Logs.Error msgf
let warn ?src msgf = kmsg kunit ?src Logs.Warning msgf
let info ?src msgf = kmsg kunit ?src Logs.Info msgf
let debug ?src msgf = kmsg kunit ?src Logs.Debug msgf

let on_error ?src ?(level = Logs.Error) ?header ?tags ~pp ~use t =
  Lwt.bind t @@ function
  | Ok v -> Lwt.return v
  | Error e ->
      kmsg (fun () -> use e) ?src level @@ fun m ->
      m ?header ?tags "@[%a@]" pp e

let on_error_msg ?src ?(level = Logs.Error) ?header ?tags ~use t =
  Lwt.bind t @@ function
  | Ok v -> Lwt.return v
  | Error (`Msg e) ->
      kmsg use ?src level @@ fun m ->
      m ?header ?tags "@[%a@]" Format.pp_print_text e

(* Source specific functions *)

module type LOG = sig
  val msg : Logs.level -> 'a log
  val app : 'a log
  val err : 'a log
  val warn : 'a log
  val info : 'a log
  val debug : 'a log
  val kmsg : ?over:(unit -> unit) -> (unit -> 'b Lwt.t) ->
    Logs.level -> ('a, 'b Lwt.t) Logs.msgf -> 'b Lwt.t

  val on_error : ?level:Logs.level -> ?header:string -> ?tags:Logs.Tag.set ->
    pp:(Format.formatter -> 'b -> unit) -> use:('b -> 'a Lwt.t) ->
    ('a, 'b) result Lwt.t -> 'a Lwt.t

  val on_error_msg : ?level:Logs.level -> ?header:string ->
    ?tags:Logs.Tag.set -> use:(unit -> 'a Lwt.t) ->
    ('a, [`Msg of string]) result Lwt.t -> 'a Lwt.t
end

let src_log src =
  let module Log = struct
    let msg level msgf = msg ~src level msgf
    let kmsg ?over k level msgf = kmsg k ~src level msgf
    let app msgf = msg Logs.App msgf
    let err msgf = msg Logs.Error msgf
    let warn msgf = msg Logs.Warning msgf
    let info msgf = msg Logs.Info msgf
    let debug msgf = msg Logs.Debug msgf
    let on_error ?level ?header ?tags ~pp ~use =
      on_error ~src ?level ?header ?tags ~pp ~use

    let on_error_msg ?level ?header ?tags ~use =
      on_error_msg ~src ?level ?header ?tags ~use
  end
  in
  (module Log : LOG)
