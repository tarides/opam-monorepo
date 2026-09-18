(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Console reporter *)

open Jsoo_runtime

let console_obj = Js.pure_js_expr "console"
let console : Logs.level -> string -> unit =
fun level s ->
  let meth = match level with
  | Logs.Error -> "error"
  | Logs.Warning -> "warn"
  | Logs.Info -> "info"
  | Logs.Debug -> "debug"
  | Logs.App -> "log"
  in
  ignore (Js.meth_call console_obj meth [| Js.string s |])

let ppf, flush =
  let b = Buffer.create 255 in
  let flush () = let s = Buffer.contents b in Buffer.clear b; s in
  Format.formatter_of_buffer b, flush

let console_report src level ~over k msgf =
  let k _ = console level (flush ()); over (); k () in
  msgf @@ fun ?header ?tags fmt ->
  match header with
  | None -> Format.kfprintf k ppf ("@[" ^^ fmt ^^ "@]@.")
  | Some h -> Format.kfprintf k ppf ("[%s] @[" ^^ fmt ^^ "@]@.") h

let console_reporter () = { Logs.report = console_report }
