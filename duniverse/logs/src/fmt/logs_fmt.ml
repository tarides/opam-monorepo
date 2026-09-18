(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let app_style = `Cyan
let err_style = `Red
let warn_style = `Yellow
let info_style = `Blue
let debug_style = `Green

let pp_brackets pp_v ppf v = Fmt.char ppf '['; pp_v ppf v; Fmt.char ppf ']'

let pp_header ~pp_h ppf (l, h) = match l with
| Logs.App ->
    begin match h with
    | None -> ()
    | Some h ->
        pp_brackets Fmt.(styled app_style string) ppf h; Fmt.char ppf ' '
    end
| Logs.Error ->
    pp_h ppf err_style (match h with None -> "ERROR" | Some h -> h)
| Logs.Warning ->
    pp_h ppf warn_style (match h with None -> "WARNING" | Some h -> h)
| Logs.Info ->
    pp_h ppf info_style (match h with None -> "INFO" | Some h -> h)
| Logs.Debug ->
    pp_h ppf debug_style (match h with None -> "DEBUG" | Some h -> h)

let pp_exec_header =
  let exec = match Array.length Sys.argv with
  | 0 -> Filename.basename Sys.executable_name
  | n -> Filename.basename Sys.argv.(0)
  in
  let pp_h ppf style h =
    Fmt.string ppf exec;
    Fmt.string ppf ": ";
    pp_brackets Fmt.(styled style string) ppf h;
    Fmt.char ppf ' ';
  in
  pp_header ~pp_h

let reporter ?(pp_header = pp_exec_header) ?app ?dst () =
  Logs.format_reporter ~pp_header ?app ?dst ()

let pp_header =
  let pp_h ppf style h = pp_brackets Fmt.(styled style string) ppf h in
  pp_header ~pp_h
