(*---------------------------------------------------------------------------
   Copyright (c) 2015 The bos programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Astring

(* Process environment *)

type t = string String.map

let current () =
  try
    let env = Unix.environment () in
    let add acc assign = match acc with
    | Error _ as e -> e
    | Ok m ->
        match String.cut ~sep:"=" assign with
        | Some (var, value) -> Ok (String.Map.add var value m)
        | None ->
            Fmt.error_msg
              "could not parse process environment variable (%S)" assign
    in
    Array.fold_left add (Ok String.Map.empty) env
  with
  | Unix.Unix_error (e, _, _) ->
      Fmt.error_msg
        "could not get process environment: %s" (Unix.error_message e)

let to_array env =
  let add_var name value acc = String.concat [name; "="; value] :: acc in
  Array.of_list (String.Map.fold add_var env [])

(* Variables *)

let var name = try Some (Unix.getenv name) with Not_found -> None
let set_var name v =
  let v = match v with None -> "" | Some v -> v in
  try Ok (Unix.putenv name v) with
  | Unix.Unix_error (e, _, _) ->
      Fmt.error_msg
        "set environment variable %s: %s" name (Unix.error_message e)

let opt_var name ~absent = try Unix.getenv name with Not_found -> absent
let req_var name = try Ok (Unix.getenv name) with
| Not_found -> Fmt.error_msg "environment variable %s: undefined" name

(* Typed lookup *)

type 'a parser = string -> ('a, [`Msg of string]) result

let parser kind k_of_string =
  fun s -> match k_of_string s with
  | None -> Fmt.error_msg "could not parse %s value from %a" kind String.dump s
  | Some v -> Ok v

let bool =
  let of_string s = match String.Ascii.lowercase s with
  | "" | "false" | "no" | "n" | "0" -> Some false
  | "true" | "yes" | "y" | "1" -> Some true
  | _ -> None
  in
  parser "bool" of_string

let string = fun s -> Ok s
let path = Fpath.of_string
let cmd = fun s -> match Bos_cmd.of_string s with
| Error _ as err -> err
| Ok cmd when Bos_cmd.is_empty cmd -> Error (`Msg "command line is empty")
| Ok _ as cmd -> cmd

let some p = fun s -> match p s with Ok v -> Ok (Some v) | Error _ as e -> e

let parse name p ~absent = match var name with
| None -> Ok absent
| Some s ->
    match p s with
    | Ok _ as r -> r
    | Error (`Msg e) -> Fmt.error_msg "environment variable %s: %s" name e

let value ?(log = Logs.Error) name p ~absent =
  Bos_log.on_error_msg ~level:log ~use:(fun () -> absent) (parse name p ~absent)

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The bos programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
