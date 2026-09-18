(*
 * Copyright (C) 2022 Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Tar.Syntax

external ba_get_int32_ne : De.bigstring -> int -> int32 = "%caml_bigstring_get32"
external ba_set_int32_ne : De.bigstring -> int -> int32 -> unit = "%caml_bigstring_set32"

let bigstring_blit_string src ~src_off dst ~dst_off ~len =
  let len0 = len land 3 in
  let len1 = len asr 2 in
  for i = 0 to len1 - 1 do
    let i = i * 4 in
    (* TODO: use String.get_int32_ne when ocaml-tar requires OCaml >= 4.13 *)
    let v = Bytes.get_int32_ne (Bytes.unsafe_of_string src) (src_off + i) in
    ba_set_int32_ne dst (dst_off + i) v
  done;
  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = String.get src (src_off + i) in
    Bigarray.Array1.set dst (dst_off + i) v
  done

let bigstring_blit_bytes src ~src_off dst ~dst_off ~len =
  let len0 = len land 3 in
  let len1 = len asr 2 in
  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = ba_get_int32_ne src (src_off + i) in
    Bytes.set_int32_ne dst (dst_off + i) v
  done;
  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = Bigarray.Array1.get src (src_off + i) in
    Bytes.set dst (dst_off + i) v
  done

type decoder =
  { mutable gz : Gz.Inf.decoder
  ; ic_buffer : De.bigstring
  ; oc_buffer : De.bigstring
  ; tp_length : int
  ; mutable pos : int }

let read_through_gz
  : decoder -> bytes -> (int, 'err, _) Tar.t
  = fun ({ ic_buffer; oc_buffer; tp_length; _ } as state) res ->
    let rec until_full_or_end gz (res, res_off, res_len) =
      match Gz.Inf.decode gz with
      | `Flush gz ->
        let max = De.bigstring_length oc_buffer - Gz.Inf.dst_rem gz in
        let len = min res_len max in
        bigstring_blit_bytes oc_buffer ~src_off:0 res ~dst_off:res_off ~len;
        if len < max
        then ( state.pos <- len
             ; state.gz <- gz
             ; Tar.return (Ok (res_off + len)) )
        else until_full_or_end (Gz.Inf.flush gz) (res, res_off + len, res_len - len)
      | `End gz ->
        let max = De.bigstring_length oc_buffer - Gz.Inf.dst_rem gz in
        let len = min res_len max in
        bigstring_blit_bytes oc_buffer ~src_off:0 res ~dst_off:res_off ~len;
        state.pos <- len;
        state.gz <- gz;
        Tar.return (Ok (res_off + len))
      | `Await gz ->
        let* tp_buffer = Tar.read (Int64.of_int tp_length) in
        let len = String.length tp_buffer in
        bigstring_blit_string tp_buffer ~src_off:0 ic_buffer ~dst_off:0 ~len;
        let gz = Gz.Inf.src gz ic_buffer 0 len in
        until_full_or_end gz (res, res_off, res_len)
      | `Malformed err -> Tar.return (Error (`Gz err)) in
    let max = (De.bigstring_length oc_buffer - Gz.Inf.dst_rem state.gz) - state.pos in
    let len = min (Bytes.length res) max in
    bigstring_blit_bytes oc_buffer ~src_off:state.pos res ~dst_off:0 ~len;
    if len < max
    then ( state.pos <- state.pos + len
         ; Tar.return (Ok len) )
    else until_full_or_end (Gz.Inf.flush state.gz) (res, len, Bytes.length res - len)

let really_read_through_gz decoder len =
  let res = Bytes.create len in
  let* len = read_through_gz decoder res in
  if Bytes.length res = len
  then Tar.return (Ok (Bytes.unsafe_to_string res))
  else Tar.return (Error `Eof)

let read_through_gz decoder len =
  let res = Bytes.create len in
  let* len = read_through_gz decoder res in
  let str = Bytes.sub_string res 0 len in
  Tar.return (Ok str)

type error = [ `Fatal of Tar.error | `Eof | `Gz of string ]

let seek_through_gz
  : decoder -> int -> (unit, [> error ], _) Tar.t
  = fun state len ->
  let* _buf = really_read_through_gz state len in
  Tar.return (Ok ())

let in_gzipped t =
  let rec go
    : type a. decoder -> (a, [> error ] as 'err, 't) Tar.t -> (a, 'err, 't) Tar.t
    = fun decoder -> function
    | Tar.Really_read len ->
      if Int64.of_int max_int < len then
        Tar.return (Error (`Gz "really read gz exceeds integer representation"))
      else
        really_read_through_gz decoder (Int64.to_int len)
    | Tar.Read len ->
      if Int64.of_int max_int < len then
        Tar.return (Error (`Gz "read gz exceeds integer representation"))
      else
        read_through_gz decoder (Int64.to_int len)
    | Tar.Seek len ->
      if Int64.of_int max_int < len then
        Tar.return (Error (`Gz "seek gz exceeds integer representation"))
      else
        seek_through_gz decoder (Int64.to_int len)
    | Tar.Return _ as ret -> ret
    | Tar.Bind (x, f) ->
      Tar.Bind (go decoder x, (fun x -> go decoder (f x)))
    | Tar.High _ as high -> high
    | Tar.Write _ as v -> v in
  let decoder =
    let oc_buffer = De.bigstring_create 0x1000 in
    { gz= Gz.Inf.decoder `Manual ~o:oc_buffer
    ; oc_buffer
    ; ic_buffer= De.bigstring_create 0x1000
    ; tp_length= 0x1000
    ; pos= 0 } in
  go decoder t

type encoder =
  { mutable state : [ `Await of Gz.Def.encoder ]
  ; ic_buffer : De.bigstring
  ; oc_buffer : De.bigstring }

let ( let* ) x f = Tar.Bind (x, f)

let rec until_await oc_pos oc_buffer = function
  | `Flush gz as state ->
    let max = De.bigstring_length oc_buffer - Gz.Def.dst_rem gz - oc_pos in
    let len = min 0x100 max in
    let res = Bytes.create len in
    bigstring_blit_bytes oc_buffer ~src_off:oc_pos res ~dst_off:0 ~len;
    let* () = Tar.write (Bytes.unsafe_to_string res) in
    if len > 0 then until_await (oc_pos + len) oc_buffer state
    else
      Gz.Def.dst gz oc_buffer 0 (De.bigstring_length oc_buffer)
      |> Gz.Def.encode
      |> until_await 0 oc_buffer
  | `Await gz -> Tar.return (Ok (`Await gz))
  | `End _ -> assert false

let rec until_end oc_pos oc_buffer = function
  | `Await _ -> assert false
  | (`Flush gz | `End gz) as state ->
    let max = De.bigstring_length oc_buffer - Gz.Def.dst_rem gz - oc_pos in
    let len = min 0x100 max in
    let res = Bytes.create len in
    bigstring_blit_bytes oc_buffer ~src_off:oc_pos res ~dst_off:0 ~len;
    let* () = Tar.write (Bytes.unsafe_to_string res) in
    if len > 0 then until_end (oc_pos + len) oc_buffer state
    else match state with
    | `End _ -> Tar.return (Ok ())
    | `Flush gz ->
      Gz.Def.dst gz oc_buffer 0 (De.bigstring_length oc_buffer)
      |> Gz.Def.encode
      |> until_end 0 oc_buffer

let write_gz ({ state; ic_buffer; oc_buffer; } as encoder) str =
  let rec go (str, str_off, str_len) state =
    if str_len = 0
    then Tar.return (Ok state)
    else begin
      let len = min str_len (De.bigstring_length ic_buffer) in
      bigstring_blit_string str ~src_off:str_off ic_buffer ~dst_off:0 ~len;
      let `Await gz = state in
      let gz = Gz.Def.src gz ic_buffer 0 len in
      let* state = until_await 0 oc_buffer (Gz.Def.encode gz) in
      go (str, str_off + len, str_len - len) state
    end in
  let* state = go (str, 0, String.length str) state in
  encoder.state <- state;
  Tar.return (Ok ())

let out_gzipped ~level ~mtime os t =
  let rec go
    : type a. encoder -> (a, 'err, 't) Tar.t -> (a, 'err, 't) Tar.t
    = fun encoder -> function
    | Tar.Really_read _ as ret -> ret
    | Tar.Read _ as ret -> ret
    | Tar.Seek _ as ret -> ret
    | Tar.Return _ as ret -> ret
    | Tar.Bind (x, f) ->
      Tar.Bind (go encoder x, (fun x -> go encoder (f x)))
    | Tar.High _ as high -> high
    | Tar.Write str -> write_gz encoder str in
  let ic_buffer = De.bigstring_create 0x1000 in
  let oc_buffer = De.bigstring_create 0x1000 in
  let q = De.Queue.create 4096 in
  let w = De.Lz77.make_window ~bits:15 in
  let gz = Gz.Def.encoder `Manual `Manual ~q ~w ~level ~mtime os in
  let gz = Gz.Def.dst gz oc_buffer 0 (De.bigstring_length oc_buffer) in
  let* state = until_await 0 oc_buffer (Gz.Def.encode gz) in
  let encoder =
    { state
    ; ic_buffer
    ; oc_buffer } in
  let* result = go encoder t in
  let `Await gz = encoder.state in
  let* () =
    Gz.Def.src gz ic_buffer 0 0
    |> Gz.Def.encode
    |> until_end 0 oc_buffer  in
  Tar.return (Ok result)
