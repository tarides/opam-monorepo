(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
 * Copyright (C)      2012 Thomas Gazagnaire <thomas@ocamlpro.com>
 * Copyright (C)      2023 Patrick Ferris <patrick@sirref.org>
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

type decode_error =
  [ `Fatal of Tar.error | `Unexpected_end_of_file | `Msg of string ]

let pp_decode_error ppf = function
  | `Fatal err -> Tar.pp_error ppf err
  | `Unexpected_end_of_file -> Fmt.string ppf "Unexpected end of file"
  | `Msg s -> Fmt.pf ppf "Error %s" s

let ( / ) = Eio.Path.( / )
let ( let* ) = Result.bind
let ( let+ ) v f = Result.map f v

module High : sig
  type t
  type 'a s = 'a

  external inj : 'a s -> ('a, t) Tar.io = "%identity"
  external prj : ('a, t) Tar.io -> 'a s = "%identity"
end = struct
  type t
  type 'a s = 'a

  external inj : 'a -> 'b = "%identity"
  external prj : 'b -> 'a = "%identity"
end

type t = High.t

let value v = Tar.High (High.inj v)

type flow = Flow : _ Eio.Flow.two_way -> flow | File : _ Eio.File.rw -> flow

let flow_of_two_way tw = Flow tw
let flow_of_file f = File f

let flow_to_source = function
  | Flow f -> (f :> Eio.Flow.source_ty Eio.Flow.source)
  | File f -> (f :> Eio.Flow.source_ty Eio.Flow.source)

let flow_to_sink = function
  | Flow f -> (f :> Eio.Flow.sink_ty Eio.Flow.sink)
  | File f -> (f :> Eio.Flow.sink_ty Eio.Flow.sink)

let skip f n =
  let buffer_size = 32768 in
  let buffer = Cstruct.create buffer_size in
  let rec loop (n : int) =
    if n <= 0 then Ok ()
    else
      let amount = min n buffer_size in
      let block = Cstruct.sub buffer 0 amount in
      Eio.Flow.read_exact f block;
      loop (n - amount)
  in
  loop n

let run t f =
  let rec run : type a. (a, 'err, t) Tar.t -> (a, 'err) result = function
    | Tar.Write s ->
        Eio.Flow.copy_string s (flow_to_sink f);
        Ok ()
    | Tar.Read len -> (
        if Int64.of_int max_int < len then
          Error (`Msg "read exceeds maximum integer")
        else
          let len = Int64.to_int len in
          let f = flow_to_source f in
          let b = Cstruct.create len in
          match Eio.Flow.single_read f b with
          | len -> Ok (Cstruct.to_string ~len b)
          | exception End_of_file ->
            (* XXX: should we catch other exceptions?! *)
            Error `Unexpected_end_of_file)
    | Tar.Really_read len -> (
        if Int64.of_int max_int < len then
          Error (`Msg "really read exceeds maximum integer")
        else
          let len = Int64.to_int len in
          let f = flow_to_source f in
          let b = Cstruct.create len in
          try
            Eio.Flow.read_exact f b;
            Ok (Cstruct.to_string b)
          with End_of_file -> Error `Unexpected_end_of_file)
    | Tar.Seek n -> (
        (* Seek is really just skip in ocaml-tar *)
        if Int64.of_int max_int < n then
          Error (`Msg "seek exceeds maximum integer")
        else
          let n = Int64.to_int n in
          match f with
          | Flow f -> skip f n
          | File f ->
            let _set : Optint.Int63.t =
              Eio.File.seek f (Optint.Int63.of_int n) `Cur
            in
            Ok ())
    | Tar.Return value -> value
    | Tar.High value -> High.prj value
    | Tar.Bind (x, f) -> (
        match run x with Ok value -> run (f value) | Error _ as err -> err)
  in
  run t

let fold f source init = run (Tar.fold f init) source
let stat path = Eio.Path.stat ~follow:true path

(** Return the header needed for a particular file on disk *)
let header_of_file ?level ?getpwuid ?getgrgid filepath : Tar.Header.t =
  let level = Tar.Header.compatibility level in
  let stat = stat filepath in
  let pwent = Option.map (fun f -> f stat.uid) getpwuid in
  let grent = Option.map (fun f -> f stat.gid) getgrgid in
  let uname = if level = V7 then Some "" else pwent in
  let gname = if level = V7 then Some "" else grent in
  let file_mode = stat.perm in
  let user_id = stat.uid |> Int64.to_int in
  let group_id = stat.gid |> Int64.to_int in
  let file_size = stat.size |> Optint.Int63.to_int64 in
  let mod_time = Int64.of_float stat.mtime in
  let link_indicator = Tar.Header.Link.Normal in
  let link_name = "" in
  let devmajor = if level = Ustar then stat.dev |> Int64.to_int else 0 in
  let devminor = if level = Ustar then stat.rdev |> Int64.to_int else 0 in
  Tar.Header.make ~file_mode ~user_id ~group_id ~mod_time ~link_indicator
    ~link_name ?uname ?gname ~devmajor ~devminor (snd filepath) file_size

let copy dst len =
  let blen = 65536 in
  let rec read_write dst len =
    if len = 0L then value (Ok ())
    else
      let open Tar.Syntax in
      let slen = Int64.min (Int64.of_int blen) len in
      let* str = Tar.really_read slen in
      let* () = Result.ok (Eio.Flow.copy_string str dst) |> value in
      read_write dst (Int64.sub len slen)
  in
  read_write dst len

let is_safe_link_target path =
  path <> ""
  && Filename.is_relative path
  && List.for_all (fun s -> s <> "..") (String.split_on_char '/' path)

let extract ?(filter = fun _ -> true) ~sw dst =
  let dst_dir = Eio.Path.open_dir ~sw dst in
  let f ?global:_ hdr () =
    let open Tar.Syntax in
    let path = dst_dir / hdr.Tar.Header.file_name in
    let skip_body () =
      let* () = Tar.seek hdr.Tar.Header.file_size in
      Tar.return (Ok ())
    in
    if not (filter hdr) then skip_body ()
    else
      match hdr.Tar.Header.link_indicator with
      | Tar.Header.Link.Normal ->
          let dst =
            Eio.Path.open_out ~sw ~create:(`If_missing hdr.Tar.Header.file_mode)
              path
          in
          let* () = copy dst hdr.Tar.Header.file_size in
          let* () = Tar.return (Ok (Eio.Flow.close dst)) in
          Tar.return (Ok ())
      | Tar.Header.Link.Symbolic ->
          if is_safe_link_target hdr.link_name then
            Eio.Path.symlink ~link_to:hdr.link_name path;
          Tar.return (Ok ())
      | Tar.Header.Link.Directory ->
          Eio.Path.mkdirs ~exists_ok:true ~perm:hdr.file_mode path;
          Tar.return (Ok ())
      | _ -> skip_body ()
  in
  Tar.fold f ()

let write_strings fd datas =
  List.iter (fun d -> Eio.Flow.copy_string d fd) datas

let write_header ?level hdr fl =
  let+ bytes = Tar.encode_header ?level hdr in
  write_strings fl bytes

let copy src sink len =
  let blen = 65536 in
  let buf = Cstruct.create blen in
  let rec read_and_write len =
    if len = 0 then Ok ()
    else
      match Eio.Flow.single_read src buf with
      | n ->
          Eio.Flow.write sink [ Cstruct.sub buf 0 n ];
          read_and_write (len - n)
      | exception End_of_file -> Error (`Msg "Unexpected end of file")
  in
  read_and_write len

let append_file ?level ?header filename dst =
  let header =
    match header with None -> header_of_file ?level filename | Some x -> x
  in
  let* () = write_header ?level header dst in
  Eio.Path.with_open_in filename @@ fun src ->
  (* TOCTOU [also, header may not be valid for file] *)
  copy src dst (Int64.to_int header.Tar.Header.file_size)

let write_global_extended_header ?level header sink =
  Result.map (write_strings sink)
    (Tar.encode_global_extended_header ?level header)

let write_end fl =
  write_strings fl [ Tar.Header.zero_block; Tar.Header.zero_block ]

let tar_header_of_file (stat : Eio.File.Stat.t) path =
  let file_mode = stat.perm in
  let mod_time = Int64.of_float stat.mtime in
  let user_id = Int64.to_int stat.uid in
  let group_id = Int64.to_int stat.gid in
  let link_indicator =
    match stat.kind with
    | `Regular_file -> Tar.Header.Link.Normal
    | `Directory -> Directory
    | `Block_device -> Block
    | `Character_special -> Character
    | `Fifo -> FIFO
    | `Symbolic_link -> Symbolic
    | `Socket | `Unknown ->
        failwith "Cannot create a tar header for sockets or unknown file kinds"
  in
  (* Only files have a size *)
  let link_name, size =
    match link_indicator with
    | Symbolic -> (Some (Eio.Path.read_link path), 0L)
    | Normal -> (None, Optint.Int63.to_int64 stat.size)
    | _ -> (None, 0L)
  in
  (* Add a trailing slash *)
  let path =
    match link_indicator with
    | Directory -> Eio.Path.(path / "") |> Eio.Path.native_exn
    | _ -> Eio.Path.native_exn path
  in
  Tar.Header.make ~file_mode ?link_name ~mod_time ~user_id ~group_id
    ~link_indicator path size

let create ?level ?global ?(filter = fun _ -> true) ~sw src =
  let contents_of_path ~sw path =
    let fd = ref `None in
    let buf = Cstruct.create 0x100 in
    let rec dispenser () =
      match !fd with
      | `Closed -> Tar.return (Ok None)
      | `None ->
          let fd' = Eio.Path.open_in ~sw path in
          fd := `Active fd';
          dispenser ()
      | `Active fd' -> (
          match Eio.Flow.single_read fd' buf with
          | 0 | (exception End_of_file) ->
              Eio.Flow.close fd';
              fd := `Closed;
              Tar.return (Ok None)
          | len ->
              let str = Cstruct.to_string ~off:0 ~len buf in
              Tar.return (Ok (Some str)))
    in
    dispenser
  in
  let to_stream lst =
    let lst = ref lst in
    fun () ->
      match !lst with
      | [] -> None
      | x :: r ->
          lst := r;
          Some x
  in
  let files = Eio.Path.read_dir src in
  let dir_hdr = tar_header_of_file (Eio.Path.stat ~follow:false src) src in
  let dir_entry = (None, dir_hdr, fun () -> Tar.return (Ok None)) in
  let entries sw =
    let rec loop acc = function
      | [] -> List.rev acc
      | file_path :: rest -> (
          let stat = Eio.Path.stat ~follow:false file_path in
          let hdr = tar_header_of_file stat file_path in
          let skip v = if not (filter hdr) then loop acc rest else v in
          match stat.kind with
          | `Regular_file ->
              skip
              @@ loop ((level, hdr, contents_of_path ~sw file_path) :: acc) rest
          | `Directory ->
              skip
              @@
              let new_files =
                Eio.Path.read_dir file_path
                |> List.map Eio.Path.(( / ) file_path)
              in
              loop
                ((level, hdr, fun () -> Tar.return (Ok None)) :: acc)
                (new_files @ rest)
          | `Unknown | `Socket ->
              (* Skipping files without a Tar header format. *)
              loop acc rest
          | _ ->
              skip
              @@ loop ((level, hdr, fun () -> Tar.return (Ok None)) :: acc) rest
          )
    in
    loop [] ((List.map Eio.Path.(( / ) src)) files)
  in
  let entries = to_stream (dir_entry :: entries sw) in
  let entries () = Tar.return (Ok (entries ())) in
  Tar.out ?level ?global_hdr:global entries
