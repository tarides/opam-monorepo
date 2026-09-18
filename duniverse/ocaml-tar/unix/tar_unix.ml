(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 * Copyright (C)      2012 Thomas Gazagnaire <thomas@ocamlpro.com>
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

let src = Logs.Src.create "tar.unix" ~doc:"Tar unix"
module Log = (val Logs.src_log src : Logs.LOG)

let ( let* ) = Result.bind

let rec safe f a =
  try Ok (f a) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> safe f a
  | Unix.Unix_error (e, f, s) -> Error (`Unix (e, f, s))

let safe_close fd =
  try Unix.close fd with _ -> ()

let read_complete fd buf len =
  let rec loop offset =
    if offset < len then
      let* n = safe (Unix.read fd buf offset) (len - offset) in
      if n = 0 then
        Error `Unexpected_end_of_file
      else
        loop (offset + n)
    else
      Ok ()
  in
  loop 0

let seek fd n =
  safe (Unix.LargeFile.lseek fd n) Unix.SEEK_CUR
  |> Result.map ignore

type error = [
  | `Fatal of Tar.error
  | `Unix of Unix.error * string * string
  | `Unexpected_end_of_file
  | `Msg of string
]

let pp_error ppf = function
  | `Fatal err -> Tar.pp_error ppf err
  | `Unix (err, fname, arg) ->
    Format.fprintf ppf "Unix error %s (function %s, arg %s)"
      (Unix.error_message err) fname arg
  | `Unexpected_end_of_file ->
    Format.fprintf ppf "Unexpected end of file"
  | `Msg msg ->
    Format.fprintf ppf "Error %s" msg

(* XXX(dinosaure): This is a trick to pass from a value ['a] to a value
   [('a, High.t) Tar.io]. It may seem that the code is "unsafe" but physically
   the value remains the same (we mainly want to decorate the type of our value
   with new information). For more information on this trick, it is well
   described in this research paper:

   https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf
*)
module High : sig
  type t
  type 'a s = 'a

  external inj : 'a s -> ('a, t) Tar.io = "%identity"
  external prj : ('a, t) Tar.io -> 'a s = "%identity"
end = struct
  type t
  type 'a s = 'a

  external inj : 'a -> 'b = "%identity"
  external prj : 'a -> 'b = "%identity"
end

type t = High.t

let value v = Tar.High (High.inj v)

let run t fd =
  let rec run : type a. (a, _ as 'err, t) Tar.t -> (a, 'err) result = function
    | Tar.Write str ->
      let* _write = safe (Unix.write_substring fd str 0) (String.length str) in
      Ok ()
    | Tar.Read len ->
      if Int64.of_int Sys.max_string_length < len then
        Error (`Msg "read with a too big parameter")
      else
        let len = Int64.to_int len in
        let b = Bytes.make len '\000' in
        let* read = safe (Unix.read fd b 0) len in
        if read = 0 then
          Error `Unexpected_end_of_file
        else if len = read then
          Ok (Bytes.unsafe_to_string b)
        else
          Ok (Bytes.sub_string b 0 read)
    | Tar.Really_read len ->
      if Int64.of_int Sys.max_string_length < len then
        Error (`Msg "read with a too big parameter")
      else
        let len = Int64.to_int len in
        let buf = Bytes.make len '\000' in
        begin match read_complete fd buf len with
          | Ok () -> Ok (Bytes.unsafe_to_string buf)
          | Error _ as err -> err end
    | Tar.Seek len -> seek fd len
    | Tar.Return value -> value
    | Tar.High value -> High.prj value
    | Tar.Bind (x, f) ->
      match run x with
      | Ok value -> run (f value)
      | Error _ as err -> err in
  run t

let fold f filename init =
  let* fd = safe Unix.(openfile filename [ O_RDONLY ]) 0 in
  Fun.protect
    ~finally:(fun () -> safe_close fd)
    (fun () -> run (Tar.fold f init) fd)

let unix_err_to_msg = function
  | `Unix (e, f, s) ->
    `Msg (Format.sprintf "error %s in function %s %s"
            (Unix.error_message e) f s)

let copy ~dst_fd len =
  let blen = 65536L in
  let rec read_write ~dst_fd len =
    let open Tar.Syntax in
    if len = 0L then Tar.return (Ok ())
    else
      let slen = min blen len in
      let* str = Tar.really_read slen in
      (* this is safe, since blen is 65536L and there's Int64.min above *)
      safe (Unix.write_substring dst_fd str 0) (Int64.to_int slen)
      |> Result.map_error unix_err_to_msg
      |> function
      | Ok _ -> read_write ~dst_fd (Int64.sub len slen)
      | Error _ as err -> Tar.return err
  in
  read_write ~dst_fd len

let extract ?(filter = fun _ -> true) ~src dst =
  let dst = Fpath.v dst in
  let f ?global:_ hdr () =
    match Fpath.of_string hdr.Tar.Header.file_name with
    | Error `Msg msg ->
      Log.warn (fun m -> m "ignoring %S - couldn't convert to a fpath %s"
                   hdr.file_name msg);
      Tar.seek hdr.file_size
    | Ok file_name ->
      let file_name = Fpath.normalize file_name in
      let path = Fpath.append dst file_name in
      if Fpath.is_rooted ~root:dst path then
        if filter { hdr with file_name = Fpath.to_string file_name } then
          match hdr.link_indicator with
          | Tar.Header.Link.Normal ->
            begin match Result.map_error unix_err_to_msg
                          (safe Unix.(openfile (Fpath.to_string path)
                                        [ O_WRONLY ; O_CREAT ; O_EXCL]) hdr.Tar.Header.file_mode) with
            | Error _ as err -> Tar.return err
            | Ok dst ->
              try copy ~dst_fd:dst hdr.Tar.Header.file_size
              with exn -> safe_close dst; Tar.return (Error (`Exn exn))
              (* TODO set owner / mode / mtime etc. *)
            end
          | l ->
            Log.warn (fun m -> m "ignoring %a due to unknown link indicator (not a normal file): %s"
                         Fpath.pp path (Tar.Header.Link.to_string l));
            (* TODO handle directories, links, etc. *)
            Tar.seek hdr.file_size
        else
          Tar.seek hdr.file_size
      else
        begin
          Log.warn (fun m -> m "ignoring %a due to escaping path %a"
                       Fpath.pp path Fpath.pp dst);
          Tar.seek hdr.file_size
        end
  in
  fold f src ()

(** Return the header needed for a particular file on disk *)
let header_of_file ?level file =
  let level = Tar.Header.compatibility level in
  let* stat = safe Unix.LargeFile.lstat file in
  let file_mode = stat.Unix.LargeFile.st_perm in
  let user_id = stat.Unix.LargeFile.st_uid in
  let group_id = stat.Unix.LargeFile.st_gid in
  let mod_time = Int64.of_float stat.Unix.LargeFile.st_mtime in
  (* TODO evaluate stat.st_kind *)
  let link_indicator = Tar.Header.Link.Normal in
  let link_name = "" in
  let* uname =
    if level = V7 then
      Ok ""
    else
      try
        let* passwd_entry = safe Unix.getpwuid stat.Unix.LargeFile.st_uid in
        Ok passwd_entry.Unix.pw_name
      with Not_found -> Error (`Msg ("No user entry found for UID"))
  in
  let devmajor = if level = Ustar then stat.Unix.LargeFile.st_dev else 0 in
  let* gname =
    if level = V7 then
      Ok ""
    else
      try
        let* passwd_entry = safe Unix.getgrgid stat.Unix.LargeFile.st_gid in
        Ok passwd_entry.Unix.gr_name
      with Not_found -> Error (`Msg "No group entry found for GID")
  in
  let devminor = if level = Ustar then stat.Unix.LargeFile.st_rdev else 0 in
  Ok (Tar.Header.make ~file_mode ~user_id ~group_id ~mod_time ~link_indicator ~link_name
        ~uname ~gname ~devmajor ~devminor file stat.Unix.LargeFile.st_size)

let write_strings fd datas =
  let* _written =
    List.fold_left (fun acc d ->
        let* _written = acc in
        Result.map_error unix_err_to_msg
          (safe (Unix.write_substring fd d 0) (String.length d)))
      (Ok 0) datas
  in
  Ok ()

let write_header ?level header fd =
  let* header_strings = Tar.encode_header ?level header in
  write_strings fd header_strings

let copy ~src_fd ~dst_fd len =
  let blen = 65536 in
  let buffer = Bytes.make blen '\000' in
  let rec read_write ~src_fd ~dst_fd len =
    if len = 0L then Ok ()
    else
      let l = min (Int64.of_int blen) len in
      (* safe, since blen is 65536 *)
      let l' = Int64.to_int l in
      let* () =
        Result.map_error
          (function
             | `Unix _ as e -> unix_err_to_msg e
             | `Unexpected_end_of_file ->
               `Msg "Unexpected end of file")
          (read_complete src_fd buffer l')
      in
      let* _written =
        Result.map_error unix_err_to_msg
          (safe (Unix.write dst_fd buffer 0) l')
      in
      read_write ~src_fd ~dst_fd (Int64.sub len l)
  in
  read_write ~src_fd ~dst_fd len

let append_file ?level ?header filename fd =
  let* header = match header with
    | None -> header_of_file ?level filename
    | Some x -> Ok x
  in
  let* () = write_header ?level header fd in
  let* src =
    Result.map_error unix_err_to_msg
      (safe Unix.(openfile filename [ O_RDONLY ]) 0)
  in
  (* TOCTOU [also, header may not be valid for file] *)
  Fun.protect ~finally:(fun () -> safe_close src)
    (fun () -> copy ~src_fd:src ~dst_fd:fd header.Tar.Header.file_size)

let write_global_extended_header ?level header fd =
  let* header_strings = Tar.encode_global_extended_header ?level header in
  write_strings fd header_strings

let write_end fd =
  write_strings fd [ Tar.Header.zero_block ; Tar.Header.zero_block ]

let create ?level ?global ?(filter = fun _ -> true) ~src dst =
  let* dst_fd =
    Result.map_error unix_err_to_msg
      (safe Unix.(openfile dst [ O_WRONLY ; O_CREAT ]) 0o644)
  in
  Fun.protect ~finally:(fun () -> safe_close dst_fd)
    (fun () ->
       let* () = match global with
         | None -> Ok ()
         | Some hdr -> write_global_extended_header ?level hdr dst_fd
       in
       let rec copy_files directory =
         let* dir = safe Unix.opendir directory in
         Fun.protect ~finally:(fun () -> try Unix.closedir dir with _ -> ())
           (fun () ->
              let rec next () =
                try
                  let* name = safe Unix.readdir dir in
                  let filename = Filename.concat directory name in
                  let* header = header_of_file ?level filename in
                  if filter header then
                    match header.Tar.Header.link_indicator with
                    | Normal ->
                      let* () = append_file ?level ~header filename dst_fd in
                      next ()
                    | Directory ->
                      (* TODO first finish curdir (and close the dir fd), then go deeper *)
                      let* () = copy_files filename in
                      next ()
                    | _ -> Ok () (* NYI *)
                  else Ok ()
                with End_of_file -> Ok ()
              in
              next ())
       in
       let* () = copy_files src in
       write_end dst_fd)
