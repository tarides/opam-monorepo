(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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

let src = Logs.Src.create "tar.lwt" ~doc:"Tar lwt"
module Log = (val Logs.src_log src : Logs.LOG)

open Tar.Syntax

type decode_error = [
  | `Fatal of Tar.error
  | `Unix of Unix.error * string * string
  | `Unexpected_end_of_file
  | `Msg of string
]

let pp_decode_error ppf = function
  | `Fatal err -> Tar.pp_error ppf err
  | `Unix (err, fname, arg) ->
    Format.fprintf ppf "Unix error %s (function %s, arg %s)"
      (Unix.error_message err) fname arg
  | `Unexpected_end_of_file ->
    Format.fprintf ppf "Unexpected end of file"
  | `Msg msg ->
    Format.fprintf ppf "Error %s" msg

let safe f a =
  let open Lwt.Infix in
  Lwt.catch
    (fun () -> f a >|= fun r -> Ok r)
    (function
      | Unix.Unix_error (e, f, a) -> Lwt.return (Error (`Unix (e, f, a)))
      | e -> Lwt.reraise e)

let read_complete fd buf len =
  let open Lwt_result.Infix in
  let rec loop offset =
    if offset < len then
      safe (Lwt_unix.read fd buf offset) (len - offset) >>= fun read ->
      if read = 0 then
        Lwt.return (Error `Unexpected_end_of_file)
      else
        loop (offset + read)
    else
      Lwt.return (Ok ())
  in
  loop 0

let seek fd n =
  safe (Lwt_unix.LargeFile.lseek fd n) Unix.SEEK_CUR
  |> Lwt_result.map ignore

let safe_close fd =
  Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)

module High : sig
  type t
  type 'a s = 'a Lwt.t

    external inj : 'a s -> ('a, t) Tar.io = "%identity"
    external prj : ('a, t) Tar.io -> 'a s = "%identity"
end = struct
  type t
  type 'a s = 'a Lwt.t

  external inj : 'a -> 'b = "%identity"
  external prj : 'a -> 'b = "%identity"
end

type t = High.t

let value v = Tar.High (High.inj v)

let run t fd =
  let open Lwt_result.Infix in
  let rec run : type a. (a, [> decode_error ] as 'err, t) Tar.t -> (a, 'err) result Lwt.t = function
    | Tar.Write str ->
      safe (Lwt_unix.write_string fd str 0) (String.length str) >>= fun _write ->
      Lwt_result.return ()
    | Tar.Read len ->
      if Int64.of_int Sys.max_string_length < len then
        Lwt_result.fail (`Msg "read with a too big parameter")
      else
        let len = Int64.to_int len in
        let b = Bytes.make len '\000' in
        safe (Lwt_unix.read fd b 0) len >>= fun read ->
        if read = 0 then
          Lwt_result.fail `Unexpected_end_of_file
        else if len = read then
          Lwt_result.return (Bytes.unsafe_to_string b)
        else
          Lwt_result.return (Bytes.sub_string b 0 read)
    | Tar.Really_read len ->
      if Int64.of_int Sys.max_string_length < len then
        Lwt_result.fail (`Msg "really read with a too big parameter")
      else
        let len = Int64.to_int len in
        let buf = Bytes.make len '\000' in
        read_complete fd buf len >|= fun () ->
        Bytes.unsafe_to_string buf
    | Tar.Seek len -> seek fd len
    | Tar.Return value -> Lwt.return value
    | Tar.High value -> High.prj value
    | Tar.Bind (x, f) ->
      run x >>= fun value -> run (f value) in
  run t

let fold f filename init =
  let open Lwt_result.Infix in
  safe Lwt_unix.(openfile filename [ O_RDONLY ]) 0 >>= fun fd ->
  Lwt.finalize
    (fun () -> run (Tar.fold f init) fd)
    (fun () -> safe_close fd)

let unix_err_to_msg = function
  | `Unix (e, f, s) ->
    `Msg (Format.sprintf "error %s in function %s %s"
            (Unix.error_message e) f s)

let copy ~dst_fd len =
  let blen = 65536 in
  let rec read_write ~dst_fd len =
    if len = 0L then value (Lwt.return (Ok ()))
    else
      let slen = min (Int64.of_int blen) len in
      (* safe, since capped to 65536 *)
      let slen' = Int64.to_int slen in
      let* str = Tar.really_read slen in
      let* _written = Lwt_result.map_error unix_err_to_msg
        (safe (Lwt_unix.write_string dst_fd str 0) slen') |> value in
      read_write ~dst_fd (Int64.sub len slen)
  in
  read_write ~dst_fd len

let extract ?(filter = fun _ -> true) ~src dst =
  let dst = Fpath.v dst in
  let safe_close fd =
    let open Lwt.Infix in
    Lwt.catch
      (fun () -> Lwt_unix.close fd)
      (fun _ -> Lwt.return_unit)
    >|= Result.ok in
  let f ?global:_ hdr () =
    match hdr.Tar.Header.link_indicator, Fpath.of_string hdr.file_name with
    | Tar.Header.Link.Normal, Ok file_name ->
      let file_name = Fpath.normalize file_name in
      let path = Fpath.append dst file_name in
      if Fpath.is_rooted ~root:dst path then
        if filter { hdr with file_name = Fpath.to_string file_name } then
          let* dst = Lwt_result.map_error
              unix_err_to_msg
              (safe Lwt_unix.(openfile (Fpath.to_string path) [ O_WRONLY; O_CREAT; O_EXCL ]) hdr.file_mode)
            |> value in
          begin try
              let* () = copy ~dst_fd:dst hdr.Tar.Header.file_size in
              let* () = value (safe_close dst) in
              Tar.return (Ok ())
            with exn ->
              let* () = value (safe_close dst) in
              Tar.return (Error (`Exn exn))
          end
        else
          Tar.seek hdr.file_size
      else
        begin
          Log.warn (fun m -> m "ignoring %a due to escaping path %a"
                       Fpath.pp path Fpath.pp dst);
          Tar.seek hdr.file_size
        end
    | _, Ok _ ->
      Log.warn (fun m -> m "ignoring %S (unknown link indicator (%s))"
                   hdr.file_name (Tar.Header.Link.to_string hdr.link_indicator));
      Tar.seek hdr.file_size
    | _, Error `Msg msg ->
      Log.warn (fun m -> m "ignoring %S - couldn't convert to a fpath %s"
                   hdr.file_name msg);
      Tar.seek hdr.file_size
  in
  fold f src ()

(** Return the header needed for a particular file on disk *)
let header_of_file ?level file =
  let open Lwt_result.Infix in
  let level = Tar.Header.compatibility level in
  safe Lwt_unix.LargeFile.stat file >>= fun stat ->
  let file_mode = stat.Lwt_unix.LargeFile.st_perm in
  let user_id = stat.Lwt_unix.LargeFile.st_uid in
  let group_id = stat.Lwt_unix.LargeFile.st_gid in
  let file_size = stat.Lwt_unix.LargeFile.st_size in
  let mod_time = Int64.of_float stat.Lwt_unix.LargeFile.st_mtime in
  let link_indicator = Tar.Header.Link.Normal in
  let link_name = "" in
  (if level = V7 then
     Lwt.return (Ok "")
   else
     Lwt.catch
       (fun () -> safe Lwt_unix.getpwuid stat.Lwt_unix.LargeFile.st_uid)
       (function
         | Not_found ->
           Lwt.return (Error (`Msg ("No user entry found for UID")))
         | e -> Lwt.reraise e) >|= fun pwent ->
     pwent.Lwt_unix.pw_name) >>= fun uname ->
  (if level = V7 then
     Lwt.return (Ok "")
   else
     Lwt.catch
       (fun () -> safe Lwt_unix.getgrgid stat.Lwt_unix.LargeFile.st_gid)
       (function
         | Not_found ->
           Lwt.return (Error (`Msg ("No group entry found for GID")))
         | e -> Lwt.reraise e) >|= fun grent ->
     grent.Lwt_unix.gr_name) >>= fun gname ->
  let devmajor = if level = Ustar then stat.Lwt_unix.LargeFile.st_dev else 0 in
  let devminor = if level = Ustar then stat.Lwt_unix.LargeFile.st_rdev else 0 in
  let hdr = Tar.Header.make ~file_mode ~user_id ~group_id ~mod_time ~link_indicator ~link_name
      ~uname ~gname ~devmajor ~devminor file file_size
  in
  Lwt.return (Ok hdr)

let write_strings fd datas =
  let open Lwt_result.Infix in
  Lwt_list.fold_left_s (fun acc d ->
      Lwt_result.lift acc >>= fun _written ->
      Lwt_result.map_error unix_err_to_msg
        (safe (Lwt_unix.write_string fd d 0) (String.length d)))
    (Ok 0) datas >|= fun _written ->
  ()

let write_header ?level header fd =
  let open Lwt_result.Infix in
  Lwt_result.lift (Tar.encode_header ?level header) >>= fun header_strings ->
  write_strings fd header_strings

let copy ~src_fd ~dst_fd len =
  let open Lwt_result.Infix in
  let blen = 65536 in
  let buffer = Bytes.make blen '\000' in
  let rec read_write ~src_fd ~dst_fd len =
    if len = 0 then
      Lwt.return (Ok ())
    else
      let l = min blen len in
      Lwt_result.map_error
        (function
          | `Unix _ as e -> unix_err_to_msg e
          | `Unexpected_end_of_file ->
            `Msg "Unexpected end of file")
        (read_complete src_fd buffer l) >>= fun () ->
      Lwt_result.map_error unix_err_to_msg
        (safe (Lwt_unix.write dst_fd buffer 0) l) >>= fun _written ->
      read_write ~src_fd ~dst_fd (len - l)
  in
  read_write ~src_fd ~dst_fd len

let append_file ?level ?header filename fd =
  let open Lwt_result.Infix in
  (match header with
   | None -> header_of_file ?level filename
   | Some x -> Lwt.return (Ok x)) >>= fun header ->
  write_header ?level header fd >>= fun () ->
  Lwt_result.map_error unix_err_to_msg
    (safe Lwt_unix.(openfile filename [ O_RDONLY ]) 0) >>= fun src ->
  (* TOCTOU [also, header may not be valid for file] *)
  Lwt.finalize
    (fun () -> copy ~src_fd:src ~dst_fd:fd
        (Int64.to_int header.Tar.Header.file_size))
    (fun () -> safe_close src)

let write_global_extended_header ?level header fd =
  let open Lwt_result.Infix in
  Lwt_result.lift (Tar.encode_global_extended_header ?level header) >>= fun header_strings ->
  write_strings fd header_strings

let write_end fd =
  write_strings fd [ Tar.Header.zero_block ; Tar.Header.zero_block ]

let create ?level ?global ?(filter = fun _ -> true) ~src dst =
  let open Lwt_result.Infix in
  Lwt_result.map_error unix_err_to_msg
    (safe Lwt_unix.(openfile dst [ O_WRONLY ; O_CREAT ]) 0o644) >>= fun dst_fd ->
  Lwt.finalize
    (fun () ->
       (match global with
        | None -> Lwt.return (Ok ())
        | Some hdr -> write_global_extended_header ?level hdr dst_fd) >>= fun () ->
       let rec copy_files directory =
         safe Lwt_unix.opendir directory >>= fun dir ->
         Lwt.finalize
           (fun () ->
              let rec next () =
                try
                  safe Lwt_unix.readdir dir >>= fun name ->
                  let filename = Filename.concat directory name in
                  header_of_file ?level filename >>= fun header ->
                  if filter header then
                    match header.Tar.Header.link_indicator with
                    | Normal ->
                      append_file ?level ~header filename dst_fd >>= fun () ->
                      next ()
                    | Directory ->
                      (* TODO first finish curdir (and close the dir fd), then go deeper *)
                      copy_files filename >>= fun () ->
                      next ()
                    | _ -> Lwt.return (Ok ()) (* NYI *)
                  else Lwt.return (Ok ())
                with End_of_file -> Lwt.return (Ok ())
              in
              next ())
           (fun () ->
              Lwt.catch
                (fun () -> Lwt_unix.closedir dir)
                (fun _ -> Lwt.return_unit))
       in
       copy_files src >>= fun () ->
       write_end dst_fd)
    (fun () -> safe_close dst_fd)
