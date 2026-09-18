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

let () = Printexc.record_backtrace true

let ( / ) = Filename.concat

let contents_of_path path =
  let fd = ref `None in
  let buf = Bytes.create 0x100 in
  let rec dispenser () = match !fd with
  | `Closed -> Tar.return (Ok None)
  | `None ->
    let fd' = Unix.openfile path Unix.[ O_RDONLY; O_CLOEXEC ] 0o644 in
    fd := `Active fd';
    dispenser ()
  | `Active fd' ->
    match Unix.read fd' buf 0 (Bytes.length buf) with
    | 0 | exception End_of_file ->
      Unix.close fd'; fd := `Closed; Tar.return (Ok None)
    | len ->
      let str = Bytes.sub_string buf 0 len in
      Tar.return (Ok (Some str)) in
  dispenser

let to_stream lst =
  let lst = ref lst in
  fun () -> match !lst with
  | [] -> None
  | x :: r -> lst := r; Some x

let create_tarball directory fd =
  let files = Sys.readdir directory in
  let os = match Sys.os_type with
    | "Win32" -> Gz.NTFS (* XXX(dinosaure): true? *)
    | "Unix" | "Cygwin" | _ -> Gz.Unix in
  let mtime = Unix.gettimeofday () in
  let dir_hdr = Tar.Header.make ~file_mode:0o755
    ~mod_time:(Int64.of_float mtime) (Filename.concat directory "") 0L in
  let dir_entry = (None, dir_hdr, (fun () -> Tar.return (Ok None))) in
  let entries = Array.fold_left begin fun acc filename ->
    let stat        = Unix.LargeFile.stat (directory / filename) in
    match stat.st_kind with
    | Unix.S_REG ->
      let file_mode = if stat.st_perm land 0o111 <> 0 then 0o755 else 0o644 in
      let mod_time  = Int64.of_float stat.Unix.LargeFile.st_mtime in
      let user_id   = stat.st_uid in
      let group_id  = stat.st_gid in
      let level     = Some Tar.Header.Ustar in
      let hdr       = Tar.Header.make
        ~file_mode ~mod_time ~user_id ~group_id
        (directory / filename) stat.st_size in
      (level, hdr, contents_of_path (directory / filename)) :: acc
    | _ -> acc end [] files in
  let entries = to_stream (dir_entry :: entries) in
  let entries () = Tar.return (Ok (entries ())) in
  let t = Tar.out ~level:Tar.Header.Ustar entries in
  let t = Tar_gz.out_gzipped ~level:4 ~mtime:(Int32.of_float mtime) os t in
  match Tar_unix.run t fd with
  | Ok () -> ()
  | Error err ->
    Format.eprintf "%s: %a\n%!" Sys.executable_name Tar_unix.pp_error err

let make directory oc =
  let fd, fd_close = match oc with
    | None -> Unix.stdout, ignore
    | Some filename ->
      let fd = Unix.openfile filename Unix.[ O_TRUNC; O_CREAT; O_WRONLY; O_CLOEXEC ] 0o644 in
      fd, (fun () -> Unix.close fd) in
  Fun.protect ~finally:fd_close @@ fun () ->
  create_tarball directory fd

let sizes = [| "B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"; "ZiB"; "YiB" |]

let bytes_to_size ?(decimals = 2) ppf = function
  | 0L -> Format.fprintf ppf "0 byte"
  | n ->
      let n = Int64.to_float n in
      let i = Float.floor (Float.log n /. Float.log 1024.) in
      let r = n /. Float.pow 1024. i in
      Format.fprintf ppf "%.*f %s" decimals r sizes.(int_of_float i)

let list filename =
  let go ?global:_ hdr () =
    Format.printf "%s (%s, %a)\n%!"
      hdr.Tar.Header.file_name
      (Tar.Header.Link.to_string hdr.link_indicator)
      (bytes_to_size ~decimals:2) hdr.Tar.Header.file_size ;
    let* () = Tar.seek hdr.Tar.Header.file_size in
    Tar.return (Ok ())
  in
  let fd = Unix.openfile filename [ Unix.O_RDONLY ] 0 in
  match Tar_unix.run (Tar_gz.in_gzipped (Tar.fold go ())) fd with
  | Ok () -> ()
  | Error (`Unix _) ->
    Format.eprintf "Some UNIX error occurred.\n%!"
  | Error (`Msg e) ->
    Format.eprintf "Some error: %s.\n%!" e
  | Error (`Unexpected_end_of_file | `Eof) ->
    Format.eprintf "Unexpected end of file.\n%!"
  | Error `Gz err ->
    Format.eprintf "Some Gzip error occurred: %s.\n%!" err
  | Error (`Fatal _) ->
    Format.eprintf "Some fatal error occurred.\n%!"

let () = match Sys.argv with
  | [| _; "list"; filename; |] when Sys.file_exists filename ->
    list filename
  | [| _; directory |] when Sys.is_directory directory ->
    make directory None
  | [| _; directory; output |] when Sys.is_directory directory ->
    make directory (Some output)
  | _ ->
    let cmd = Filename.basename Sys.argv.(0) in
    Format.eprintf "%s <directory> [<filename.tar.gz>]\n%s list <filename.tar.gz>\n" cmd cmd
