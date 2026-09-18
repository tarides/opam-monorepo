(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

(** Unix I/O for tar-formatted data. *)

type error = [
  | `Fatal of Tar.error
  | `Unix of Unix.error * string * string
  | `Unexpected_end_of_file
  | `Msg of string
]

type t

val pp_error : Format.formatter -> error -> unit

val run : ('a, [> error ] as 'b, t) Tar.t -> Unix.file_descr -> ('a, 'b) result
val value : ('a, 'err) result -> ('a, 'err, t) Tar.t

(** [fold f filename acc] folds over the tar archive. The function [f] is called
    for each [hdr : Tar.Header.t]. It should forward the position in the file
    descriptor by [hdr.Tar.Header.file_size]. *)
val fold :
  (?global:Tar.Header.Extended.t -> Tar.Header.t -> 'a ->
   ('a, error, t) Tar.t) ->
  string -> 'a -> ('a, error) result

(** [extract ~filter ~src dst] extracts the tar archive [src] into the
    directory [dst]. If [filter] is provided (defaults to [fun _ -> true]), any
    file where [filter hdr] returns [false], is skipped. No directories are
    created including [dst] and any (implicit or explicit) directories in the
    archive. *)
val extract :
  ?filter:(Tar.Header.t -> bool) ->
  src:string -> string ->
  (unit, [> `Exn of exn | error ]) result

(** [create ~level ~filter ~src dst] creates a tar archive at [dst]. It uses
    [src], a directory name, as input. If [filter] is provided
    (defaults to [fun _ -> true]), any file where [filter hdr] returns [false]
    is skipped. *)
val create : ?level:Tar.Header.compatibility ->
  ?global:Tar.Header.Extended.t ->
  ?filter:(Tar.Header.t -> bool) ->
  src:string -> string ->
  (unit, [ `Msg of string | `Unix of (Unix.error * string * string) ]) result

(** [header_of_file ~level filename] returns the tar header of [filename]. *)
val header_of_file : ?level:Tar.Header.compatibility -> string ->
  (Tar.Header.t, [ `Msg of string | `Unix of (Unix.error * string * string) ]) result

(** [append_file ~level ~header filename fd] appends the contents of [filename]
    to the tar archive [fd]. If [header] is not provided, {header_of_file} is
    used for constructing a header. *)
val append_file : ?level:Tar.Header.compatibility -> ?header:Tar.Header.t ->
  string -> Unix.file_descr ->
  (unit, [ `Msg of string | `Unix of (Unix.error * string * string) ]) result

(** [write_header ~level hdr fd] writes the header [hdr] to [fd]. *)
val write_header : ?level:Tar.Header.compatibility ->
  Tar.Header.t -> Unix.file_descr ->
  (unit, [ `Msg of string | `Unix of (Unix.error * string * string) ]) result

(** [write_global_extended_header ~level hdr fd] writes the extended header [hdr] to
    [fd]. *)
val write_global_extended_header : ?level:Tar.Header.compatibility ->
  Tar.Header.Extended.t -> Unix.file_descr ->
  (unit, [ `Msg of string | `Unix of (Unix.error * string * string) ]) result

(** [write_end fd] writes the tar end marker to [fd]. *)
val write_end : Unix.file_descr -> (unit, [> `Msg of string ]) result
