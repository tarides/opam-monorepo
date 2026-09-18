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

(** {1 Eio Tar}

    This library provides low-level and high-level abstractions for reading and
    writing Tar files using Eio. *)

type t

type flow
(** A type for reading or writing tar files to. *)

val flow_of_two_way : _ Eio.Flow.two_way -> flow
(** Construct a {! flow} from an {! Eio.Flow.two_way}. Use {! flow_of_file} if
    your flow is really a file. *)

val flow_of_file : _ Eio.File.rw -> flow
(** Construct a {! flow} from an {! Eio.File.rw}. If you are using a file it is
    best to use this function as the [seek] operations will be more efficient.
*)

type decode_error =
  [ `Fatal of Tar.error | `Unexpected_end_of_file | `Msg of string ]
(** Possible decoding errors *)

val pp_decode_error : decode_error Fmt.t
(** A pretty-printer for decoding errors. *)

(** {2 High-level Interface} *)

val extract :
  ?filter:(Tar.Header.t -> bool) ->
  sw:Eio.Switch.t ->
  Eio.Fs.dir_ty Eio.Path.t ->
  (unit, [> `Fatal of Tar.error ], t) Tar.t
(** [extract ~sw dst] produces a {! Tar.t} that will extract to [dst]. For
    example:

    {[
    Eio.Path.with_open_out ~create:`Never foo_tar_gz @@ fun src ->
    Eio.Switch.run @@ fun sw ->
    let t = Tar_eio.extract ~sw (env#cwd / "foo.out") in
    (* optional gzip: let t = Tar_gz.in_gzipped t in *)
    Tar_eio.run t (Tar_eio.flow_of_file src)
    ]}

    will extract the file at [foo_tar_gz] into the directory at ["foo.out"].
    Note that this function only creates {b files}, {b directories} and
    {b symlinks} with the correct mode (it does not, yet, set the ownership of
    the files according to the tar file).

    All operations, in particular the {! Tar_eio.run} must be completed inside
    the scope of the {! Eio.Switch.t} [sw].

    @param filter Can be used to exclude certain entries based on their header.
*)

val create :
  ?level:Tar.Header.compatibility ->
  ?global:Tar.Header.Extended.t ->
  ?filter:(Tar.Header.t -> bool) ->
  sw:Eio.Switch.t ->
  _ Eio.Path.t ->
  (unit, [> `Msg of string ], 'b) Tar.t
(** [create ~sw dir] is the opposite of {! extract}. It returns a
    {! Tar.t} of the supplied [dir] that is only valid for the scope of the
    function.

    Common usage might be tarring a directory to a new file.

    {[
    let foo = Eio.Path.(cwd / "foo") in
    let foo_tar = Eio.Path.(cwd / "foo.tar") in
    Eio.Path.with_open_out ~create:(`Or_truncate 0o755) foo_tar @@ fun foo_tar ->
    Eio.Switch.run @@ fun sw ->
    let t = Tar_eio.create ~sw foo in
    Tar_eio.run t (Tar_eio.flow_of_file tar)
    ]}

    @param filter Can be used to exclude certain entries based on their header.
*)

val run :
  ('a, ([> `Unexpected_end_of_file | `Msg of string ] as 'b), t) Tar.t -> flow -> ('a, 'b) result
(** [run tar src] will run the given [tar] using {! Eio} on [src]. *)

val fold :
  (?global:Tar.Header.Extended.t ->
  Tar.Header.t ->
  'acc ->
  ('acc, ([> `Fatal of Tar.error | `Unexpected_end_of_file | `Msg of string ] as 'b), t) Tar.t) ->
  flow ->
  'acc ->
  ('acc, 'b) result
(** [fold f src init] folds over the tarred [src]. *)

(** {2 Low-level Interface} *)

val value : ('a, 'err) result -> ('a, 'err, t) Tar.t
(** Converts a normal result into {! Tar}s IO type *)

val append_file :
  ?level:Tar.Header.compatibility ->
  ?header:Tar.Header.t ->
  Eio.Fs.dir_ty Eio.Path.t ->
  _ Eio.Flow.sink ->
  (unit, [> decode_error ]) result
(** [append_file dst sink] *)

val header_of_file :
  ?level:Tar.Header.compatibility ->
  ?getpwuid:(int64 -> string) ->
  ?getgrgid:(int64 -> string) ->
  Eio.Fs.dir_ty Eio.Path.t ->
  Tar.Header.t
(** Return the header needed for a particular file on disk. [getpwuid] and
    [getgrgid] are optional functions that should take the uid and gid
    respectively and return the passwd and group entry names for each. These
    will be added to the header. *)

val write_header :
  ?level:Tar.Header.compatibility ->
  Tar.Header.t ->
  _ Eio.Flow.sink ->
  (unit, [> decode_error ]) result

val write_global_extended_header :
  ?level:Tar.Header.compatibility ->
  Tar.Header.Extended.t ->
  _ Eio.Flow.sink ->
  (unit, [> decode_error ]) result

val write_end : _ Eio.Flow.sink -> unit
