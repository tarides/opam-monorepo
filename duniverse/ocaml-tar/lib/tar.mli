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

(** Tar utilities

    {e v3.5.0 - {{:https://github.com/mirage/ocaml-tar }homepage}} *)

(** The type of errors that may occur. *)
type error = [
  | `Checksum_mismatch
  | `Corrupt_pax_header
  | `Zero_block
  | `Unmarshal of string
]

(** [pp_error ppf e] pretty prints the error [e] on the formatter [ppf]. *)
val pp_error : Format.formatter -> [< error] -> unit

module Header : sig
  (** Process and create tar file headers. *)

  (** tar format assumptions. Default is {!V7} (for compatibility with versions
      of ocaml-tar before this type was introduced).
      @see <https://www.gnu.org/software/tar/manual/html_section/Formats.html> *)
  type compatibility =
    | OldGNU (** GNU tar < 1.12 *)
    | GNU (** GNU tar 1.12 - 1.13.25 *)
    | V7 (** Origin 7th Release format *)
    | Ustar (** POSIX.1-1988 *)
    | Posix (** POSIX.1-2001 *)

  (** Return the compatibility level, defaults to {!V7}. *)
  val compatibility : compatibility option -> compatibility

  module Link : sig
    (** Determines the type of the file. *)
    type t =
      | Normal
      | Hard (** a hard link *)
      | Symbolic (** a symbolic link *)
      | Character (** a character device node *)
      | Block (** a block device node *)
      | Directory (** a directory (also indicated by trailing [/] in [file_name]) *)
      | FIFO (** a FIFO node *)
      | GlobalExtendedHeader (** a PaxExtension global header *)
      | PerFileExtendedHeader (** a PaxExtension per-file header *)
      | LongLink (** a GNU LongLink i.e. a very long link name *)
      | LongName (** a GNU LongName i.e. a very long filename *)
    val to_string: t -> string
  end

  module Extended: sig
    type t = {
      access_time: int64 option; (** second granularity since the Epoch *)
      charset: string option;
      comment: string option;
      group_id: int option;
      gname: string option;
      header_charset: string option;
      link_path: string option;
      mod_time: int64 option; (** second granularity since the Epoch *)
      path: string option;
      file_size: int64 option;
      user_id: int option;
      uname: string option;
    }
    (** Represents a "Pax" extended header. *)

    (** [make ()] creates an extended header. *)
    val make :
      ?access_time:int64 ->
      ?charset:string ->
      ?comment:string ->
      ?group_id:int ->
      ?gname:string ->
      ?header_charset:string ->
      ?link_path:string ->
      ?mod_time:int64 ->
      ?path:string ->
      ?file_size:int64 ->
      ?user_id:int ->
      ?uname:string ->
      unit ->
      t

    (** Pretty-print the extended header record. *)
    val to_detailed_string : t -> string

    (** Unmarshal a pax Extended Header block. This header block may
        be preceded by [global] blocks which will override some
        fields. *)
    val unmarshal : global:t option -> string -> (t, [> error ]) result
  end

  (** Represents a standard archive (note checksum not stored). *)
  type t = {
    file_name : string;
    file_mode: int;
    user_id: int;
    group_id: int;
    file_size: int64;
    mod_time: int64;
    link_indicator: Link.t;
    link_name: string;
    uname: string;
    gname: string;
    devmajor: int;
    devminor: int;
    extended: Extended.t option;
  }

  (** Helper function to make a simple header. *)

  (** [make file_name file_size] creates a simple header.
      [file_mode] defaults to [0o400], [user_id], [group_id] default to [0],
      [mod_time] defaults to [0L] (epoch), [link_indicator] defaults to [Normal],
      [link_name], [uname] and [gname] default to [""], and [devmajor] and
      [devminor] default to [0]. *)
  val make :
    ?file_mode:int ->
    ?user_id:int ->
    ?group_id:int ->
    ?mod_time:int64 ->
    ?link_indicator:Link.t ->
    ?link_name:string ->
    ?uname:string ->
    ?gname:string ->
    ?devmajor:int ->
    ?devminor:int ->
    string ->
    int64 ->
    t

  (** Length of a header block. *)
  val length : int

  (** A blank header block (two of these in series mark the end of the tar). *)
  val zero_block : string

  (** Pretty-print the header record. *)
  val to_detailed_string : t -> string

  (** Unmarshal a header block, returning [None] if it's all zeroes.
      This header block may be preceded by an [?extended] block which
      will override some fields. *)
  val unmarshal :
    ?extended:Extended.t ->
    string ->
    (t, [> `Zero_block | `Checksum_mismatch | `Unmarshal of string]) result

  (** Marshal a header block, computing and inserting the checksum. *)
  val marshal :
    ?level:compatibility ->
    bytes ->
    t ->
    (unit, [> `Msg of string ]) result

  (** Compute the amount of zero-padding required to round up the file size
      to a whole number of blocks. *)
  val compute_zero_padding_length : t -> int

  (** Return the required zero-padding as a string. *)
  val zero_padding : t -> string

  (** [to_sectors t] is the number of sectors occupied by the data. *)
  val to_sectors: t -> int64
end

(** {1 Decoding and encoding of a whole archive} *)

(** The type of the decode state. *)
type decode_state

(** [decode_state ~global ()] constructs a decode_state. *)
val decode_state : ?global:Header.Extended.t -> unit -> decode_state

(** [decode t data] decodes [data] taking the current state [t] into account.
    It may result on success in a new state, optionally some action that should
    be done ([`Read] or [`Skip]), or a decoded [`Header]. Possibly a new global
    PAX header is provided as well.

    If no [`Read] or [`Skip] is returned, the new state should be used with
    [decode] with the next [Header.length] sized string, which will lead to
    further decoding until [`Eof] (or an error) occurs. *)
val decode : decode_state -> string ->
  (decode_state * [ `Read of int64 | `Skip of int64 | `Header of Header.t ] option * Header.Extended.t option,
   [> `Eof | `Fatal of error ])
    result

(** [encode_header ~level hdr] encodes the header with the provided [level]
    (defaults to [V7]) into a list of strings to be written to the disk.
    Once a header is written, the payload (padded to multiples of
    [Header.length]) should follow. *)
val encode_header : ?level:Header.compatibility ->
  Header.t -> (string list, [> `Msg of string ]) result

(** [encode_global_extended_header hdr] encodes the global extended header as
    a list of strings. *)
val encode_global_extended_header :
  ?level:Header.compatibility ->
  Header.Extended.t ->
  (string list, [> `Msg of string ]) result

(** {1 Pure implementation of [fold].}

    [fold] produces a [('a, 'err, 't) t] value which can be {b evaluated} by
    a scheduler (such as [lwt] or [unix]). This value describe when we require
    to [Read] (like {!val:Stdlib.input}), [Really_read] (like
    {!val:Stdlib.really_read}) and [Seek] (like {!val:Stdlib.seek_in}).

    We can compose these actions with [Bind], [Return] and [High]. The latter
    allows you to use a value [('a, 't) io] that comes from the scheduler used -
    so you can use an Lwt value (['a Lwt.t]) without depending on Lwt
    ([('a, lwt) t]) at this stage.

    For further informations, you can look at the paper about Lightweight
    Higher Kind Polymorphism available
    {{:https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf} here}. *)

type ('a, 't) io

type ('a, 'err, 't) t =
  | Really_read : int64 -> (string, 'err, 't) t
  | Read : int64 -> (string, 'err, 't) t
  | Seek : int64 -> (unit, 'err, 't) t
  | Bind : ('a, 'err, 't) t * ('a -> ('b, 'err, 't) t) -> ('b, 'err, 't) t
  | Return : ('a, 'err) result -> ('a, 'err, 't) t
  | High : (('a, 'err) result, 't) io -> ('a, 'err, 't) t
  | Write : string -> (unit, 'err, 't) t

val really_read : int64 -> (string, _, _) t
val read : int64 -> (string, _, _) t
val seek : int64 -> (unit, _, _) t
val bind : ('a, 'err, 't) t -> ('a -> ('b, 'err, 't) t) -> ('b, 'err, 't) t
val return : ('a, 'err) result -> ('a, 'err, _) t
val write : string -> (unit, _, _) t

module Syntax : sig
  val ( let* ) : ('a, 'err, 't) t -> ('a -> ('b, 'err, 't) t) -> ('b, 'err, 't) t
end

val ( let* ) : ('a, 'err, 't) t -> ('a -> ('b, 'err, 't) t) -> ('b, 'err, 't) t
[@@ocaml.deprecated "Use Tar.bind or Tar.Syntax.( let* )"]
(** Deprecated. Use Tar.bind or Tar.Syntax.( let* ) *)

type ('a, 'err, 't) fold =
  (?global:Header.Extended.t -> Header.t -> 'a -> ('a, 'err, 't) t) ->
  'a ->
  ('a, 'err, 't) t

val fold : ('a, [> `Fatal of error ], 't) fold
(** [fold f] is a [_ t] that reads an archive and executes [f] on each header.
    [f] is expected to either read or skip the file contents, or return an
    error. *)

type ('err, 't) content = unit -> (string option, 'err, 't) t
type ('err, 't) entry = Header.compatibility option * Header.t * ('err, 't) content
type ('err, 't) entries = unit -> (('err, 't) entry option, 'err, 't) t

val out :
     ?level:Header.compatibility
  -> ?global_hdr:Header.Extended.t
  -> ([> `Msg of string ] as 'err, 't) entries
  -> (unit, 'err, 't) t
(** [out hdr entries] is a [_ t] that writes [entries] into an archive. [hdr] is
    the global header and each entry must come from a {!type:content} stream and
    the associated header.*)
