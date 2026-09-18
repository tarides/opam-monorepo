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

type error = [ `Fatal of Tar.error | `Eof | `Gz of string ]

val in_gzipped : ('a, ([> error ] as 'err), 't) Tar.t -> ('a, 'err, 't) Tar.t
  (** [in_gzipped] takes a {i tar process} (like {!val:Tar.fold}) and add a
      uncompression layer to be able to manipulate a [*.tar.gz] archive. *)

val out_gzipped :
     level:int
  -> mtime:int32
  -> Gz.os
  -> ('a, 'err, 't) Tar.t
  -> ('a, 'err, 't) Tar.t
(** [out_gzipped] takes a {i tar process} (like {!val:Tar.out}) and add a
    compression layer to be able to generate a [*.tar.gz] archive. *)
