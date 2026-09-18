type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module type S = sig
  type t = Optint.t
  (** Representation of the checksum value. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-printer of {!t}. *)

  val equal : t -> t -> bool
  (** The equal function of {!t}. *)

  val default : t
  (** Default value of {!t}. *)

  val digest_bytes : Bytes.t -> int -> int -> t -> t
  (** [digest_bytes msg off len t] is the digest of [msg] at [off] on [len]
      byte(s). *)

  val unsafe_digest_bytes : Bytes.t -> int -> int -> t -> t
  (** [unsafe_digest_bytes msg off len t] is the same as {!digest_bytes} without
      bound-checking. *)

  val digest_string : String.t -> int -> int -> t -> t
  (** Same as {!digest_bytes} but for {!String.t}. *)

  val unsafe_digest_string : String.t -> int -> int -> t -> t
  (** [unsafe_digest_string msg off len t] is the same as {!digest_string}
      without bound-checking. *)

  val digest_bigstring : bigstring -> int -> int -> t -> t
  (** Same as {!digest_bytes} but for {!bigstring}. *)

  val unsafe_digest_bigstring : bigstring -> int -> int -> t -> t
  (** [unsafe_digest_bigstring msg off len t] is the same as {!digest_bigstring}
      without bound-checking. *)

  val to_int32 : t -> int32
  (** [to_int32 crc] casts [crc] to an {b unsigned} [int32] value. We precise
      {i unsigned} because every bits are significant for the checksum (even the
      sign bit). We interpret the given [int32] as a value where the sign bit
      does not have any signification (hence the {i unsigned}). *)

  val of_int32 : int32 -> t
  (** [of_int32 crc] casts the given unsigned [int32] value to {!t}. For a
      checksum, every bits (including the sign bit) has a signification. The
      user must interpret the given [int32] as an unsigned [int32] (eg.
      regardless the sign bit which has another signification for the checksum). *)
end

module Adler32 : S
(** Implementation of the ADLER-32 cheksum. *)

module Crc32c : S
(** Implementation of the CRC32C checksum. *)

module Crc32 : S
(** Implementation of CRC32 checksum. *)

module Crc24 : S
(** Implementation of CRC24 checksum. *)
