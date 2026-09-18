type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module type S = sig
  type t = Optint.t

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val default : t
  val digest_bytes : Bytes.t -> int -> int -> t -> t
  val unsafe_digest_bytes : Bytes.t -> int -> int -> t -> t
  val digest_string : String.t -> int -> int -> t -> t
  val unsafe_digest_string : String.t -> int -> int -> t -> t
  val digest_bigstring : bigstring -> int -> int -> t -> t
  val unsafe_digest_bigstring : bigstring -> int -> int -> t -> t
  val to_int32 : t -> int32
  val of_int32 : int32 -> t
end

module Adler32 : S = Gin_adler32
module Crc32c : S = Gin_crc32c
module Crc32 : S = Gin_crc32
module Crc24 : S = Gin_crc24
