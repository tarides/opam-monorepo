type ba =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type st = Bytes.t
type off = int
type len = int
type optint = Optint.t

module type FOREIGN = sig
  type t

  val unsafe_bytes : t -> st -> off -> len -> t
  val unsafe_bigstring : t -> ba -> off -> len -> t
end

module type DESC = sig
  val default : optint
end

module Adler32_foreign_64 = struct
  type t = int

  external unsafe_bytes :
    (t[@untagged]) ->
    st ->
    (off[@untagged]) ->
    (len[@untagged]) ->
    (t[@untagged])
    = "caml_checkseum_adler32_st" "caml_checkseum_adler32_st_untagged"
    [@@noalloc]

  external unsafe_bigstring :
    (t[@untagged]) ->
    ba ->
    (off[@untagged]) ->
    (len[@untagged]) ->
    (t[@untagged])
    = "caml_checkseum_adler32_ba" "caml_checkseum_adler32_ba_untagged"
    [@@noalloc]
end

module Adler32_foreign_32 = struct
  type t = int32

  external unsafe_bytes :
    (t[@unboxed]) -> st -> (off[@untagged]) -> (len[@untagged]) -> (t[@unboxed])
    = "caml_checkseum_adler32_st" "caml_checkseum_adler32_st_untagged"
    [@@noalloc]

  external unsafe_bigstring :
    (t[@unboxed]) -> ba -> (off[@untagged]) -> (len[@untagged]) -> (t[@unboxed])
    = "caml_checkseum_adler32_ba" "caml_checkseum_adler32_ba_untagged"
    [@@noalloc]
end

module Adler32_foreign : FOREIGN with type t = optint = struct
  let impl : (module FOREIGN with type t = Optint.t) =
    match Optint.is_immediate with
    | Optint.Conditional.True ->
        (module Adler32_foreign_64 : FOREIGN with type t = Optint.t)
    | Optint.Conditional.False ->
        (module Adler32_foreign_32 : FOREIGN with type t = Optint.t)

  include (val impl : FOREIGN with type t = Optint.t)
end

module Crc32_foreign_64 = struct
  type t = int

  external unsafe_bytes :
    (t[@untagged]) ->
    st ->
    (off[@untagged]) ->
    (len[@untagged]) ->
    (t[@untagged])
    = "caml_checkseum_crc32_st" "caml_checkseum_crc32_st_untagged"
    [@@noalloc]

  external unsafe_bigstring :
    (t[@untagged]) ->
    ba ->
    (off[@untagged]) ->
    (len[@untagged]) ->
    (t[@untagged])
    = "caml_checkseum_crc32_ba" "caml_checkseum_crc32_ba_untagged"
    [@@noalloc]
end

module Crc32_foreign_32 = struct
  type t = int32

  external unsafe_bytes :
    (t[@unboxed]) -> st -> (off[@untagged]) -> (len[@untagged]) -> (t[@unboxed])
    = "caml_checkseum_crc32_st" "caml_checkseum_crc32_st_untagged"
    [@@noalloc]

  external unsafe_bigstring :
    (t[@unboxed]) -> ba -> (off[@untagged]) -> (len[@untagged]) -> (t[@unboxed])
    = "caml_checkseum_crc32_ba" "caml_checkseum_crc32_ba_untagged"
    [@@noalloc]
end

module Crc32_foreign : FOREIGN with type t = optint = struct
  let impl : (module FOREIGN with type t = Optint.t) =
    match Optint.is_immediate with
    | Optint.Conditional.True ->
        (module Crc32_foreign_64 : FOREIGN with type t = Optint.t)
    | Optint.Conditional.False ->
        (module Crc32_foreign_32 : FOREIGN with type t = Optint.t)

  include (val impl : FOREIGN with type t = Optint.t)
end

module Crc32c_foreign_64 = struct
  type t = int

  external unsafe_bytes :
    (t[@untagged]) ->
    st ->
    (off[@untagged]) ->
    (len[@untagged]) ->
    (t[@untagged])
    = "caml_checkseum_crc32c_st" "caml_checkseum_crc32c_st_untagged"
    [@@noalloc]

  external unsafe_bigstring :
    (t[@untagged]) ->
    ba ->
    (off[@untagged]) ->
    (len[@untagged]) ->
    (t[@untagged])
    = "caml_checkseum_crc32c_ba" "caml_checkseum_crc32c_ba_untagged"
    [@@noalloc]
end

module Crc32c_foreign_32 = struct
  type t = int32

  external unsafe_bytes :
    (t[@unboxed]) -> st -> (off[@untagged]) -> (len[@untagged]) -> (t[@unboxed])
    = "caml_checkseum_crc32c_st" "caml_checkseum_crc32c_st_untagged"
    [@@noalloc]

  external unsafe_bigstring :
    (t[@unboxed]) -> ba -> (off[@untagged]) -> (len[@untagged]) -> (t[@unboxed])
    = "caml_checkseum_crc32c_ba" "caml_checkseum_crc32c_ba_untagged"
    [@@noalloc]
end

module Crc32c_foreign : FOREIGN with type t = optint = struct
  let impl : (module FOREIGN with type t = Optint.t) =
    match Optint.is_immediate with
    | Optint.Conditional.True ->
        (module Crc32c_foreign_64 : FOREIGN with type t = Optint.t)
    | Optint.Conditional.False ->
        (module Crc32c_foreign_32 : FOREIGN with type t = Optint.t)

  include (val impl : FOREIGN with type t = Optint.t)
end

module Crc24_foreign_64 = struct
  type t = int

  external unsafe_bytes :
    (t[@untagged]) ->
    st ->
    (off[@untagged]) ->
    (len[@untagged]) ->
    (t[@untagged])
    = "caml_checkseum_crc24_st" "caml_checkseum_crc24_st_untagged"
    [@@noalloc]

  external unsafe_bigstring :
    (t[@untagged]) ->
    ba ->
    (off[@untagged]) ->
    (len[@untagged]) ->
    (t[@untagged])
    = "caml_checkseum_crc24_ba" "caml_checkseum_crc24_ba_untagged"
    [@@noalloc]
end

module Crc24_foreign_32 = struct
  type t = int32

  external unsafe_bytes :
    (t[@unboxed]) -> st -> (off[@untagged]) -> (len[@untagged]) -> (t[@unboxed])
    = "caml_checkseum_crc24_st" "caml_checkseum_crc24_st_untagged"
    [@@noalloc]

  external unsafe_bigstring :
    (t[@unboxed]) -> ba -> (off[@untagged]) -> (len[@untagged]) -> (t[@unboxed])
    = "caml_checkseum_crc24_ba" "caml_checkseum_crc24_ba_untagged"
    [@@noalloc]
end

module Crc24_foreign : FOREIGN with type t = optint = struct
  let impl : (module FOREIGN with type t = Optint.t) =
    match Optint.is_immediate with
    | Optint.Conditional.True ->
        (module Crc24_foreign_64 : FOREIGN with type t = Optint.t)
    | Optint.Conditional.False ->
        (module Crc24_foreign_32 : FOREIGN with type t = Optint.t)

  include (val impl : FOREIGN with type t = Optint.t)
end

module Make (F : FOREIGN) (D : DESC) = struct
  type t = optint

  let pp ppf v = Optint.pp ppf v
  let equal a b = Optint.equal a b
  let default = D.default
  let unsafe_digest_bytes a o l v = F.unsafe_bytes v a o l
  let to_int32 = Optint.to_unsigned_int32
  let of_int32 = Optint.of_unsigned_int32

  let unsafe_digest_string a o l v =
    F.unsafe_bytes v (Bytes.unsafe_of_string a) o l

  let unsafe_digest_bigstring a o l v = F.unsafe_bigstring v a o l

  let digest_bytes a o l v =
    if o < 0 || l < 0 || o > Bytes.length a - l
    then invalid_arg "index out of bounds" ;
    unsafe_digest_bytes a o l v

  let digest_string a o l v =
    if o < 0 || l < 0 || o > String.length a - l
    then invalid_arg "index out of bounds" ;
    unsafe_digest_string a o l v

  let digest_bigstring a o l v =
    if o < 0 || l < 0 || o > Bigarray.Array1.dim a - l
    then invalid_arg "index out of bounds" ;
    unsafe_digest_bigstring a o l v
end

type bigstring = ba

module type S = sig
  type t = optint

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

module Adler32 : S =
  Make
    (Adler32_foreign)
    (struct
      let default = Optint.one
    end)

module Crc32 : S =
  Make
    (Crc32_foreign)
    (struct
      let default = Optint.zero
    end)

module Crc32c : S =
  Make
    (Crc32c_foreign)
    (struct
      let default = Optint.zero
    end)

module Crc24 : S =
  Make
    (Crc24_foreign)
    (struct
      let default = Optint.of_int 0xb704ce
    end)
