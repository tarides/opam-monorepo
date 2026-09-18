let _1000000 = Optint.of_unsigned_int 0x1000000
let _ffffff = Optint.of_unsigned_int 0xffffff
let _crc24_poly = Optint.of_unsigned_int 0x1864cfb

let crc24 :
    type a. get:(a -> int -> char) -> a -> int -> int -> Optint.t -> Optint.t =
 fun ~get buf off len crc ->
  let crc = ref crc in
  for i = 0 to len - 1 do
    crc :=
      Optint.logxor !crc
        (Optint.of_unsigned_int (Char.code (get buf (off + i)) lsl 16)) ;
    for _ = 0 to 7 do
      crc := Optint.shift_left !crc 1 ;
      if Optint.logand !crc _1000000 <> Optint.zero
      then crc := Optint.logxor !crc _crc24_poly
    done
  done ;
  Optint.logand !crc _ffffff

type t = Optint.t

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let equal a b = Optint.equal a b
let pp ppf v = Optint.pp ppf v
let default = Optint.of_unsigned_int 0xb704ce
let digest_bigstring a o l v = crc24 ~get:Bigarray.Array1.get a o l v

let unsafe_digest_bigstring a o l v =
  crc24 ~get:Bigarray.Array1.unsafe_get a o l v

let digest_string a o l v = crc24 ~get:String.get a o l v
let unsafe_digest_string a o l v = crc24 ~get:String.unsafe_get a o l v
let digest_bytes a o l v = crc24 ~get:Bytes.get a o l v
let unsafe_digest_bytes a o l v = crc24 ~get:Bytes.unsafe_get a o l v
let to_int32 = Optint.to_unsigned_int32
let of_int32 = Optint.of_unsigned_int32
