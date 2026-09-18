type t = Optint.t

let equal a b = Optint.equal a b
let pp ppf v = Optint.pp ppf v
let default = Optint.one
let _base = Optint.of_unsigned_int32 65521l
let _nmax = 5552

let digest : type a. get:(a -> int -> char) -> a -> int -> int -> t -> t =
 fun ~get buf off len adler32 ->
  let open Optint.Infix in
  let a = ref ((adler32 lsr 16) land Optint.of_int 0xFFFF) in
  let b = ref (adler32 land Optint.of_int 0xFFFF) in
  let l = ref len in
  let o = ref off in
  if len = 0
  then adler32
  else if len = 1
  then (
    b := !b + Optint.of_int (Char.code (get buf !o)) ;
    if !b >= _base then b := !b - _base ;
    a := !a + !b ;
    if !a >= _base then a := !a - _base ;
    !b lor (!a lsl 16))
  else if len < 16
  then (
    while !l <> 0 do
      b := !b + Optint.of_int (Char.code (get buf !o)) ;
      a := !a + !b ;
      incr o ;
      decr l
    done ;
    if !b >= _base then b := !b - _base ;
    a := Optint.rem !a _base ;
    !b lor (!a lsl 16))
  else (
    while !l >= _nmax do
      (l := Stdlib.(!l - _nmax)) ;
      for _ = Stdlib.(_nmax / 16) downto 1 do
        b := !b + Optint.of_int (Char.code (get buf !o)) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 1))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 2))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 3))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 4))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 5))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 6))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 7))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 8))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 9))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 10))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 11))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 12))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 13))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 14))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 15))) ;
        a := !a + !b ;
        o := Stdlib.(!o + 16)
      done ;
      b := Optint.rem !b _base ;
      a := Optint.rem !a _base
    done ;
    if !l > 0
    then (
      while !l >= 16 do
        (l := Stdlib.(!l - 16)) ;
        b := !b + Optint.of_int (Char.code (get buf !o)) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 1))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 2))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 3))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 4))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 5))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 6))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 7))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 8))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 9))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 10))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 11))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 12))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 13))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 14))) ;
        a := !a + !b ;
        b := !b + Optint.of_int (Char.code (get buf Stdlib.(!o + 15))) ;
        a := !a + !b ;
        o := Stdlib.(!o + 16)
      done ;
      while !l > 0 do
        b := !b + Optint.of_int (Char.code (get buf !o)) ;
        a := !a + !b ;
        decr l ;
        incr o
      done ;
      b := Optint.rem !b _base ;
      a := Optint.rem !a _base) ;
    !b lor (!a lsl 16))

let unsafe_digest_bytes a o l v = digest ~get:Bytes.unsafe_get a o l v
let digest_bytes a o l v = digest ~get:Bytes.get a o l v
let unsafe_digest_string a o l v = digest ~get:String.unsafe_get a o l v
let digest_string a o l v = digest ~get:String.get a o l v

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let unsafe_digest_bigstring a o l v =
  digest ~get:Bigarray.Array1.unsafe_get a o l v

let digest_bigstring a o l v = digest ~get:Bigarray.Array1.get a o l v
let to_int32 = Optint.to_unsigned_int32
let of_int32 = Optint.of_unsigned_int32
