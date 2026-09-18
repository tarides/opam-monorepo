type t = int

let zero = 0
let one = 1
let minus_one = (-1)
let neg x = (-x)
let add a b = a + b
let sub a b = a - b
let mul a b = a * b

let _unsigned_compare n m =
  let open Nativeint in
  compare (sub n min_int) (sub m min_int)

let _unsigned_div n d =
  let open Nativeint in
  if d < zero then
    if _unsigned_compare n d < 0 then zero else one
  else
    let q = shift_left (div (shift_right_logical n 1) d) 1 in
    let r = sub n (mul q d) in
    if _unsigned_compare r d >= 0 then succ q else q

let div a b = Nativeint.to_int (_unsigned_div (Nativeint.of_int a) (Nativeint.of_int b))
let rem a b = a mod b
let succ x = x + 1
let pred x = x - 1
let abs x =
  let mask = x asr Sys.int_size in (* extract sign: -1 if signed, 0 if not signed *)
  (x + mask) lxor mask
let max_int = Int32.(to_int max_int)
let min_int = Int32.(to_int min_int)
let logand a b = a land b
let logor a b = a lor b
let logxor a b = a lxor b
let lognot x = lnot x
let shift_left a n = a lsl n
let shift_right a n = a asr n
let shift_right_logical a n = a lsr n
external of_int : int -> t = "%identity"
external of_unsigned_int : int -> t = "%identity"
external to_int : t -> int = "%identity"
external to_unsigned_int : t -> int = "%identity"
let to_int64 = Stdlib.Int64.of_int
let of_int64 = Stdlib.Int64.to_int
let of_float x = int_of_float x
let to_float x = (* allocation *) float_of_int x
let of_string x = int_of_string x
let of_string_opt x = try (* allocation *) Some (of_string x) with Failure _ -> None
let to_string x = string_of_int x
let compare : int -> int -> int = fun a b -> a - b
let equal : int -> int -> bool = fun a b -> a = b

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let uint32_max = (0xffff lsl 16) lor 0xffff
let int32_sign_maskl = 0x80000000l
let int32_sign_mask = 1 lsl 31
let int32_maxl = 0x7fffffffl
let int32_max = 0x7fffffff

let to_int32 x =
  let truncated = x land uint32_max in
  if x = truncated then Int32.of_int truncated
  else if compare 0 x > 0 && (x lsr 31) = uint32_max
  then Int32.(logor int32_sign_maskl (of_int (x land int32_max)))
  else invalid_arg "Optint.to_int32: %d can not fit into a 32 bits integer" x

let to_unsigned_int32 x =
  let truncated = x land uint32_max in
  if x <> truncated
  then invalid_arg "Optint.to_unsigned_int32: %d can not fit into a 32 bits integer" x
  else Int32.of_int truncated

let of_int32 =
  let negative_int32_mask = (int32_max lsl 32) lor int32_sign_mask in
  fun x ->
    if x < 0l
    then
      let x = Int32.logand x int32_maxl in
      negative_int32_mask lor (Int32.to_int x)
    else Int32.to_int x

let of_unsigned_int32 x =
  if x < 0l
  then
    let x = Int32.logand x (Int32.lognot int32_sign_maskl) in
    (Int32.to_int x) lor int32_sign_mask
  else Int32.to_int x

let pp ppf (x:t) = Format.fprintf ppf "%d" x

let encoded_size = 4

external set_32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32u"
external get_32 : string -> int -> int32 = "%caml_string_get32"
external swap32 : int32 -> int32 = "%bswap_int32"

let encode buf ~off t =
  let t = to_int32 t in
  let t = if not Sys.big_endian then swap32 t else t in
  set_32 buf off t

let decode buf ~off =
  let t = get_32 buf off in
  let t = if not Sys.big_endian then swap32 t else t in
  of_int32 t

module Infix = struct
  let ( + ) a b = add a b
  let ( - ) a b = sub a b
  let ( * ) a b = mul a b
  let ( % ) a b = rem a b
  let ( / ) a b = div a b

  let ( land ) a b = logand a b
  let ( lor ) a b = logor a b
  let ( lsr ) a b = shift_right a b
  let ( lsl ) a b = shift_left a b

  let ( && ) = ( land )
  let ( || ) = ( lor )
  let ( >> ) = ( lsr )
  let ( << ) = ( lsl )

end
