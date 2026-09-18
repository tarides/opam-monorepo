type t = int

let zero = 0
let one = 1
let minus_one = -1
let neg x = -x
let add a b = a + b
let sub a b = a - b
let mul a b = a * b
let div a b = a / b
let rem a b = a mod b
let succ x = succ x
let pred x = pred x
let logand a b = a land b
let logor a b = a lor b
let logxor a b = a lxor b
let lognot x = lnot x
let shift_left a n = a lsl n
let shift_right a n = a asr n
let shift_right_logical a n = a lsr n
let abs x = abs x
let max_int = max_int
let min_int = min_int

external of_int : t -> t = "%identity"
external to_int : t -> t = "%identity"

let to_int32 = Stdlib.Int32.of_int
let of_int32 = Stdlib.Int32.to_int
let to_int64 = Stdlib.Int64.of_int
let of_int64 = Stdlib.Int64.to_int
let of_float x = int_of_float x
let to_float x = float_of_int x
let of_string x = int_of_string x
let of_string_opt x = try Some (of_string x) with Failure _ -> None
let to_string x = string_of_int x
let equal : int -> int -> bool = fun a b -> a = b
let compare : int -> int -> int = fun a b -> compare a b
let pp = Format.pp_print_int

external to_unsigned_int : t -> int = "%identity"
external of_unsigned_int : int -> t = "%identity"

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let to_unsigned_int32 =
  let uint32_mask = (0xffff lsl 16) lor 0xffff in
  fun x ->
    let truncated = x land uint32_mask in
    if x <> truncated
    then invalid_arg "Int63.to_unsigned_int32: %d can not fit into a 32 bits integer" x
    else Int32.of_int truncated

let of_unsigned_int32 =
  let int32_sign_mask = 1 lsl 31 in
  let int32_sign_maskl = 0x80000000l in
  fun x ->
    if x < 0l then
      let x = Int32.logand x (Int32.lognot int32_sign_maskl) in
      Int32.to_int x lor int32_sign_mask
    else Int32.to_int x

let encoded_size = 8

external set_64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64u"
external get_64 : string -> int -> int64 = "%caml_string_get64"
external swap64 : int64 -> int64 = "%bswap_int64"

let encode buf ~off t =
  let t = to_int64 t in
  let t = if not Sys.big_endian then swap64 t else t in
  set_64 buf off t

let decode buf ~off =
  let t = get_64 buf off in
  let t = if not Sys.big_endian then swap64 t else t in
  of_int64 t

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
