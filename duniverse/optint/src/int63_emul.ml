(* On 32-bit systems, we emulate a 63-bit integer via a boxed 64-bit integer
   with its lowest bit set to 0. The remaining 63 bits are left-shifted by one
   place. This is analogous to the standard encoding of [int], with the bottom
   bit set to 0 rather than 1.

   See {{:https://github.com/janestreet/base/blob/master/src/int63_emul.ml}[Base.Int63_emul]}
   for a similar encoding that has subtly different guarantees. This
   implementation seeks to be strictly observationally equivalent to the
   unemulated one (on 64-bit architectures), at the cost of performance of
   certain functions.
*)

type t = int64

(* The following all preserve semantics under our chosen encoding. *)
include (Int64 : sig
  val add : t -> t -> t
  val sub : t -> t -> t
  val rem : t -> t -> t
  val neg : t -> t
  val abs : t -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val shift_left : t -> int -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end)

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

module Conv : sig
  val wrap_exn : int64 -> t      (* Raises if the [int64] has its topmost bit set. *)
  val wrap_modulo : int64 -> t   (* Discards the topmost bit of the [int64]. *)

  val unwrap : t -> int64        (* Lossless, assuming [t] satisfies the encoding. *)
end = struct
  let int64_fits_on_int63 =
    let min = Int64.(shift_right min_int) 1 in
    let max = Int64.(shift_right max_int) 1 in
    fun x -> Int64.compare min x <= 0 && Int64.compare x max <= 0

  let wrap_modulo x = Int64.mul x 2L
  let wrap_exn x =
    if int64_fits_on_int63 x then
      Int64.mul x 2L
    else
      Printf.ksprintf failwith
        "Conversion from int64 to int63 failed: %Ld is out of range" x

  let unwrap x = Int64.shift_right x 1
end

let unset_bottom_bit =
  let mask = 0xffff_ffff_ffff_fffEL in
  fun x -> Int64.logand x mask

let min_int = unset_bottom_bit Int64.min_int
let max_int = unset_bottom_bit Int64.max_int
let minus_one = Conv.wrap_exn (-1L)
let zero = Conv.wrap_exn 0L
let one = Conv.wrap_exn 1L

let succ x = add x one
let pred x = sub x one

let mul x y = Int64.mul x (Conv.unwrap y)
let div x y =
  let r = Int64.div x y in
  if Int64.equal r 0x4000_0000_0000_0000L then
    (* This case happens when we overflow via [ min_int / 1 ], in which case we
       should wrap back to [ min_int ]. *)
    min_int
  else
    Conv.wrap_modulo r

let lognot x = unset_bottom_bit (Int64.lognot x)
let logxor x y = unset_bottom_bit (Int64.logxor x y)
let shift_right x i = unset_bottom_bit (Int64.shift_right x i)
let shift_right_logical x i = unset_bottom_bit (Int64.shift_right_logical x i)

let to_int x = Int64.to_int (Conv.unwrap x)
let of_int x = Conv.wrap_exn (Int64.of_int x)
let to_int32 x = Int64.to_int32 (Conv.unwrap x)
let of_int32 x = Conv.wrap_exn (Int64.of_int32 x)
let to_int64 x = Conv.unwrap x
let of_int64 x = Conv.wrap_exn x
let to_float x = Int64.to_float (Conv.unwrap x)
let of_float x = Conv.wrap_exn (Int64.of_float x)

let to_string x = Int64.to_string (Conv.unwrap x)
let of_string x = Conv.wrap_exn (Int64.of_string x)
let of_string_opt x = try Some (of_string x) with _ -> None

let pp ppf x = Format.fprintf ppf "%Ld" (Conv.unwrap x)

let to_unsigned_int x =
  let max_int = of_int Stdlib.max_int in
  if compare zero x <= 0 && compare x max_int <= 0
  then to_int x
  else invalid_arg "Int63.to_unsigned_int: %Lx can not fit into a 31 bits unsigned integer" x

let without_bit_sign (x:int) = if x >= 0 then x else x land (lnot 0x40000000)

let of_unsigned_int x =
  if x < 0
  then logor 0x40000000L (of_int (without_bit_sign x))
  else of_int x

let to_unsigned_int32 x =
  let max_int = of_int32 Int32.max_int in
  if compare zero x <= 0 && compare x max_int <= 0
  then to_int32 x
  else invalid_arg "Int63.to_unsigned_int32: %Lx can not fit into a 32 bits unsigned integer" x

let of_unsigned_int32 x =
  if x < 0l
  then logor 0x80000000L (of_int32 (Int32.logand x (Int32.lognot 0x80000000l)))
  else of_int32 x

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
