[@@@landmark "auto"]

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(* XXX(dinosaure): prelude. *)

let invalid_arg fmt = Format.kasprintf invalid_arg fmt
let kstrf k fmt = Format.kasprintf k fmt
let bigstring_empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0

let bigstring_create l =
  Bigarray.Array1.create Bigarray.char Bigarray.c_layout l

let bigstring_length x = Bigarray.Array1.dim x [@@inline]

external swap : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external unsafe_get_uint8 : bigstring -> int -> int = "%caml_ba_ref_1"
external unsafe_get_char : bigstring -> int -> char = "%caml_ba_ref_1"
external unsafe_get_uint16 : bigstring -> int -> int = "%caml_bigstring_get16"
external unsafe_get_uint32 : bigstring -> int -> int32 = "%caml_bigstring_get32"

external string_unsafe_get_uint32 : string -> int -> int32
  = "%caml_string_get32"

let string_unsafe_get_uint8 : string -> int -> int =
 fun buf off -> Char.code buf.[off]

external bytes_unsafe_get_uint32 : bytes -> int -> int32 = "%caml_bytes_get32"

let bytes_unsafe_get_uint8 : bytes -> int -> int =
 fun buf off -> Char.code (Bytes.get buf off)

external unsafe_set_uint8 : bigstring -> int -> int -> unit = "%caml_ba_set_1"

external unsafe_set_uint16 : bigstring -> int -> int -> unit
  = "%caml_bigstring_set16"

external unsafe_set_uint32 : bigstring -> int -> int32 -> unit
  = "%caml_bigstring_set32"

let bytes_unsafe_set_uint8 : bytes -> int -> int -> unit =
 fun buf off v -> Bytes.set buf off (Char.unsafe_chr (v land 0xff))

external bytes_unsafe_set_uint32 : bytes -> int -> int32 -> unit
  = "%caml_bytes_set32"

(* XXX(dinosaure): little-endian only *)
let unsafe_set_uint16_le =
  if not Sys.big_endian then fun buf off v -> unsafe_set_uint16 buf off v
  else fun buf off v -> unsafe_set_uint16 buf off (swap v)

let _unsafe_set_uint16_be =
  if Sys.big_endian then fun buf off v -> unsafe_set_uint16 buf off v
  else fun buf off v -> unsafe_set_uint16 buf off (swap v)

let unsafe_get_uint16_le =
  if not Sys.big_endian then fun buf off -> unsafe_get_uint16 buf off
  else fun buf off -> swap (unsafe_get_uint16 buf off)

let _unsafe_get_uint16_be =
  if Sys.big_endian then fun buf off -> unsafe_get_uint16 buf off
  else fun buf off -> swap (unsafe_get_uint16 buf off)

let _unsafe_set_uint32_le =
  if Sys.big_endian then fun buf off v -> unsafe_set_uint32 buf off (swap32 v)
  else fun buf off v -> unsafe_set_uint32 buf off v

let bigstring_to_string v =
  let len = bigstring_length v in
  let res = Bytes.create len in
  let len0 = len land 3 in
  let len1 = len asr 2 in

  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = unsafe_get_uint32 v i in
    bytes_unsafe_set_uint32 res i v
  done

  ; for i = 0 to len0 - 1 do
      let i = (len1 * 4) + i in
      let v = unsafe_get_uint8 v i in
      bytes_unsafe_set_uint8 res i v
    done

  ; Bytes.unsafe_to_string res

let bigstring_of_string v =
  let len = String.length v in
  let res = bigstring_create len in
  let len0 = len land 3 in
  let len1 = len asr 2 in

  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = string_unsafe_get_uint32 v i in
    unsafe_set_uint32 res i v
  done

  ; for i = 0 to len0 - 1 do
      let i = (len1 * 4) + i in
      let v = string_unsafe_get_uint8 v i in
      unsafe_set_uint8 res i v
    done
  ; res

let[@inline always] is_power_of_two v = v <> 0 && v land (lnot v + 1) = v

let[@inline always] to_power_of_two v =
  let res = ref (pred v) in
  res := !res lor (!res lsr 1)
  ; res := !res lor (!res lsr 2)
  ; res := !res lor (!res lsr 4)
  ; res := !res lor (!res lsr 8)
  ; res := !res lor (!res lsr 16)
  ; succ !res

let output_bigstring oc buf off len =
  (* XXX(dinosaure): stupidly slow! *)
  let v = Bigarray.Array1.sub buf off len in
  let v = bigstring_to_string v in
  output_string oc v

let input_bigstring ic buf off len =
  let tmp = Bytes.create len in
  let res = input ic tmp 0 len in

  let len0 = res land 3 in
  let len1 = res asr 2 in

  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = bytes_unsafe_get_uint32 tmp i in
    unsafe_set_uint32 buf (off + i) v
  done

  ; for i = 0 to len0 - 1 do
      let i = (len1 * 4) + i in
      let v = bytes_unsafe_get_uint8 tmp i in
      unsafe_set_uint8 buf (off + i) v
    done
  ; res

let invalid_bounds off len =
  invalid_arg "Out of bounds (off: %d, len: %d)" off len

let unsafe_blit src src_off dst dst_off len =
  for i = 0 to len - 1 do
    unsafe_set_uint8 dst (dst_off + i) (unsafe_get_uint8 src (src_off + i))
  done

let slow_blit2 src src_off dst0 dst0_off dst1 dst1_off len =
  for i = 0 to len - 1 do
    let v = unsafe_get_uint8 src (src_off + i) in
    unsafe_set_uint8 dst0 (dst0_off + i) v
    ; unsafe_set_uint8 dst1 (dst1_off + i) v
  done

(* XXX(dinosaure): fast blit when it's possible. *)

let blit2 src src_off dst0 dst0_off dst1 dst1_off len =
  if dst0_off - src_off < 4 then
    slow_blit2 src src_off dst0 dst0_off dst1 dst1_off len
  else
    let len0 = len land 3 in
    let len1 = len asr 2 in

    for i = 0 to len1 - 1 do
      let i = i * 4 in
      let v = unsafe_get_uint32 src (src_off + i) in
      unsafe_set_uint32 dst0 (dst0_off + i) v
      ; unsafe_set_uint32 dst1 (dst1_off + i) v
    done

    ; for i = 0 to len0 - 1 do
        let i = (len1 * 4) + i in
        let v = unsafe_get_uint8 src (src_off + i) in
        unsafe_set_uint8 dst0 (dst0_off + i) v
        ; unsafe_set_uint8 dst1 (dst1_off + i) v
      done

(* XXX(dinosaure): fast fill operation. (usually when [Match (len:?, dist:1)]) *)

let fill2 v dst0 dst0_off dst1 dst1_off len =
  let len0 = len land 3 in
  let len1 = len asr 2 in

  let nv = Nativeint.of_int v in
  let vv = Nativeint.(logor (shift_left nv 8) nv) in
  let vvvv = Nativeint.(logor (shift_left vv 16) vv) in
  let vvvv = Nativeint.to_int32 vvvv in

  for i = 0 to len1 - 1 do
    let i = i * 4 in
    unsafe_set_uint32 dst0 (dst0_off + i) vvvv
    ; unsafe_set_uint32 dst1 (dst1_off + i) vvvv
  done

  ; for i = 0 to len0 - 1 do
      let i = (len1 * 4) + i in
      unsafe_set_uint8 dst0 (dst0_off + i) v
      ; unsafe_set_uint8 dst1 (dst1_off + i) v
    done

let io_buffer_size = 65536

(* XXX(dinosaure): Specialization. *)

external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"

let ( > ) (x : int) y = x > y [@@inline]
let ( < ) (x : int) y = x < y [@@inline]
let ( <= ) (x : int) y = x <= y [@@inline]
let ( >= ) (x : int) y = x >= y [@@inline]
let min (a : int) b = if a <= b then a else b [@@inline]
let max (a : int) b = if a >= b then a else b [@@inline]

(* XXX(dinosaure): Constants. *)

let _max_bits = 15
let _smallest = 1
let _rep_3_6 = 16
let _repz_3_10 = 17
let _repz_11_138 = 18
let _literals = 256
let _length_codes = 29
let _l_codes = _literals + 1 + _length_codes
let _d_codes = 30
let _heap_size = (2 * _l_codes) + 1
let _bl_codes = 19

let zigzag =
  [|16; 17; 18; 0; 8; 7; 9; 6; 10; 5; 11; 4; 12; 3; 13; 2; 14; 1; 15|]

let _length =
  [|
     0; 0; 0; 0; 1; 2; 3; 4; 5; 6; 7; 8; 8; 9; 9; 10; 10; 11; 11; 12; 12; 12; 12
   ; 13; 13; 13; 13; 14; 14; 14; 14; 15; 15; 15; 15; 16; 16; 16; 16; 16; 16; 16
   ; 16; 17; 17; 17; 17; 17; 17; 17; 17; 18; 18; 18; 18; 18; 18; 18; 18; 19; 19
   ; 19; 19; 19; 19; 19; 19; 20; 20; 20; 20; 20; 20; 20; 20; 20; 20; 20; 20; 20
   ; 20; 20; 20; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21
   ; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 23; 23; 23
   ; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 24; 24; 24; 24; 24; 24
   ; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24
   ; 24; 24; 24; 24; 24; 24; 24; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25
   ; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25
   ; 25; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26
   ; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 27; 27; 27; 27; 27
   ; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27
   ; 27; 27; 27; 27; 27; 27; 27; 28
  |]

let _distance =
  [|
     0; 1; 2; 3; 4; 4; 5; 5; 6; 6; 6; 6; 7; 7; 7; 7; 8; 8; 8; 8; 8; 8; 8; 8; 9; 9
   ; 9; 9; 9; 9; 9; 9; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10
   ; 10; 10; 11; 11; 11; 11; 11; 11; 11; 11; 11; 11; 11; 11; 11; 11; 11; 11; 12
   ; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12
   ; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 13; 13; 13; 13; 13; 13; 13
   ; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13
   ; 13; 13; 13; 13; 13; 13; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14
   ; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14
   ; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14
   ; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 15; 15; 15; 15; 15; 15
   ; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15
   ; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15
   ; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15
   ; 15; 0; 0; 16; 17; 18; 18; 19; 19; 20; 20; 20; 20; 21; 21; 21; 21; 22; 22
   ; 22; 22; 22; 22; 22; 22; 23; 23; 23; 23; 23; 23; 23; 23; 24; 24; 24; 24; 24
   ; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 25; 25; 25; 25; 25; 25; 25; 25
   ; 25; 25; 25; 25; 25; 25; 25; 25; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26
   ; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26
   ; 26; 26; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27
   ; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 28; 28; 28; 28
   ; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28
   ; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28
   ; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28
   ; 28; 28; 28; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29
   ; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29
   ; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29
   ; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29
  |]

let _distance code =
  if code < 256 then _distance.(code) else _distance.(256 + (code lsr 7))
[@@inline]

let _base_length =
  [|
     0; 1; 2; 3; 4; 5; 6; 7; 8; 10; 12; 14; 16; 20; 24; 28; 32; 40; 48; 56; 64
   ; 80; 96; 112; 128; 160; 192; 224; 255; 0; 0
  |]

(* assert (Array.length _base_length = 32) ;

   XXX(dinosaure): in [zlib], [base_length] has 29 elements - however, it uses
   the array only when it deflates something - it uses something else about
   the inflation. We added two last [0] to avoid an [index out of bounds] where,
   in some context, [_base_length] is used with input bits - finally, we can
   mask input bits with [0x1f]. *)

let _extra_lbits =
  [|
     0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3; 4; 4; 4; 4; 5; 5
   ; 5; 5; 0; 0; 0; 0
  |]

let _extra_dbits =
  [|
     0; 0; 0; 0; 1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9; 10; 10; 11
   ; 11; 12; 12; 13; 13; 0; 0
  |]

(* assert (Array.length _extra_dbits = 32) ; *)

let _base_dist =
  [|
     0; 1; 2; 3; 4; 6; 8; 12; 16; 24; 32; 48; 64; 96; 128; 192; 256; 384; 512
   ; 768; 1024; 1536; 2048; 3072; 4096; 6144; 8192; 12288; 16384; 24576; -1; -1
  |]

(* Window for end-user. *)

type window = bigstring

let make_window ~bits =
  if bits >= 8 && bits <= 15 then bigstring_create (1 lsl 15)
  else invalid_arg "bits MUST be between 8 and 15 (%d)" bits

let ffs n =
  if n = 0 then invalid_arg "ffs on 0"
  else
    let t = ref 1 in
    let r = ref 0 in

    while n land !t = 0 do
      t := !t lsl 1
      ; incr r
    done
    ; !r

let window_bits w = ffs (bigstring_length w)

module Lookup = struct
  (* Used as inflate to store lookup.[bit-sequence] = [len << 15 | byte].
     Used as deflate to store lookup.[byte] = [len << 15 | bit-sequence].

     For inflate, the table can be two-level (see zlib's inftrees.c):
     [m] is the mask of the [root] index bits
     [l] is the longest code length used to know how many bits to buffer

     A one-level table simply has [root = l] *)
  type t = {t: int array; m: int; l: int; root: int}

  let mask = (1 lsl _max_bits) - 1

  let make t ?root max =
    let root = Option.value ~default:max root in
    {t; m= (1 lsl root) - 1; l= max; root}

  let get t i =
    let v = t.t.(i) in
    v lsr _max_bits, v land mask
  (* allocation *)
  [@@inline]
end

let _static_ltree =
  [|
     12, 8; 140, 8; 76, 8; 204, 8; 44, 8; 172, 8; 108, 8; 236, 8; 28, 8; 156, 8
   ; 92, 8; 220, 8; 60, 8; 188, 8; 124, 8; 252, 8; 2, 8; 130, 8; 66, 8; 194, 8
   ; 34, 8; 162, 8; 98, 8; 226, 8; 18, 8; 146, 8; 82, 8; 210, 8; 50, 8; 178, 8
   ; 114, 8; 242, 8; 10, 8; 138, 8; 74, 8; 202, 8; 42, 8; 170, 8; 106, 8; 234, 8
   ; 26, 8; 154, 8; 90, 8; 218, 8; 58, 8; 186, 8; 122, 8; 250, 8; 6, 8; 134, 8
   ; 70, 8; 198, 8; 38, 8; 166, 8; 102, 8; 230, 8; 22, 8; 150, 8; 86, 8; 214, 8
   ; 54, 8; 182, 8; 118, 8; 246, 8; 14, 8; 142, 8; 78, 8; 206, 8; 46, 8; 174, 8
   ; 110, 8; 238, 8; 30, 8; 158, 8; 94, 8; 222, 8; 62, 8; 190, 8; 126, 8; 254, 8
   ; 1, 8; 129, 8; 65, 8; 193, 8; 33, 8; 161, 8; 97, 8; 225, 8; 17, 8; 145, 8
   ; 81, 8; 209, 8; 49, 8; 177, 8; 113, 8; 241, 8; 9, 8; 137, 8; 73, 8; 201, 8
   ; 41, 8; 169, 8; 105, 8; 233, 8; 25, 8; 153, 8; 89, 8; 217, 8; 57, 8; 185, 8
   ; 121, 8; 249, 8; 5, 8; 133, 8; 69, 8; 197, 8; 37, 8; 165, 8; 101, 8; 229, 8
   ; 21, 8; 149, 8; 85, 8; 213, 8; 53, 8; 181, 8; 117, 8; 245, 8; 13, 8; 141, 8
   ; 77, 8; 205, 8; 45, 8; 173, 8; 109, 8; 237, 8; 29, 8; 157, 8; 93, 8; 221, 8
   ; 61, 8; 189, 8; 125, 8; 253, 8; 19, 9; 275, 9; 147, 9; 403, 9; 83, 9; 339, 9
   ; 211, 9; 467, 9; 51, 9; 307, 9; 179, 9; 435, 9; 115, 9; 371, 9; 243, 9
   ; 499, 9; 11, 9; 267, 9; 139, 9; 395, 9; 75, 9; 331, 9; 203, 9; 459, 9; 43, 9
   ; 299, 9; 171, 9; 427, 9; 107, 9; 363, 9; 235, 9; 491, 9; 27, 9; 283, 9
   ; 155, 9; 411, 9; 91, 9; 347, 9; 219, 9; 475, 9; 59, 9; 315, 9; 187, 9
   ; 443, 9; 123, 9; 379, 9; 251, 9; 507, 9; 7, 9; 263, 9; 135, 9; 391, 9; 71, 9
   ; 327, 9; 199, 9; 455, 9; 39, 9; 295, 9; 167, 9; 423, 9; 103, 9; 359, 9
   ; 231, 9; 487, 9; 23, 9; 279, 9; 151, 9; 407, 9; 87, 9; 343, 9; 215, 9
   ; 471, 9; 55, 9; 311, 9; 183, 9; 439, 9; 119, 9; 375, 9; 247, 9; 503, 9
   ; 15, 9; 271, 9; 143, 9; 399, 9; 79, 9; 335, 9; 207, 9; 463, 9; 47, 9; 303, 9
   ; 175, 9; 431, 9; 111, 9; 367, 9; 239, 9; 495, 9; 31, 9; 287, 9; 159, 9
   ; 415, 9; 95, 9; 351, 9; 223, 9; 479, 9; 63, 9; 319, 9; 191, 9; 447, 9
   ; 127, 9; 383, 9; 255, 9; 511, 9; 0, 7; 64, 7; 32, 7; 96, 7; 16, 7; 80, 7
   ; 48, 7; 112, 7; 8, 7; 72, 7; 40, 7; 104, 7; 24, 7; 88, 7; 56, 7; 120, 7
   ; 4, 7; 68, 7; 36, 7; 100, 7; 20, 7; 84, 7; 52, 7; 116, 7; 3, 8; 131, 8
   ; 67, 8; 195, 8; 35, 8; 163, 8; 99, 8; 227, 8
  |]

let _static_ltree =
  let t = Array.map (fun (v, l) -> (l lsl _max_bits) lor v) _static_ltree in
  Lookup.make t 9

let _static_dtree =
  [|
     0, 5; 16, 5; 8, 5; 24, 5; 4, 5; 20, 5; 12, 5; 28, 5; 2, 5; 18, 5; 10, 5
   ; 26, 5; 6, 5; 22, 5; 14, 5; 30, 5; 1, 5; 17, 5; 9, 5; 25, 5; 5, 5; 21, 5
   ; 13, 5; 29, 5; 3, 5; 19, 5; 11, 5; 27, 5; 7, 5; 23, 5
  |]

let _static_dtree =
  let t = Array.map (fun (v, l) -> (l lsl _max_bits) lor v) _static_dtree in
  Lookup.make t 5

(* XXX(dinosaure): [zlib] raises "Invalid distance code" where it wants to
   access to [base_dist.(30|31)]. It uses a smart mask to catch this behavior.
   In this code, we did not raise an error nor /compromise/ output when we fall
   to [Match (len:?, dist:0)] (so, nothing to do).

   Case can be retrieved with "\x02\x7e\xff\xff". NOTE: [miniz] has this silent
   behavior.

   XXX(dinosaure): It's not true anymore where we decide to raise an error to
   avoid an other error about access on window. It's explained below when we
   have a [Write] operation. *)

type optint = Optint.t

module WInf = struct
  type t = {raw: bigstring; mutable w: int; mutable c: optint}

  let max = 1 lsl 15
  let mask = (1 lsl 15) - 1

  let[@warning "-unused-value-declaration"] make () =
    {raw= bigstring_create max; w= 0; c= Checkseum.Adler32.default}

  let from raw = {raw; w= 0; c= Checkseum.Adler32.default}

  let reset t =
    t.w <- 0
    ; t.c <- Checkseum.Adler32.default

  let mask v = v land mask [@@inline]

  let update w =
    let c = Checkseum.Adler32.unsafe_digest_bigstring w.raw 0 max w.c in
    w.c <- c

  let add t v =
    unsafe_set_uint8 t.raw (mask t.w) v
    ; if mask (t.w + 1) == 0 then update t
    ; t.w <- t.w + 1

  let sub a b = a - b

  let compare a b =
    (compare : int -> int -> int) (sub a min_int) (sub b min_int)

  let have t = if compare t.w max < 0 then t.w else max

  (* XXX(dinosaure): a dragoon here. overflow can appear on [t.w] which only
     increases. [compare] gives us a new chance to compare correctly [t.w] if it
     overflows __one-time__. Then, for the second time, this code is broken. *)

  let blit t w w_off o o_off len =
    let msk = mask t.w in
    let pre = max - msk in
    let rst = len - pre in
    if rst >= 0 then (
      blit2 w w_off t.raw msk o o_off pre
      ; update t
      ; blit2 w (w_off + pre) t.raw 0 o (o_off + pre) rst)
    else (
      blit2 w w_off t.raw msk o o_off len
      ; if mask (t.w + len) == 0 && len > 0 then update t)
    ; t.w <- t.w + len

  let fill t v o o_off len =
    let msk = mask t.w in
    let pre = max - msk in
    let rst = len - pre in
    if rst >= 0 then (
      fill2 v t.raw msk o o_off pre
      ; update t
      ; fill2 v t.raw 0 o (o_off + pre) rst)
    else (
      fill2 v t.raw msk o o_off len
      ; if mask (t.w + len) == 0 && len > 0 then update t)
    ; t.w <- t.w + len

  let tail w =
    let msk = mask w.w in
    if msk > 0 then (
      let c = Checkseum.Adler32.unsafe_digest_bigstring w.raw 0 msk w.c in
      w.w <- 0
      ; (* XXX(dinosaure): reset! *)
        w.c <- c)

  let checksum w = w.c
end

module Inf = struct
  (* à la dbuenzli *)

  type src = [ `Channel of in_channel | `String of string | `Manual ]
  type decode = [ `Await | `Flush | `End | `Malformed of string ]

  exception Invalid_huffman

  type kind = CODES | LENS | DISTS

  let _link_flag = 1 lsl 20
  let empty_table = [|1 lsl _max_bits (* len: 1, val: 0 *)|], 1, 1

  let huffman kind table off codes =
    let bl_count = Array.make 16 0 in
    let max = ref 15 in

    for sym = 0 to codes - 1 do
      let p = table.(off + sym) in
      bl_count.(p) <- bl_count.(p) + 1
    done

    ; (* XXX(dinosaure): check if we have an incomplete set for [LENS] and [DIST].
         This code is ugly, TODO! *)
      let exception Break in
      (try
         while !max >= 1 do
           if bl_count.(!max) != 0 then raise_notrace Break
           ; decr max
         done
       with Break -> ())

      ; if !max == 0 then empty_table
        else begin
          let left = ref 1 in
          for i = 1 to 15 do
            left := (!left lsl 1) - bl_count.(i)
            ; if !left < 0 then raise Invalid_huffman
          done
          ; if !left > 0 && (kind = CODES || !max != 1) then
              raise Invalid_huffman
          ; let min = ref 1 in
            (try
               while !min <= 15 do
                 if bl_count.(!min) != 0 then raise_notrace Break
                 ; incr min
               done
             with Break -> ())
            ; let offs = Array.make 16 0 in
              for idx = 1 to 14 do
                offs.(idx + 1) <- offs.(idx) + bl_count.(idx)
              done
              ; let count = offs.(!max) + bl_count.(!max) in
                let work = Array.make count 0 in
                for sym = 0 to codes - 1 do
                  let l = table.(off + sym) in
                  if l <> 0 then (
                    work.(offs.(l)) <- sym
                    ; offs.(l) <- offs.(l) + 1)
                done
                ; let root =
                    ref (match kind with LENS -> 9 | DISTS -> 6 | CODES -> 7)
                  in
                  if !root > !max then root := !max
                  ; if !root < !min then root := !min
                  ; let size =
                      if !max <= !root then 1 lsl !max
                      else
                        match kind with
                        | LENS -> 852
                        | DISTS -> 592
                        | CODES -> 1 lsl !max in
                    let tbl = Array.make size 0 in
                    let rec backward huff incr =
                      if huff land incr <> 0 then backward huff (incr lsr 1)
                      else incr in
                    let huff = ref 0
                    and sym = ref 0
                    and len = ref !min
                    and next = ref 0
                    and curr = ref !root
                    and drop = ref 0
                    and low = ref (-1)
                    and mask = (1 lsl !root) - 1
                    and fill_size = ref 0
                    and finished = ref false in
                    while not !finished do
                      let value = work.(!sym) in
                      let entry = (!len lsl 15) lor value in
                      let step = 1 lsl (!len - !drop) in
                      fill_size := 1 lsl !curr
                      ; let rec go fill =
                          let fill = fill - step in
                          tbl.(!next + (!huff lsr !drop) + fill) <- entry
                          ; if fill <> 0 then go fill in
                        go !fill_size
                        ; let inc = backward !huff (1 lsl (!len - 1)) in
                          huff :=
                            if inc <> 0 then (!huff land (inc - 1)) + inc else 0
                          ; incr sym
                          ; bl_count.(!len) <- bl_count.(!len) - 1
                          ; if bl_count.(!len) = 0 then
                              if !len = !max then finished := true
                              else len := table.(off + work.(!sym))
                          ; if
                              (not !finished)
                              && !len > !root
                              && !huff land mask <> !low
                            then begin
                              if !drop = 0 then drop := !root
                              ; next := !next + !fill_size
                              ; curr := !len - !drop
                              ; let left = ref (1 lsl !curr) in
                                begin try
                                  while !curr + !drop < !max do
                                    left := !left - bl_count.(!curr + !drop)
                                    ; if !left <= 0 then raise_notrace Break
                                    ; incr curr
                                    ; left := !left lsl 1
                                  done
                                with Break -> ()
                                end
                                ; low := !huff land mask
                                ; tbl.(!low) <-
                                    _link_flag lor (!curr lsl 15) lor !next
                            end
                    done
                    ; tbl, !root, !max
        end

  let resolve lookup hold =
    let value = lookup.Lookup.t.(hold land lookup.Lookup.m) in
    if value land _link_flag == 0 then value
    else
      let sub = (value lsr 15) land 0x1f in
      lookup.Lookup.t.((value land 0x7fff)
                       + ((hold lsr lookup.Lookup.root) land ((1 lsl sub) - 1)))
  [@@inline]

  (* allocation *)

  type decoder = {
      src: src
    ; mutable i: bigstring
    ; mutable i_pos: int
    ; mutable i_len: int
    ; mutable hold: int
    ; mutable bits: int
    ; mutable last: bool
    ; o: bigstring
    ; t: bigstring
    ; mutable t_need: int
    ; mutable t_len: int
    ; mutable o_pos: int
    ; mutable l: int (* literal / length *)
    ; mutable d: int (* distance *)
    ; mutable literal: Lookup.t
    ; mutable distance: Lookup.t
    ; mutable jump: jump
    ; w: WInf.t
    ; mutable s: state
    ; mutable k: decoder -> ret
  }

  and state =
    | Header
    | Table of {hlit: int; hdist: int; hclen: int}
    | Inflate_table of {t: int array; l: int; r: int array; h: int * int * int}
    | Inflate
    | Slow
    | Flat_header
    | Dynamic_header
    | Flat
    | End_of_inflate

  and jump = Length | Extra_length | Distance | Extra_distance | Write
  and ret = Await | Flush | End | K | Malformed of string

  let malformedf fmt = kstrf (fun s -> Malformed s) fmt

  (* End of input [eoi] is signalled by [d.i_pos = 0] and [d.i_len = min_int]
     which implies [i_rem d < 0] is [true]. *)

  let eoi d =
    d.i <- bigstring_empty
    ; d.i_pos <- 0
    ; d.i_len <- min_int

  let final _ = End

  (* errors. *)

  let err_unexpected_end_of_input d =
    eoi d
    ; d.k <- final
    ; malformedf "Unexpected end of input"

  let err_invalid_kind_of_block d =
    eoi d
    ; d.k <- final
    ; malformedf "Invalid kind of block"

  let err_invalid_dictionary d =
    eoi d
    ; d.k <- final
    ; malformedf "Invalid dictionary"

  let err_invalid_complement_of_length d =
    eoi d
    ; d.k <- final
    ; malformedf "Invalid complement of length"

  let err_invalid_distance d =
    eoi d
    ; d.k <- final
    ; malformedf "Invalid distance"

  let err_invalid_distance_code d =
    eoi d
    ; d.k <- final
    ; malformedf "Invalid distance code"

  (* remaining bytes to read [d.i]. *)
  let i_rem d = d.i_len - d.i_pos + 1 [@@inline]

  (* set [d.i] with [s]. *)
  let src d s j l =
    if j < 0 || l < 0 || j + l > bigstring_length s then invalid_bounds j l
    ; if l == 0 then eoi d
      else (
        d.i <- s
        ; d.i_pos <- j
        ; d.i_len <- j + l - 1)

  (* get new input in [d.i] and [k]ontinue. *)
  let refill k d =
    match d.src with
    | `String _ -> eoi d ; k d
    | `Channel ic ->
      let res = input_bigstring ic d.i 0 (bigstring_length d.i) in
      src d d.i 0 res ; k d
    | `Manual ->
      d.k <- k
      ; Await

  (* ensure to call [k] with, at least, [n] bits available. *)
  let rec c_peek_bits n k d =
    if d.bits >= n then k d
    else
      let rem = i_rem d in

      if rem <= 0 then
        if rem < 0 (* end of input *) then err_unexpected_end_of_input d
        else refill (c_peek_bits n k) d (* allocation *)
      else
        let byte = unsafe_get_uint8 d.i d.i_pos in
        d.i_pos <- d.i_pos + 1
        ; d.hold <- d.hold lor (byte lsl d.bits)
        ; d.bits <- d.bits + 8
        ; if d.bits >= n then k d else c_peek_bits n k d

  let t_need d n =
    d.t_len <- 0
    ; d.t_need <- n

  let rec t_fill k d =
    let blit d len =
      unsafe_blit d.i d.i_pos d.t d.t_len len
      ; d.i_pos <- d.i_pos + len
      ; d.t_len <- d.t_len + len in
    let rem = i_rem d in
    if rem < 0 then k d (* TODO *)
    else
      let need = d.t_need - d.t_len in
      if rem < need then (
        blit d rem
        ; refill (t_fill k) d)
      else (
        blit d need
        ; d.t_need <- 0
        ; k d)

  let reverse_bits bits =
    let t =
      [|
         0x00; 0x80; 0x40; 0xC0; 0x20; 0xA0; 0x60; 0xE0; 0x10; 0x90; 0x50; 0xD0
       ; 0x30; 0xB0; 0x70; 0xF0; 0x08; 0x88; 0x48; 0xC8; 0x28; 0xA8; 0x68; 0xE8
       ; 0x18; 0x98; 0x58; 0xD8; 0x38; 0xB8; 0x78; 0xF8; 0x04; 0x84; 0x44; 0xC4
       ; 0x24; 0xA4; 0x64; 0xE4; 0x14; 0x94; 0x54; 0xD4; 0x34; 0xB4; 0x74; 0xF4
       ; 0x0C; 0x8C; 0x4C; 0xCC; 0x2C; 0xAC; 0x6C; 0xEC; 0x1C; 0x9C; 0x5C; 0xDC
       ; 0x3C; 0xBC; 0x7C; 0xFC; 0x02; 0x82; 0x42; 0xC2; 0x22; 0xA2; 0x62; 0xE2
       ; 0x12; 0x92; 0x52; 0xD2; 0x32; 0xB2; 0x72; 0xF2; 0x0A; 0x8A; 0x4A; 0xCA
       ; 0x2A; 0xAA; 0x6A; 0xEA; 0x1A; 0x9A; 0x5A; 0xDA; 0x3A; 0xBA; 0x7A; 0xFA
       ; 0x06; 0x86; 0x46; 0xC6; 0x26; 0xA6; 0x66; 0xE6; 0x16; 0x96; 0x56; 0xD6
       ; 0x36; 0xB6; 0x76; 0xF6; 0x0E; 0x8E; 0x4E; 0xCE; 0x2E; 0xAE; 0x6E; 0xEE
       ; 0x1E; 0x9E; 0x5E; 0xDE; 0x3E; 0xBE; 0x7E; 0xFE; 0x01; 0x81; 0x41; 0xC1
       ; 0x21; 0xA1; 0x61; 0xE1; 0x11; 0x91; 0x51; 0xD1; 0x31; 0xB1; 0x71; 0xF1
       ; 0x09; 0x89; 0x49; 0xC9; 0x29; 0xA9; 0x69; 0xE9; 0x19; 0x99; 0x59; 0xD9
       ; 0x39; 0xB9; 0x79; 0xF9; 0x05; 0x85; 0x45; 0xC5; 0x25; 0xA5; 0x65; 0xE5
       ; 0x15; 0x95; 0x55; 0xD5; 0x35; 0xB5; 0x75; 0xF5; 0x0D; 0x8D; 0x4D; 0xCD
       ; 0x2D; 0xAD; 0x6D; 0xED; 0x1D; 0x9D; 0x5D; 0xDD; 0x3D; 0xBD; 0x7D; 0xFD
       ; 0x03; 0x83; 0x43; 0xC3; 0x23; 0xA3; 0x63; 0xE3; 0x13; 0x93; 0x53; 0xD3
       ; 0x33; 0xB3; 0x73; 0xF3; 0x0B; 0x8B; 0x4B; 0xCB; 0x2B; 0xAB; 0x6B; 0xEB
       ; 0x1B; 0x9B; 0x5B; 0xDB; 0x3B; 0xBB; 0x7B; 0xFB; 0x07; 0x87; 0x47; 0xC7
       ; 0x27; 0xA7; 0x67; 0xE7; 0x17; 0x97; 0x57; 0xD7; 0x37; 0xB7; 0x77; 0xF7
       ; 0x0F; 0x8F; 0x4F; 0xCF; 0x2F; 0xAF; 0x6F; 0xEF; 0x1F; 0x9F; 0x5F; 0xDF
       ; 0x3F; 0xBF; 0x7F; 0xFF
      |] in
    t.(bits)
  [@@inline]

  let fixed_lit, fixed_dist =
    let tbl_lit =
      Array.init 288 @@ fun n ->
      if n < 144 then 8 else if n < 256 then 9 else if n < 280 then 7 else 8
    in
    let tbl_dist =
      let res = Array.make (1 lsl 5) 0 in
      Array.iteri
        (fun i _ -> res.(i) <- (5 lsl 15) lor reverse_bits (i lsl 3))
        res
      ; res in
    let tbl_lit, root_lit, max_lit = huffman LENS tbl_lit 0 288 in
    Lookup.make tbl_lit ~root:root_lit max_lit, Lookup.make tbl_dist 5

  let checksum d = WInf.checksum d.w

  let rec flat d =
    let len = min (min (i_rem d) d.l) (bigstring_length d.o - d.o_pos) in
    WInf.blit d.w d.i d.i_pos d.o d.o_pos len

    ; d.o_pos <- d.o_pos + len
    ; d.i_pos <- d.i_pos + len
    ; d.l <- d.l - len

    ; if d.l == 0 then
        if d.last then (
          d.s <- End_of_inflate
          ; K)
        else (
          d.s <- Header
          ; K)
      else
        match i_rem d, bigstring_length d.o - d.o_pos with
        | 0, _ -> (
          match d.src with
          | `String _ ->
            eoi d
            ; err_unexpected_end_of_input d
          | `Channel ic ->
            let len = input_bigstring ic d.i 0 (bigstring_length d.i) in
            src d d.i 0 len ; flat d
            (* XXX(dinosaure): check this branch. TODO! *)
          | `Manual -> Await)
        | _, 0 -> Flush
        | _, _ -> assert false

  let flat_header d =
    let k d =
      let t_pos = ref 0 in
      let hold = ref d.hold in
      let bits = ref d.bits in
      let len = ref 0 and nlen = ref 0xffff in

      let consume () =
        if !bits < 8 then (
          hold := (unsafe_get_uint8 d.t !t_pos lsl !bits) lor !hold
          ; bits := !bits + 8
          ; incr t_pos) in

      consume ()
      ; len := !hold land 0xff
      ; hold := !hold lsr 8
      ; bits := !bits - 8

      ; consume ()
      ; len := ((!hold land 0xff) lsl 8) lor !len
      ; hold := !hold lsr 8
      ; bits := !bits - 8

      ; consume ()
      ; nlen := !hold land 0xff
      ; hold := !hold lsr 8
      ; bits := !bits - 8

      ; consume ()
      ; nlen := ((!hold land 0xff) lsl 8) lor !nlen
      ; hold := !hold lsr 8
      ; bits := !bits - 8

      ; if !nlen != 0xffff - !len then err_invalid_complement_of_length d
        else (
          d.hold <- 0
          ; d.bits <- 0
          ; d.l <- !len
          ; d.s <- Flat
          ; flat d) in
    d.hold <- d.hold lsr (d.bits land 7)
    ; (* XXX(cfcs): diff between [d.bits] and [d.bits round down to nearest multiple of 8]. *)
      let truncated_bits = d.bits land lnot 7 in

      (* XXX(cfcs): round down to nearest multiple of 8, logical equivalents:
         d.bits land (lnot (8 - 1))
         d.bits land (lnot 7)

         For some reason saving this variable locally instead of accessing [d.bits] twice
         shaves off one instruction when compiling with [flambda]. *)
      d.bits <- truncated_bits
      ; let required = 4 - (truncated_bits asr 3) in
        d.s <- Flat_header
        ; t_need d required
        ; t_fill k d

  let rec c_put_byte byte k d =
    if d.o_pos < bigstring_length d.o then (
      unsafe_set_uint8 d.o d.o_pos byte
      ; WInf.add d.w byte
      ; d.o_pos <- d.o_pos + 1
      ; k d)
    else (
      d.k <- c_put_byte byte k (* allocation *)
      ; Flush)

  let slow_inflate lit dist jump d =
    let rec c_peek_bits n k d =
      if d.bits >= n then k d
      else
        let rem = i_rem d in

        if rem <= 0 then
          if rem < 0 (* end of input *) then
            let v = resolve lit d.hold in
            let is_end_of_block =
              v land Lookup.mask == 256 && v lsr 15 <= d.bits && d.last in
            if is_end_of_block then k d else err_unexpected_end_of_input d
          else refill (c_peek_bits n k) d (* allocation *)
        else
          let byte = unsafe_get_uint8 d.i d.i_pos in
          d.i_pos <- d.i_pos + 1
          ; d.hold <- d.hold lor (byte lsl d.bits)
          ; d.bits <- d.bits + 8
          ; if d.bits >= n && i_rem d >= 0 then k d else c_peek_bits n k d in

    match jump with
    | Length ->
      let k d =
        let v = resolve lit d.hold in
        let value = v land Lookup.mask in
        let len = v lsr 15 in
        d.hold <- d.hold lsr len
        ; d.bits <- d.bits - len

        ; if value < 256 then
            let k d =
              d.s <- Inflate
              ; (* allocation *)
                K in
            c_put_byte value k d
          else if value == 256 then
            if d.last then (
              d.s <- End_of_inflate
              ; K
                (* XXX(dinosaure): [K] is needed here to save remaining byte(s) correctly
                   in [End_of_inflate] state. *))
            else (
              d.s <- Header
              ; K)
          else (
            d.l <- value - 257
            ; d.jump <- Extra_length
            ; d.s <- Inflate (* allocation *)
            ; K) in

      (* XXX(dinosaure): this is necessary where [EOB] is not necessary the
         longest code. So we can occur the case where we are at the end of the
         input and have the [EOB] code, but not enough to have [lit.Lookup.l]
         bits:

         - previously, we just ask more input
         - now, we check if [d.hold] is [EOB]: assumption, codes are prefix free
           AND we reach end of input.

         TODO: optimize this branch! *)
      c_peek_bits lit.Lookup.l k d
    | Extra_length ->
      let len = _extra_lbits.(d.l) in
      let k d =
        let extra = d.hold land ((1 lsl len) - 1) in
        d.hold <- d.hold lsr len
        ; d.bits <- d.bits - len
        ; d.l <- _base_length.(d.l land 0x1f) + 3 + extra
        ; d.jump <- Distance
        ; d.s <- Inflate
        ; (* allocation *)
          K in
      c_peek_bits len k d
    | Distance ->
      let k d =
        let v = resolve dist d.hold in
        let value = v land Lookup.mask in
        let len = v lsr 15 in

        d.hold <- d.hold lsr len
        ; d.bits <- d.bits - len
        ; d.d <- value
        ; d.jump <- Extra_distance
        ; d.s <- Inflate
        ; (* allocation *)
          K in
      c_peek_bits dist.Lookup.l k d
    | Extra_distance ->
      let len = _extra_dbits.(d.d land 0x1f) in
      let k d =
        let extra = d.hold land ((1 lsl len) - 1) in
        d.hold <- d.hold lsr len
        ; d.bits <- d.bits - len
        ; d.d <- _base_dist.(d.d) + 1 + extra
        ; d.jump <- Write
        ; d.s <- Inflate
        ; (* allocation *)
          K in
      c_peek_bits len k d
    | Write ->
      if d.d == 0 then err_invalid_distance_code d
      else if d.d > WInf.have d.w then err_invalid_distance d
      else
        let len = min d.l (bigstring_length d.o - d.o_pos) in
        let off = WInf.mask (d.w.WInf.w - d.d) in
        let pre = WInf.max - off in
        let rst = len - pre in
        if rst > 0 then (
          WInf.blit d.w d.w.WInf.raw off d.o d.o_pos pre
          ; WInf.blit d.w d.w.WInf.raw 0 d.o (d.o_pos + pre) rst)
        else WInf.blit d.w d.w.WInf.raw off d.o d.o_pos len
        ; d.o_pos <- d.o_pos + len
        ; if d.l - len == 0 then (
            d.jump <- Length
            ; d.s <- Inflate (* allocation *)
            ; K)
          else (
            d.l <- d.l - len
            ; d.s <- Inflate (* allocation *)
            ; Flush)

  let inflate lit dist jump d =
    let exception End in
    let exception Invalid_distance in
    let exception Invalid_distance_code in
    let hold = ref (Nativeint.of_int d.hold) in
    let bits = ref d.bits in
    let jump = ref jump in
    let i_pos = ref d.i_pos in
    let o_pos = ref d.o_pos in

    let lit_mask = Nativeint.of_int lit.Lookup.m in
    let dist_mask = Nativeint.of_int dist.Lookup.m in
    let lit_root = lit.Lookup.root in
    let dist_root = dist.Lookup.root in

    (* XXX(dinosaure): 2 jumps were done in this hot-loop:
       1- [while],
       2- [match .. with]).

       A [let rec length = .. and extra_length = ..] can be optimized by
       [flambda]. We should replace [match .. with] by this design. TODO. *)
    try
      while d.i_len - !i_pos + 1 > 1 && !o_pos < bigstring_length d.o do
        match !jump with
        | Length ->
          if !bits < lit.Lookup.l then (
            hold :=
              Nativeint.logor !hold
                Nativeint.(
                  shift_left (of_int (unsafe_get_uint8 d.i !i_pos)) !bits)
            ; bits := !bits + 8
            ; incr i_pos
            ; hold :=
                Nativeint.logor !hold
                  Nativeint.(
                    shift_left (of_int (unsafe_get_uint8 d.i !i_pos)) !bits)
            ; bits := !bits + 8
            ; incr i_pos)
          ; let code =
              let h = Nativeint.(to_int (logand !hold lit_mask)) in
              let v = lit.Lookup.t.(h) in
              if v land _link_flag == 0 then v
              else
                let i = v land 0x7fff in
                let m =
                  let open Nativeint in
                  sub (shift_left 1n ((v lsr 15) land 0x1f)) 1n in
                let i =
                  let open Nativeint in
                  i + to_int (logand (shift_right_logical !hold lit_root) m)
                in
                lit.Lookup.t.(i) in
            let value = code land Lookup.mask in
            let len = code lsr 15 in
            hold := Nativeint.shift_right_logical !hold len
            ; bits := !bits - len

            ; if value < 256 then (
                unsafe_set_uint8 d.o !o_pos value
                ; WInf.add d.w value
                ; incr o_pos (* ; jump := Length *))
              else if value == 256 then raise_notrace End
              else (
                jump := Extra_length
                ; d.l <- value - 257)
        | Extra_length ->
          let len = _extra_lbits.(d.l) in
          if !bits < len then (
            hold :=
              Nativeint.logor !hold
                Nativeint.(
                  shift_left (of_int (unsafe_get_uint8 d.i !i_pos)) !bits)
            ; bits := !bits + 8
            ; incr i_pos)
          ; let extra =
              Nativeint.(to_int (logand !hold (sub (shift_left 1n len) 1n)))
            in

            hold := Nativeint.shift_right_logical !hold len
            ; bits := !bits - len
            ; d.l <- _base_length.(d.l land 0x1f) + 3 + extra
            ; jump := Distance
        | Distance ->
          if !bits < dist.Lookup.l then (
            hold :=
              Nativeint.logor !hold
                Nativeint.(
                  shift_left (of_int (unsafe_get_uint8 d.i !i_pos)) !bits)
            ; bits := !bits + 8
            ; incr i_pos
            ; hold :=
                Nativeint.logor !hold
                  Nativeint.(
                    shift_left (of_int (unsafe_get_uint8 d.i !i_pos)) !bits)
            ; bits := !bits + 8
            ; incr i_pos)
          ; let code =
              let v =
                dist.Lookup.t.(Nativeint.(to_int (logand !hold dist_mask)))
              in
              if v land _link_flag == 0 then v
              else
                let i = v land 0x7fff in
                let m =
                  let open Nativeint in
                  sub (shift_left 1n ((v lsr 15) land 0x1f)) 1n in
                let i =
                  let open Nativeint in
                  i + to_int (logand (shift_right_logical !hold dist_root) m)
                in
                dist.Lookup.t.(i) in
            let value = code land Lookup.mask in
            let len = code lsr 15 in

            hold := Nativeint.shift_right_logical !hold len
            ; bits := !bits - len
            ; d.d <- value
            ; jump := Extra_distance
        | Extra_distance ->
          let len = _extra_dbits.(d.d land 0x1f) in
          if !bits < len then (
            hold :=
              Nativeint.logor !hold
                Nativeint.(
                  shift_left (of_int (unsafe_get_uint8 d.i !i_pos)) !bits)
            ; bits := !bits + 8
            ; incr i_pos
            ; hold :=
                Nativeint.logor !hold
                  Nativeint.(
                    shift_left (of_int (unsafe_get_uint8 d.i !i_pos)) !bits)
            ; bits := !bits + 8
            ; incr i_pos)
          ; let extra =
              Nativeint.(to_int (logand !hold (sub (shift_left 1n len) 1n)))
            in
            hold := Nativeint.shift_right_logical !hold len
            ; bits := !bits - len
            ; d.d <- _base_dist.(d.d) + 1 + extra

            ; jump := Write
        | Write ->
          if d.d == 0 then raise_notrace Invalid_distance_code
          ; if d.d > WInf.have d.w then raise_notrace Invalid_distance

          ; (* if d.d > WInf.have d.w then raise Invalid_distance ;
               XXX(dinosaure): [WInf.have] does not tell me the truth where
               we need a read cursor in [WInf.t] for that. *)
            let len = min d.l (bigstring_length d.o - !o_pos) in
            let off = WInf.mask (d.w.WInf.w - d.d) in

            (if d.d == 1 then
               let v = unsafe_get_uint8 d.w.WInf.raw off in
               WInf.fill d.w v d.o !o_pos len
             else
               let off = WInf.mask (d.w.WInf.w - d.d) in
               let pre = WInf.max - off in
               let rst = len - pre in
               if rst > 0 then (
                 WInf.blit d.w d.w.WInf.raw off d.o !o_pos pre
                 ; WInf.blit d.w d.w.WInf.raw 0 d.o (!o_pos + pre) rst)
               else WInf.blit d.w d.w.WInf.raw off d.o !o_pos len)
            ; o_pos := !o_pos + len
              ; if d.l - len == 0 then jump := Length else d.l <- d.l - len
      done

      ; d.hold <- Nativeint.to_int !hold
      ; d.bits <- !bits
      ; d.i_pos <- !i_pos
      ; d.o_pos <- !o_pos
      ; d.jump <- !jump
      ; d.k <- slow_inflate lit dist !jump
      ; (* allocation *)
        d.s <- Slow

      ; if i_rem d > 0 then if d.o_pos == bigstring_length d.o then Flush else K
        else
          match d.src with
          | `String _ -> eoi d ; K
          (* XXX(dinosaure): [K] is required here mostly because the semantic
             of the hot-loop. If we reach end of input, we may have some
             trailing bits in [d.hold] and we need to process them.

             [slow_inflate] is more precise (but... slow) and will consume
             them to reach [End_of_inflate] then correctly. *)
          | `Channel ic ->
            let len = input_bigstring ic d.i 0 (bigstring_length d.i) in
            src d d.i 0 len ; K
            (* XXX(dinosaure): should work fine! But it
               needs check. *)
          | `Manual -> K
    with
    | End ->
      d.hold <- Nativeint.to_int !hold
      ; d.bits <- !bits
      ; d.i_pos <- !i_pos
      ; d.o_pos <- !o_pos

      ; if d.last then (
          d.s <- End_of_inflate
          ; K
            (* XXX(dinosaure): [K] is needed here to save remaining byte(s) correctly
               in [End_of_inflate] state. *))
        else (
          d.s <- Header
          ; K)
    | Invalid_distance -> err_invalid_distance d
    | Invalid_distance_code -> err_invalid_distance_code d

  let fixed d =
    let lit, dist = fixed_lit, fixed_dist in
    d.literal <- lit
    ; d.distance <- dist
    ; d.jump <- Length
    ; d.s <- Inflate
    ; (* allocation *)
      inflate lit dist Length d

  (* XXX(dinosaure): [huffman] can raise an exception. *)
  let make_table t hlit hdist d =
    try
      if t.(256) == 0 then raise_notrace Invalid_huffman

      ; (* XXX(dinosaure): an huffman tree MUST have at least an End-Of-Block
           symbol. *)
        let t_lit, root_lit, l_lit = huffman LENS t 0 hlit in
        let t_dist, root_dist, l_dist = huffman DISTS t hlit hdist in

        let lit = Lookup.make t_lit ~root:root_lit l_lit in
        let dist = Lookup.make t_dist ~root:root_dist l_dist in

        d.literal <- lit
        ; d.distance <- dist
        ; d.jump <- Length
        ; d.s <- Inflate
        ; (* allocation *)
          inflate lit dist Length d
    with Invalid_huffman -> err_invalid_dictionary d

  let inflate_table d =
    let[@warning "-partial-match"] (Inflate_table
                                      {
                                        t
                                      ; l= max_bits
                                      ; r= res
                                      ; h= hlit, hdist, _
                                      }) =
      d.s in
    let max_res = hlit + hdist in
    let mask = (1 lsl max_bits) - 1 in
    let get k d =
      let len, v =
        t.(d.hold land mask) lsr 15, t.(d.hold land mask) land ((1 lsl 15) - 1)
      in
      d.hold <- d.hold lsr len
      ; d.bits <- d.bits - len
      ; k v d in
    let get k d = c_peek_bits max_bits (get k) d in
    let get_bits n k d =
      let k d =
        let v = d.hold land ((1 lsl n) - 1) in
        d.hold <- d.hold lsr n
        ; d.bits <- d.bits - n
        ; k v d in
      c_peek_bits n k d in
    let ret r d = make_table r hlit hdist d in
    (* XXX(dinosaure): [prv] and [i] are stored as associated env of [go]. We
       can not retake them from [d.s]. *)
    let rec record i copy len d =
      if i + copy > max_res then err_invalid_dictionary d
      else (
        for x = 0 to copy - 1 do
          res.(i + x) <- len
        done
        ; if i + copy < max_res then get (fun d -> go (i + copy) d) d
          else ret res d)
    and go i v d =
      if v < 16 then (
        res.(i) <- v
        ; if succ i < max_res then get (fun d -> go (succ i) d) d else ret res d)
      else if v == 16 then
        let k v d = record i (v + 3) res.(i - 1) d in
        if i == 0 then err_invalid_dictionary d else get_bits 2 k d
      else if v == 17 then
        let k v d = record i (v + 3) 0 d in
        get_bits 3 k d
      else if v == 18 then
        let k v d = record i (v + 11) 0 d in
        get_bits 7 k d
      else assert false
      (* TODO: really never occur? *) in
    let k v d = go 0 v d in
    get k d

  (* XXX(dinosaure): previous design asks to load [hclen * 3] bits, however, in
     a specific context, it can oveflow [hold]. So new design is to ensure to
     have enough bytes to inflate huffman tree. *)

  let table d =
    let[@warning "-partial-match"] (Table {hlit; hdist; hclen}) = d.s in
    let hold = ref d.hold in
    let bits = ref d.bits in
    let t_pos = ref 0 in
    let i = ref 0 in

    let res = Array.make 19 0 in

    while !i < hclen do
      if !bits < 3 then (
        hold := !hold lor (unsafe_get_uint8 d.t !t_pos lsl !bits)
        ; bits := !bits + 8
        ; incr t_pos)
      ; let code = !hold land 0x7 in
        res.(zigzag.(!i)) <- code
        ; hold := !hold lsr 3
        ; bits := !bits - 3
        ; incr i
    done

    ; try
        let t, _, l = huffman CODES res 0 19 in

        d.hold <- !hold
        ; d.bits <- !bits
        ; (* assert (!t_pos == d.t_len) ; *)
          d.t_len <- 0
        ; d.t_need <- 0
        ; d.s <-
            Inflate_table
              {t; l; r= Array.make (hlit + hdist) 0; h= hlit, hdist, hclen}
        ; inflate_table d
      with Invalid_huffman -> err_invalid_dictionary d

  let ( // ) x y =
    if y < 0 then raise Division_by_zero
    else if x > 0 then 1 + ((x - 1) / y)
    else 0
  [@@inline]

  let dynamic d =
    let l_header d =
      let t_pos = ref 0 in

      while d.t_len > 0 do
        d.hold <- d.hold lor (unsafe_get_uint8 d.t !t_pos lsl d.bits)
        ; d.bits <- d.bits + 8
        ; incr t_pos
        ; d.t_len <- d.t_len - 1
      done

      ; let hlit = (d.hold land 0x1f) + 257 in
        let hdist = ((d.hold land 0x3e0) lsr 5) + 1 in
        let hclen = ((d.hold land 0x3c00) lsr 10) + 4 in

        d.s <- Table {hlit; hdist; hclen}
        ; d.hold <- d.hold lsr 14
        ; d.bits <- d.bits - 14

        ; (* XXX(dinosaure): we ensure to have enough bytes to start to inflate
             huffman tree. *)
          let k d =
            let rem = i_rem d in

            if rem < 0 then err_unexpected_end_of_input d
            else (
              t_need d (((hclen * 3) - d.bits) // 8)
              ; t_fill table d) in
          k d in
    let required = (14 - d.bits) // 8 in
    d.s <- Dynamic_header
    ; t_need d required
    ; t_fill l_header d

  let decode_k d =
    match d.s with
    | Header ->
      (* XXX(dinosaure): check this code, we should need a [k]ontinuation. *)
      let l_header d =
        assert (d.bits >= 3)
        ; (* allocation *)
          let last = d.hold land 1 == 1 in
          let k =
            match (d.hold land 0x6) lsr 1 with
            | 0 -> flat_header
            | 1 -> fixed
            | 2 -> dynamic
            | 3 -> err_invalid_kind_of_block
            | _ -> assert false in
          d.last <- last
          ; d.hold <- d.hold lsr 3
          ; d.bits <- d.bits - 3
          ; d.k <- k
          ; k d in
      c_peek_bits 3 l_header d
    | Table _ -> t_fill table d
    | Inflate_table _ -> d.k d
    | Inflate ->
      if i_rem d > 1 then inflate d.literal d.distance d.jump d
      else (
        d.s <- Slow
        ; slow_inflate d.literal d.distance d.jump d)
    | Slow -> d.k d
    | Dynamic_header -> d.k d
    | Flat_header -> d.k d
    | Flat -> flat d
    | End_of_inflate ->
      WInf.tail d.w

      ; if d.bits >= 8 then (
          d.i_pos <- d.i_pos - 1
          ; d.bits <- d.bits - 8
          ; d.hold <- 0 (* XXX(dinosaure): keep? *))
      ; End

  let rec decode d =
    match decode_k d with
    | Await -> `Await
    | Flush -> `Flush
    | End -> `End
    | Malformed err -> `Malformed err
    | K -> decode d

  let dst_rem d = bigstring_length d.o - d.o_pos

  (* TODO: why [+1] disappears? *)

  let src_rem d = i_rem d
  let flush d = d.o_pos <- 0

  let decoder src ~o ~w =
    let i, i_pos, i_len =
      match src with
      | `Manual -> bigstring_empty, 1, 0
      | `String x -> bigstring_of_string x, 0, String.length x - 1
      | `Channel _ -> bigstring_create io_buffer_size, 1, 0 in
    {
      src
    ; i
    ; i_pos
    ; i_len
    ; o
    ; o_pos= 0
    ; t= bigstring_create 10
    ; t_need= 0
    ; t_len= 0
    ; hold= 0
    ; bits= 0
    ; last= false
    ; l= 0
    ; d= 0
    ; literal= fixed_lit
    ; distance= fixed_dist
    ; jump= Length
    ; w= WInf.from w
    ; s= Header
    ; k= decode_k
    }

  let reset d =
    let i, i_pos, i_len =
      match d.src with
      | `Manual -> bigstring_empty, 1, 0
      | `String x -> bigstring_of_string x, 0, String.length x - 1
      | `Channel _ -> bigstring_create io_buffer_size, 1, 0 in
    d.i <- i
    ; d.i_pos <- i_pos
    ; d.i_len <- i_len
    ; d.hold <- 0
    ; d.bits <- 0
    ; d.last <- false
    ; d.o_pos <- 0
    ; d.l <- 0
    ; d.d <- 0
    ; d.literal <- fixed_lit
    ; d.distance <- fixed_dist
    ; d.jump <- Length
    ; d.s <- Header
    ; d.k <- decode_k
    ; WInf.reset d.w

  module Ns = struct
    type decoder = {
        i: bigstring
      ; mutable i_pos: int
      ; i_len: int
      ; mutable hold: int
      ; mutable bits: int
      ; o: bigstring
      ; mutable o_pos: int
      ; o_len: int
    }

    (* errors. *)

    type error =
      [ `Unexpected_end_of_input
      | `Unexpected_end_of_output
      | `Invalid_kind_of_block
      | `Invalid_dictionary
      | `Invalid_complement_of_length
      | `Invalid_distance
      | `Invalid_distance_code ]

    let pp_error ppf e =
      let s =
        match e with
        | `Unexpected_end_of_input -> "Unexpected end of input"
        | `Unexpected_end_of_output -> "Unexpected end of output"
        | `Invalid_kind_of_block -> "Invalid kind of block"
        | `Invalid_dictionary -> "Invalid dictionary"
        | `Invalid_complement_of_length -> "Invalid complement of length"
        | `Invalid_distance -> "Invalid distance"
        | `Invalid_distance_code -> "Invalid distance code" in
      Format.fprintf ppf "%s" s

    exception Malformed of error

    let err_unexpected_end_of_input () =
      raise (Malformed `Unexpected_end_of_input)

    let err_unexpected_end_of_output () =
      raise (Malformed `Unexpected_end_of_output)

    let err_invalid_kind_of_block () = raise (Malformed `Invalid_kind_of_block)
    let err_invalid_dictionary () = raise (Malformed `Invalid_dictionary)

    let err_invalid_complement_of_length () =
      raise (Malformed `Invalid_complement_of_length)

    let err_invalid_distance () = raise (Malformed `Invalid_distance)
    let err_invalid_distance_code () = raise (Malformed `Invalid_distance_code)

    (* remaining bytes to read [d.i]. *)
    let i_rem d = d.i_len - d.i_pos [@@inline]

    let _slow_blit src src_off dst dst_off len =
      for i = 0 to len - 1 do
        let v = unsafe_get_uint8 src (src_off + i) in
        unsafe_set_uint8 dst (dst_off + i) v
      done

    let _blit src src_off dst dst_off len =
      if dst_off - src_off < 4 then _slow_blit src src_off dst dst_off len
      else
        let len0 = len land 3 in
        let len1 = len asr 2 in

        for i = 0 to len1 - 1 do
          let i = i * 4 in
          let v = unsafe_get_uint32 src (src_off + i) in
          unsafe_set_uint32 dst (dst_off + i) v
        done

        ; for i = 0 to len0 - 1 do
            let i = (len1 * 4) + i in
            let v = unsafe_get_uint8 src (src_off + i) in
            unsafe_set_uint8 dst (dst_off + i) v
          done

    let flat d =
      d.i_pos <- d.i_pos - (d.bits / 8)
      ; d.hold <- 0
      ; d.bits <- 0
      ; if i_rem d < 4 then err_unexpected_end_of_input ()
      ; let len = unsafe_get_uint16_le d.i d.i_pos in
        let nlen = unsafe_get_uint16_le d.i (d.i_pos + 2) in
        d.i_pos <- d.i_pos + 4
        ; if nlen != 0xffff - len then err_invalid_complement_of_length ()
          else (
            if len > i_rem d then err_unexpected_end_of_input ()
            ; if len > d.o_len - d.o_pos then err_unexpected_end_of_output ()
            ; _blit d.i d.i_pos d.o d.o_pos len
            ; d.o_pos <- d.o_pos + len
            ; d.i_pos <- d.i_pos + len)

    let _fill_bits d n =
      if d.bits < n then
        let rem = i_rem d in
        if rem > 1 then (
          d.hold <- d.hold lor (unsafe_get_uint16_le d.i d.i_pos lsl d.bits)
          ; d.i_pos <- d.i_pos + 2
          ; d.bits <- d.bits + 16)
        else if rem = 1 then (
          d.hold <- d.hold lor (unsafe_get_uint8 d.i d.i_pos lsl d.bits)
          ; d.i_pos <- d.i_pos + 1
          ; d.bits <- d.bits + 8)
        else err_unexpected_end_of_input ()
    [@@inline]

    let __fill_bits d n =
      if d.bits < n then
        let rem = i_rem d in
        if rem > 1 then (
          d.hold <- d.hold lor (unsafe_get_uint16_le d.i d.i_pos lsl d.bits)
          ; d.i_pos <- d.i_pos + 2
          ; d.bits <- d.bits + 16)
        else if rem = 1 then (
          d.hold <- d.hold lor (unsafe_get_uint8 d.i d.i_pos lsl d.bits)
          ; d.i_pos <- d.i_pos + 1
          ; d.bits <- d.bits + 8)
    [@@inline]

    let pop_bits d n =
      let v = d.hold land ((1 lsl n) - 1) in
      d.hold <- d.hold lsr n
      ; d.bits <- d.bits - n
      ; v
    [@@inline]

    exception End
    exception Invalid_distance
    exception Invalid_distance_code

    let inflate lit dist d =
      try
        let rec inflate_loop () =
          __fill_bits d lit.Lookup.l
          ; let code = resolve lit d.hold in
            let value = code land Lookup.mask in
            let len = code lsr 15 in
            d.hold <- d.hold lsr len
            ; d.bits <- d.bits - len
            ; if value < 256 then (
                if d.o_pos >= d.o_len then err_unexpected_end_of_output ()
                ; unsafe_set_uint8 d.o d.o_pos value
                ; d.o_pos <- d.o_pos + 1
                ; inflate_loop ())
              else if value == 256 then raise_notrace End
              else
                let l = value - 257 in
                let len = _extra_lbits.(l) in
                __fill_bits d len
                ; let extra = pop_bits d len in
                  let l = _base_length.(l land 0x1f) + 3 + extra in
                  __fill_bits d dist.Lookup.l
                  ; let code = resolve dist d.hold in
                    let value = code land Lookup.mask in
                    let len = code lsr 15 in
                    d.hold <- d.hold lsr len
                    ; d.bits <- d.bits - len
                    ; let d_ = value in
                      let len = _extra_dbits.(d_ land 0x1f) in
                      __fill_bits d len
                      ; let extra = pop_bits d len in
                        let d_ = _base_dist.(d_) + 1 + extra in
                        if d_ == 0 then raise_notrace Invalid_distance_code
                        ; if d_ > min d.o_pos (1 lsl 15) then
                            raise_notrace Invalid_distance
                        ; let off = d.o_pos - d_ in
                          if l > d.o_len - d.o_pos then
                            err_unexpected_end_of_output ()
                          ; _blit d.o off d.o d.o_pos l
                          ; d.o_pos <- d.o_pos + l
                          ; inflate_loop () in
        inflate_loop ()
      with
      | End -> ()
      | Invalid_distance -> err_invalid_distance ()
      | Invalid_distance_code -> err_invalid_distance_code ()

    let fixed d = inflate fixed_lit fixed_dist d

    (* XXX(clecat): The table functions are almost a copy of the stream implementation, by
       adapting their code, they should be easily merged *)
    let make_table t hlit hdist d =
      try
        if t.(256) == 0 then raise_notrace Invalid_huffman

        ; (* XXX(dinosaure): an huffman tree MUST have at least an End-Of-Block
             symbol. *)
          let t_lit, root_lit, l_lit = huffman LENS t 0 hlit in
          let t_dist, root_dist, l_dist = huffman DISTS t hlit hdist in

          let lit = Lookup.make t_lit ~root:root_lit l_lit in
          let dist = Lookup.make t_dist ~root:root_dist l_dist in

          inflate lit dist d
      with Invalid_huffman -> err_invalid_dictionary ()

    let inflate_table d t max_bits res (hlit, hdist, _) =
      let max_res = hlit + hdist in
      let mask = (1 lsl max_bits) - 1 in
      let get d =
        _fill_bits d max_bits
        ; let v = t.(d.hold land mask) land Lookup.mask in
          let len = t.(d.hold land mask) lsr 15 in
          d.hold <- d.hold lsr len
          ; d.bits <- d.bits - len
          ; v in
      let get_bits d n = _fill_bits d n ; pop_bits d n in
      let ret r d = make_table r hlit hdist d in
      let rec record i copy len d =
        if i + copy > max_res then err_invalid_dictionary ()
        else (
          for x = 0 to copy - 1 do
            res.(i + x) <- len
          done
          ; if i + copy < max_res then go (i + copy) (get d) d else ret res d)
      and go i v d =
        if v < 16 then (
          res.(i) <- v
          ; if succ i < max_res then go (succ i) (get d) d else ret res d)
        else if v == 16 then
          if i == 0 then err_invalid_dictionary ()
          else
            let v = get_bits d 2 in
            record i (v + 3) res.(i - 1) d
        else if v == 17 then
          let v = get_bits d 3 in
          record i (v + 3) 0 d
        else if v == 18 then
          let v = get_bits d 7 in
          record i (v + 11) 0 d
        else assert false
        (* TODO: really never occur? *) in
      go 0 (get d) d

    let table d hlit hdist hclen =
      let i = ref 0 in
      let res = Array.make 19 0 in

      while !i < hclen do
        _fill_bits d 3
        ; let code = pop_bits d 3 in
          res.(zigzag.(!i)) <- code
          ; incr i
      done
      ; try
          let t, _, l = huffman CODES res 0 19 in
          let r = Array.make (hlit + hdist) 0 in
          let h = hlit, hdist, hclen in
          inflate_table d t l r h
        with Invalid_huffman -> err_invalid_dictionary ()

    let dynamic d =
      _fill_bits d 14
      ; let hlit = pop_bits d 5 + 257 in
        let hdist = pop_bits d 5 + 1 in
        let hclen = pop_bits d 4 + 4 in
        table d hlit hdist hclen

    let rec decode d =
      _fill_bits d 3
      ; let last = pop_bits d 1 == 1 in
        let block_type = pop_bits d 2 in
        (match block_type with
        | 0 -> flat d
        | 1 -> fixed d
        | 2 -> dynamic d
        | 3 -> err_invalid_kind_of_block ()
        | _ -> assert false)
        ; if last then d.i_pos <- d.i_pos - (d.bits lsr 3) else decode d

    let inflate src dst =
      let d =
        {
          i= src
        ; i_pos= 0
        ; i_len= bigstring_length src
        ; o= dst
        ; o_pos= 0
        ; o_len= bigstring_length dst
        ; hold= 0
        ; bits= 0
        } in
      try
        decode d
        ; Ok (d.i_pos, d.o_pos)
      with Malformed e -> Error (e : error :> [> error ])
  end
end

let unsafe_set_cursor d c = d.Inf.w.WInf.w <- c

module T = struct
  module Heap = struct
    type t = {heap: int array; mutable len: int; mutable max: int}

    let make () = {heap= Array.make _heap_size 0; len= 0; max= _heap_size}

    let populate ~length ~freqs tree_lengths ~depth heap =
      (* assert (Array.length tree_lengths = Array.length freqs) ;
         assert (Array.length depth = heap.max) ;
         assert (Array.length heap.heap = heap.max) ;
         assert (heap.max = _heap_size) ;
      *)
      let max_code = ref (-1) in

      (* Construct the initial heap, with least frequent element in
         heap[SMALLEST]. The sons of heap[n] are heap[2*n] and heap[2*n+1].
         heap[0] is not used. *)
      for n = 0 to length - 1 do
        if freqs.(n) <> 0 then (
          heap.len <- heap.len + 1
          ; heap.heap.(heap.len) <- n
          ; max_code := n
          ; depth.(n) <- 0)
        else tree_lengths.(n) <- 0
          (* XXX(dinosaure): we consider that [tree_lengths] can have bad
             informations, so we clean it. However, it was initialized with [0] in
             [T.make] builder. *)
      done

      ; !max_code

    (* The pkzip format requires that at least one distance code exists,
       and that at least one bit should be sent even if there is only one
       possible code. So to avoid special checks later on we force at least
       two codes of non zero frequency. *)
    let pkzip max_code ~freqs ~depth heap =
      let max_code = ref max_code in

      while heap.len < 2 do
        let node = if !max_code < 2 then (incr max_code ; !max_code) else 0 in
        freqs.(node) <- 1
        ; heap.len <- heap.len + 1
        ; heap.heap.(heap.len) <- node
        ; depth.(node) <- 0
      done

      ; !max_code

    let[@inline] smaller freqs n m depth =
      freqs.(n) < freqs.(m) || (freqs.(n) = freqs.(m) && depth.(n) <= depth.(m))

    let pqdownheap ~freqs ~depth heap k =
      let exception Break in
      let v = heap.heap.(k) in
      let j = ref (k lsl 1) in
      let k = ref k in

      (try
         while !j <= heap.len do
           if
             !j < heap.len
             && smaller freqs heap.heap.(!j + 1) heap.heap.(!j) depth
           then incr j
           ; if smaller freqs v heap.heap.(!j) depth then raise_notrace Break
           ; heap.heap.(!k) <- heap.heap.(!j)
           ; k := !j
           ; j := !j lsl 1
         done
       with Break -> ())

      ; heap.heap.(!k) <- v

    let pqremove ~freqs ~depth heap =
      let top = heap.heap.(_smallest) in
      heap.heap.(_smallest) <- heap.heap.(heap.len)
      ; heap.len <- heap.len - 1
      ; pqdownheap ~freqs ~depth heap _smallest
      ; top
  end

  (* Reverse the first len bits of a code, using a straightforward code
     (a faster method would use a table). *)
  let reverse_code code len =
    (* assert (1 <= len && len <= 15); *)
    let res = ref 0 in
    let len = ref len in
    let code = ref code in
    while
      res := !res lor (!code land 1)
      ; code := !code asr 1
      ; res := !res lsl 1
      ; decr len
      ; !len > 0
    do
      ()
    done
    ; !res asr 1

  let generate_codes ~tree_lengths ~max_code ~bl_count =
    let tree_codes = Array.make (Array.length tree_lengths) 0 in
    let next_code = Array.make (_max_bits + 1) 0 in
    let code = ref 0 in

    (* The distribution counts are fist used to generate the code values without
       bit reversal. *)
    for bits = 1 to _max_bits do
      code := (!code + bl_count.(bits - 1)) lsl 1
      ; next_code.(bits) <- !code land 0xffff
    done

    ; (* check that the bit counts in [bl_count] are consistent. The last code
         must be all ones. *)
      assert (!code + bl_count.(_max_bits) - 1 = (1 lsl _max_bits) - 1)

    ; for n = 0 to max_code do
        let len = tree_lengths.(n) in
        if len > 0 then (
          (* Now reverse the bits. *)
          tree_codes.(n) <- reverse_code next_code.(len) len
          ; next_code.(len) <- next_code.(len) + 1)
      done

    ; tree_codes

  let generate_lengths
      ~tree_dads ~tree_lengths ~max_code ~max_length heap ~bl_count =
    (* assert (Array.length bl_count = _max_bits + 1) ;
       assert (Array.for_all ((=) 0) bl_count) ;
    *)

    (* In a first pass, compute the optimal bit lengths (which may overflow in
       the case of the bit length tree). *)
    tree_lengths.(heap.Heap.heap.(heap.max)) <- 0
    ; (* root of the heap. *)
      let overflow = ref 0 in

      Array.fill bl_count 0 (Array.length bl_count) 0

      ; for h = heap.max + 1 to _heap_size - 1 do
          let n = heap.heap.(h) in
          let bits = tree_lengths.(tree_dads.(n)) + 1 in
          let bits =
            if bits > max_length then (incr overflow ; max_length) else bits
          in
          tree_lengths.(n) <- bits

          ; if n <= max_code (* XXX(dinosaure): it's a leaf. *) then
              bl_count.(bits) <- bl_count.(bits) + 1
        done

      ; if
          !overflow != 0
          (* This happends for example on obj2 and pic of the
             Calgary corpus. *)
        then (
          let rec go () =
            let bits = ref (max_length - 1) in
            while bl_count.(!bits) == 0 do
              decr bits
            done
            ; bl_count.(!bits) <- bl_count.(!bits) - 1
            ; bl_count.(!bits + 1) <- bl_count.(!bits + 1) + 2
            ; bl_count.(max_length) <- bl_count.(max_length) - 1

            ; overflow := !overflow - 2

            ; if !overflow > 0 then go () in

          go ()

          ; let h = ref _heap_size in
            for bits = max_length downto 1 do
              let n = ref bl_count.(bits) in

              while !n != 0 do
                decr h
                ; let m = heap.heap.(!h) in
                  if m <= max_code then (
                    if tree_lengths.(m) <> bits then tree_lengths.(m) <- bits
                    ; decr n)
              done
            done)

  type tree = {lengths: int array; max_code: int; tree: Lookup.t}

  let make ~length ?(max_length = _max_bits) freqs ~bl_count =
    let heap = Heap.make () in
    let depth = Array.make ((2 * _l_codes) + 1) 0 in
    let tree_dads = Array.make _heap_size 0 in
    let tree_lengths = Array.make _heap_size 0 in

    let max_code = Heap.populate ~length ~freqs ~depth tree_lengths heap in
    let max_code = Heap.pkzip max_code ~freqs ~depth heap in

    for n = heap.len / 2 downto 1 do
      Heap.pqdownheap ~freqs ~depth heap n
    done

    ; let node = ref length in

      let rec go () =
        let n = Heap.pqremove ~freqs ~depth heap in
        let m = heap.heap.(_smallest) in

        heap.max <- heap.max - 1
        ; heap.heap.(heap.max) <- n
        ; heap.max <- heap.max - 1
        ; heap.heap.(heap.max) <- m

        ; freqs.(!node) <- freqs.(n) + freqs.(m)
        ; depth.(!node) <-
            (if depth.(n) >= depth.(m) then depth.(n) else depth.(m)) + 1
        ; tree_dads.(n) <- !node
        ; tree_dads.(m) <- !node
        ; heap.heap.(_smallest) <- !node
        ; incr node
        ; Heap.pqdownheap ~freqs ~depth heap _smallest

        ; if heap.len >= 2 then go ()
          else (
            heap.max <- heap.max - 1
            ; heap.heap.(heap.max) <- heap.heap.(_smallest)) in

      go ()
      ; generate_lengths ~tree_dads ~tree_lengths ~max_code ~max_length heap
          ~bl_count
      ; let tree_codes = generate_codes ~tree_lengths ~max_code ~bl_count in
        let length = ref 0 in

        let tree =
          Array.map2
            (fun len code ->
              length := max !length len
              ; (len lsl _max_bits) lor code)
            tree_lengths tree_codes in
        {
          lengths= tree_lengths
        ; max_code
        ; tree=
            {Lookup.t= tree; m= (1 lsl !length) - 1; l= !length; root= !length}
        }

  let scan tree_lengths max_code ~bl_freqs =
    let prevlen = ref (-1) in
    let nextlen = ref tree_lengths.(0) in
    let curlen = ref !nextlen in

    let count = ref 0 in

    let max_count = ref 7 in
    let min_count = ref 4 in

    let exception Continue in
    if !nextlen = 0 then (
      max_count := 138
      ; min_count := 3)
    ; tree_lengths.(max_code + 1) <- 0xffff

    ; for n = 0 to max_code do
        curlen := !nextlen
        ; nextlen := tree_lengths.(n + 1)
        ; incr count

        ; try
            if !count < !max_count && !curlen == !nextlen then
              raise_notrace Continue
            else if !count < !min_count then
              bl_freqs.(!curlen) <- bl_freqs.(!curlen) + !count
            else if !curlen != 0 then (
              if !curlen != !prevlen then
                bl_freqs.(!curlen) <- bl_freqs.(!curlen) + 1
              ; bl_freqs.(_rep_3_6) <- bl_freqs.(_rep_3_6) + 1)
            else if !count <= 10 then
              bl_freqs.(_repz_3_10) <- bl_freqs.(_repz_3_10) + 1
            else bl_freqs.(_repz_11_138) <- bl_freqs.(_repz_11_138) + 1

            ; count := 0
            ; prevlen := !curlen

            ; if !nextlen == 0 then (
                max_count := 138
                ; min_count := 3)
              else if !curlen = !nextlen then (
                max_count := 6
                ; min_count := 3)
              else (
                max_count := 7
                ; min_count := 4)
          with Continue -> ()
      done

  let code code lookup = lookup.Lookup.t.(code)
  let bits code len = (len lsl _max_bits) lor code

  let symbols i tree_lengths max_code ~bl_symbols ~bltree =
    let i = ref i in

    let prevlen = ref (-1) in
    let nextlen = ref tree_lengths.(0) in
    let curlen = ref !nextlen in

    let count = ref 0 in

    let max_count = ref 7 in
    let min_count = ref 4 in

    let exception Continue in
    if !nextlen = 0 then (
      max_count := 138
      ; min_count := 3)

    ; for n = 0 to max_code do
        curlen := !nextlen
        ; nextlen := tree_lengths.(n + 1)
        ; incr count

        ; try
            if !count < !max_count && !curlen == !nextlen then
              raise_notrace Continue
            else if !count < !min_count then
              while
                bl_symbols.(!i) <- code !curlen bltree.tree
                ; incr i
                ; decr count
                ; !count != 0
              do
                ()
              done
            else if !curlen != 0 then (
              if !curlen != !prevlen then (
                bl_symbols.(!i) <- code !curlen bltree.tree
                ; incr i
                ; decr count)
              ; bl_symbols.(!i) <- code _rep_3_6 bltree.tree
              ; incr i
              ; bl_symbols.(!i) <- bits (!count - 3) 2
              ; incr i)
            else if !count <= 10 then (
              bl_symbols.(!i) <- code _repz_3_10 bltree.tree
              ; incr i
              ; bl_symbols.(!i) <- bits (!count - 3) 3
              ; incr i)
            else (
              bl_symbols.(!i) <- code _repz_11_138 bltree.tree
              ; incr i
              ; bl_symbols.(!i) <- bits (!count - 11) 7
              ; incr i)

            ; count := 0
            ; prevlen := !curlen

            ; if !nextlen == 0 then (
                max_count := 138
                ; min_count := 3)
              else if !curlen == !nextlen then (
                max_count := 6
                ; min_count := 3)
              else (
                max_count := 7
                ; min_count := 4)
          with Continue -> ()
      done

    ; !i
end

module Queue = struct
  type cmd = int
  type buf = (cmd, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t
  type t = {buf: buf; mutable w: int; mutable r: int; mutable c: int}

  let mask t v = v land (t.c - 1)
  let empty t = t.r = t.w
  let size t = t.w - t.r
  let available t = t.c - (t.w - t.r)
  let full t = size t = t.c
  let length t = size t
  let is_empty t = empty t
  let is_full t = full t

  external unsafe_get : buf -> int -> int = "%caml_ba_ref_1"
  external unsafe_set : buf -> int -> int -> unit = "%caml_ba_set_1"

  exception Full
  exception Empty

  let push_exn t v =
    if (full [@inlined]) t then raise Full
    ; unsafe_set t.buf ((mask [@inlined]) t t.w) v
    ; t.w <- t.w + 1

  let end_with_eob t =
    if not (empty t) then
      unsafe_get t.buf ((mask [@inlined]) t (t.w - 1)) == 256
    else false

  let rem_exn t n =
    if size t >= n then t.w <- t.w - n
    else invalid_arg "You requested too many commands to delete"

  let pop_exn t =
    if (empty [@inlined]) t then raise Empty
    ; let r = unsafe_get t.buf ((mask [@inlined]) t t.r) in
      t.r <- t.r + 1
      ; r

  let peek_exn t =
    if (empty [@inlined]) t then raise Empty
    ; unsafe_get t.buf ((mask [@inlined]) t t.r)

  let unsafe_junk t = t.r <- t.r + 1

  let junk_exn t n =
    if (size [@inlined]) t < n then
      invalid_arg "You want to junk more than what we have"
    ; t.r <- t.r + n

  let copy ~off ~len : cmd =
    assert (len >= 3 && len <= 255 + 3)
    ; assert (off >= 1 && off <= 32767 + 1)
    ; ((len - 3) lsl 16) lor (off - 1) lor 0x2000000
  [@@inline]

  let literal chr = Char.code chr [@@inline]
  let eob = 256

  let cmd = function
    | `Literal chr -> literal chr
    | `Copy (off, len) -> copy ~off ~len
    | `End -> 256

  let code cmd =
    match cmd land 0x2000000 <> 0 with
    | false -> if cmd == 256 then `End else `Literal (Char.chr (cmd land 0xff))
    | true ->
      let off = (cmd land 0xffff) + 1 in
      let len = ((cmd lsr 16) land 0x1ff) + 3 in
      (* XXX(dinosaure): ((1 lsl 9) - 1) - 0xff *)
      `Copy (off, len)

  let blit t buf off len =
    if available t < len then raise Full
    ; let msk = mask t t.w in
      let pre = t.c - msk in
      let rst = len - pre in
      if rst > 0 then (
        for i = 0 to pre - 1 do
          unsafe_set t.buf (msk + i) (unsafe_get_uint8 buf (off + i))
        done
        ; for i = 0 to rst - 1 do
            unsafe_set t.buf i (unsafe_get_uint8 buf (off + pre + i))
          done)
      else
        for i = 0 to len - 1 do
          unsafe_set t.buf (msk + i) (unsafe_get_uint8 buf (off + i))
        done
      ; t.w <- t.w + len

  let create length =
    if not (is_power_of_two length) then
      invalid_arg "Length of queue MUST be a power of two"

    ; {
        buf= Bigarray.Array1.create Bigarray.int Bigarray.c_layout length
      ; w= 0
      ; r= 0
      ; c= length
      }

  let reset t =
    t.w <- 0
    ; t.r <- 0

  let to_list t =
    let res = ref [] in
    let len = size t in
    let msk = mask t t.r in
    let pre = t.c - msk in
    let rst = len - pre in

    if rst > 0 then (
      for i = 0 to pre - 1 do
        res := code (unsafe_get t.buf i) :: !res
      done
      ; for i = 0 to rst - 1 do
          res := code (unsafe_get t.buf i) :: !res
        done)
    else
      for i = 0 to len - 1 do
        res := code (unsafe_get t.buf i) :: !res
      done

    ; List.rev !res

  let ( <.> ) f g x = f (g x)

  let of_list lst =
    let q = create (to_power_of_two (List.length lst)) in
    List.iter (push_exn q <.> cmd) lst
    ; q
end

type literals = int array
type distances = int array

let make_literals () =
  let res = Array.make ((2 * _l_codes) + 1) 0 in
  res.(256) <- 1
  ; res

let succ_literal literals chr =
  literals.(Char.code chr) <- literals.(Char.code chr) + 1

let succ_length literals length =
  assert (length >= 3 && length <= 255 + 3)
  ; literals.(256 + 1 + _length.(length)) <-
      literals.(256 + 1 + _length.(length)) + 1

let make_distances () = Array.make ((2 * _d_codes) + 1) 0

let succ_distance distances distance =
  assert (distance >= 1 && distance <= 32767 + 1)
  ; distances.(_distance (pred distance)) <-
      distances.(_distance (pred distance)) + 1

(* XXX placeholder for me to find myself XXX*)
module Def = struct
  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]

  type dynamic = {
      ltree: T.tree
    ; dtree: T.tree
    ; bltree: T.tree
    ; h_lit: int
    ; h_dst: int
    ; h_len: int
    ; symbols: int array
  }

  let bl_tree ltree dtree ~bl_count =
    let bl_freqs = Array.make ((2 * _bl_codes) + 1) 0 in
    T.scan ltree.T.lengths ltree.T.max_code ~bl_freqs
    ; T.scan dtree.T.lengths dtree.T.max_code ~bl_freqs

    ; let bltree = T.make ~length:_bl_codes ~max_length:7 bl_freqs ~bl_count in
      (* XXX(dinosaure): [T.make] needs [max_length] to avoid generation of a bad
         bltree (limited to 7 bits with extra). *)
      let max_blindex = ref (_bl_codes - 1) in
      let exception Break in
      (try
         while !max_blindex >= 3 do
           if bltree.T.lengths.(zigzag.(!max_blindex)) <> 0 then
             raise_notrace Break
           ; decr max_blindex
         done
       with Break -> ())

      ; !max_blindex, bltree

  let dynamic_of_frequencies :
      literals:int array -> distances:int array -> dynamic =
   fun ~literals:lit_freqs ~distances:dst_freqs ->
    let bl_count = Array.make (_max_bits + 1) 0 in
    let ltree = T.make ~length:_l_codes lit_freqs ~bl_count in
    let dtree = T.make ~length:_d_codes dst_freqs ~bl_count in
    let max_blindex, bltree = bl_tree ltree dtree ~bl_count in
    let bl_symbols = Array.make (_l_codes + _d_codes) 0 in
    let i = T.symbols 0 ltree.T.lengths ltree.T.max_code ~bltree ~bl_symbols in
    let i = T.symbols i dtree.T.lengths dtree.T.max_code ~bltree ~bl_symbols in
    let bl_symbols = Array.sub bl_symbols 0 i in

    {
      h_lit= ltree.T.max_code + 1
    ; h_dst= dtree.T.max_code + 1
    ; h_len= max_blindex + 1
    ; bltree
    ; ltree
    ; dtree
    ; symbols= bl_symbols
    }

  let invalid_encode () = invalid_arg "expected `Await encode"

  type kind = Flat | Fixed | Dynamic of dynamic
  type block = {kind: kind; last: bool}
  type encode = [ `Await | `Flush | `Block of block ]

  let calculate_static_block ~literals ~distances =
    let bits = ref 0 in
    for idx = 0 to _l_codes - 1 do
      if literals.(idx) <> 0 then
        bits := !bits + (literals.(idx) * fst (Lookup.get _static_ltree idx))
    done
    ; for idx = 0 to _d_codes - 1 do
        if distances.(idx) <> 0 then
          bits := !bits + (distances.(idx) + fst (Lookup.get _static_dtree idx))
      done
    ; !bits

  let calculate_dynamic_block dynamic ~literals ~distances =
    (* HLIT + HDST + HCLEN + precode lengths *)
    let bits = ref (5 + 5 + 4 + (dynamic.h_len * 3)) in
    let fn x = bits := !bits + (x lsr _max_bits) in
    Array.iter fn dynamic.symbols
    ; let ltree = dynamic.ltree.T.lengths and dtree = dynamic.dtree.T.lengths in
      for idx = 0 to _l_codes - 1 do
        if literals.(idx) <> 0 then
          bits := !bits + (literals.(idx) * ltree.(idx))
      done
      ; for idx = 0 to _d_codes - 1 do
          if distances.(idx) <> 0 then
            bits := !bits + (distances.(idx) + dtree.(idx))
        done
      ; !bits

  let block_of_frequencies ~last ~literals ~distances =
    let dynamic = dynamic_of_frequencies ~literals ~distances in
    if
      calculate_dynamic_block dynamic ~literals ~distances
      <= calculate_static_block ~literals ~distances
    then {kind= Dynamic dynamic; last}
    else {kind= Fixed; last}

  let exists v block =
    match v, block.kind with
    | (`Copy _ | `End), Flat ->
      invalid_arg "copy code in flat block can not exist"
    | `Literal chr, Dynamic dynamic ->
      dynamic.ltree.T.tree.Lookup.t.(Char.code chr) lsr _max_bits > 0
    | `Copy (off, len), Dynamic dynamic ->
      (* assert (len >= 3 && len <= 255 + 3) ; *)
      (* assert (off >= 1 && off <= 32767 + 1) ; *)
      dynamic.ltree.T.tree.Lookup.t.(256 + 1 + _length.(len)) lsr _max_bits > 0
      && dynamic.dtree.T.tree.Lookup.t.(_distance (pred off)) lsr _max_bits > 0
    | `End, (Fixed | Dynamic _) | `Literal _, (Flat | Fixed) | `Copy _, Fixed ->
      true

  type encoder = {
      dst: dst
    ; mutable blk: block
    ; mutable hold: int
    ; mutable bits: int
    ; mutable bits_rem: [ `Rem of int | `Pending ]
    ; mutable flat: int
    ; mutable fmax: int
    ; mutable o: bigstring
    ; mutable o_pos: int
    ; mutable o_max: int
    ; b: Queue.t
    ; mutable k: encoder -> encode -> [ `Ok | `Partial | `Block ]
  }

  (* remaining bytes to write in [e.o]. *)
  let o_rem e = e.o_max - e.o_pos + 1 [@@inline]

  (* set [e.o] with [s]. *)
  let dst e s j l =
    if j < 0 || l < 0 || j + l > bigstring_length s then invalid_bounds j l
    ; e.o <- s
    ; e.o_pos <- j
    ; e.o_max <- j + l - 1

  let partial k e = function
    | `Await -> k e
    (* if [encode] returns [`Partial], end-user must call [encode] with
       [`Await] value. Otherwise, it's a bad logic. *)
    | `Literal _ | `Copy _ | `Block _ | `Flush | `End -> invalid_encode ()

  let flush k e =
    match e.dst with
    | `Manual ->
      e.k <- partial k
      ; `Partial
    | `Channel oc ->
      output_bigstring oc e.o 0 e.o_pos
      ; e.o_pos <- 0
      ; k e
    | `Buffer b ->
      for i = 0 to e.o_pos - 1 do
        Buffer.add_char b (Char.unsafe_chr (unsafe_get_uint8 e.o i))
      done
      ; (* TODO: check why we need [unsafe_chr]. *)
        e.o_pos <- 0
      ; k e

  let rec c_byte byte k e =
    let rem = o_rem e in
    if rem < 1 then flush (fun e -> c_byte byte k e) e
    else (
      unsafe_set_uint8 e.o e.o_pos byte
      ; e.o_pos <- e.o_pos + 1
      ; k e)

  let rec c_short short k e =
    let rem = o_rem e in
    if rem < 2 then flush (fun e -> c_short short k e) e
    else (
      unsafe_set_uint16_le e.o e.o_pos short
      ; e.o_pos <- e.o_pos + 2
      ; k e)

  let c_bits bits long k e =
    if e.bits + long < 16 then (
      e.hold <- (bits lsl e.bits) lor e.hold
      ; e.bits <- e.bits + long
      ; k e)
    else
      let k e =
        e.hold <- e.hold lsr 16
        ; e.bits <- e.bits - 16
        ; k e in
      e.hold <- (bits lsl e.bits) lor e.hold
      ; e.bits <- e.bits + long
      ; c_short (e.hold land 0xffff) k e

  (* encode flat *)

  let rec ensure n k e =
    let rem = o_rem e in
    if rem >= n then k e else flush (ensure n k) e

  let flush_bits k e =
    assert (e.bits <= 16)

    ; if e.bits > 8 then
        let k e =
          e.hold <- 0
          ; e.bits <- 0
          ; k e in
        c_short (e.hold land 0xffff) k e
      else if e.bits > 0 then
        let k e =
          e.hold <- 0
          ; e.bits <- 0
          ; k e in
        c_byte (e.hold land 0xff) k e
      else k e

  let encode_flat_header last k e =
    let k3 e =
      assert (o_rem e >= 4)

      ; unsafe_set_uint16_le e.o (e.o_pos + 0) e.fmax
      ; unsafe_set_uint16_le e.o (e.o_pos + 2) (lnot e.fmax)
      ; e.o_pos <- e.o_pos + 4
      ; e.flat <- 0

      ; (* XXX(dinosaure): clean! *)
        k e in
    let k2 e = flush_bits (ensure 4 k3) e in
    let k1 e = c_bits 0x0 2 k2 e in
    let k0 e = c_bits (if last then 1 else 0) 1 k1 e in

    k0 e

  (* encode dynamic huffman tree *)

  let encode_huffman dynamic k e =
    let flush e =
      if e.bits >= 16 then
        let k e =
          e.hold <- e.hold lsr 16
          ; e.bits <- e.bits - 16
          ; k e in
        c_short (e.hold land 0xffff) k e
      else if e.bits >= 8 then
        let k e =
          e.hold <- e.hold lsr 8
          ; e.bits <- e.bits - 8
          ; k e in
        c_byte (e.hold land 0xff) k e
      else k e in
    let rec go rank e =
      if rank == Array.length dynamic.symbols then flush e
      else
        let len, code =
          ( dynamic.symbols.(rank) lsr _max_bits
          , dynamic.symbols.(rank) land ((1 lsl _max_bits) - 1) ) in
        (* max_len: 7 *)
        c_bits code len (go (succ rank)) e in
    go 0 e

  let encode_zigzag dynamic k e =
    let rec go rank e =
      if rank == dynamic.h_len then encode_huffman dynamic k e
      else
        let k e = go (succ rank) e in
        c_bits dynamic.bltree.T.lengths.(zigzag.(rank)) 3 k e in
    go 0 e

  let encode_dynamic_header last dynamic k e =
    (* More readable but should be optimized. *)
    let k5 e = encode_zigzag dynamic k e in
    let k4 e = c_bits (dynamic.h_len - 4) 4 k5 e in
    let k3 e = c_bits (dynamic.h_dst - 1) 5 k4 e in
    let k2 e = c_bits (dynamic.h_lit - 257) 5 k3 e in
    let k1 e = c_bits 0x2 2 k2 e in
    let k0 e = c_bits (if last then 1 else 0) 1 k1 e in

    k0 e

  let encode_fixed_header last k e =
    let k1 e = c_bits 0x1 2 k e in
    let k0 e = c_bits (if last then 1 else 0) 1 k1 e in

    k0 e

  let pending_bits k e =
    assert (e.bits <= 16)
    ; let k = flush k in

      if e.bits > 8 then
        let k e =
          e.hold <- 0
          ; e.bits_rem <- `Rem (16 - e.bits)
          ; e.bits <- 0
          ; k e in
        c_short (e.hold land 0xffff) k e
      else if e.bits > 0 then
        let k e =
          e.hold <- 0
          ; e.bits_rem <- `Rem (8 - e.bits)
          ; e.bits <- 0
          ; k e in
        c_byte (e.hold land 0xff) k e
      else k e

  exception Flush_bits of {hold: int; bits: int}

  let rec block e = function
    | `Block block ->
      let k e =
        match block.kind with
        | Dynamic dynamic ->
          encode_dynamic_header block.last dynamic
            (fun e ->
              e.k <- encode
              ; write e)
            e
        | Fixed ->
          encode_fixed_header block.last
            (fun e ->
              e.k <- encode
              ; write e)
            e
        | Flat ->
          if Queue.end_with_eob e.b then Queue.rem_exn e.b 1
          ; let len = min (Queue.length e.b) 0xffff in
            e.fmax <- len
            ; encode_flat_header block.last
                (fun e ->
                  e.k <- encode
                  ; write_flat e)
                e in
      e.blk <- block
      ; k e
    | (`Flush | `Await) as v -> encode e v

  (* TODO: not really clear. *)
  and flush_bits ~bits ~hold k e =
    if e.bits >= 16 && o_rem e > 1 then (
      unsafe_set_uint16_le e.o e.o_pos (e.hold land 0xffff)
      ; e.hold <- e.hold lsr 16
      ; e.bits <- e.bits - 16
      ; e.o_pos <- e.o_pos + 2)
    ; if e.bits >= 8 && o_rem e > 0 then (
        unsafe_set_uint8 e.o e.o_pos (e.hold land 0xff)
        ; e.hold <- e.hold lsr 8
        ; e.bits <- e.bits - 8
        ; e.o_pos <- e.o_pos + 1)

    ; if bits + e.bits > 31 then flush (flush_bits ~bits ~hold k) e
      else if bits > 0 then
        (e.hold <- ((hold land 0xffff) lsl e.bits) lor e.hold
         ; let len = min bits 16 in
           e.bits <- e.bits + len
           ; flush_bits ~bits:(bits - len) ~hold:(hold lsr len) k)
          e
      else k e

  and write e =
    let o_pos = ref e.o_pos in
    let hold = ref e.hold in
    let bits = ref e.bits in

    let exception Leave in
    let exception End in
    let k_ok e =
      e.k <- encode
      ; `Ok in
    let k_nw e =
      e.k <- block
      ; `Block in
    let k_continue e = write e in

    (* XXX(dinosaure): [k_continue] is used by [flush_bits]. When [flush_bits]
       is done, it's only to prevent an integer overflow of [hold] and it does
       not mean that we finish to encode the queue. [flush_bits] still continue
       to recall [write] then and we ensure that we have enough space to flush
       [hold] by [flush] and when the assumption of the given output has, at
       least, 2 free bytes.

       A bug appears when we compress with GZip layer [paper2] and when we reach
       [flush_bits] but we don't have enough spaces. User give to us a new
       output but:

       1) in the old implementation of [flush_bits], we wrote nothing (at least
       we want to write 16 bits)
       2) [flush_bits] finish with [`Ok] which tells to the user that we encoded
       all the queue *)
    let k_flush_bits ~bits ~hold e =
      flush (flush_bits ~bits ~hold k_continue) e in

    let rec emit e =
      if !bits >= 16 then (
        unsafe_set_uint16_le e.o !o_pos !hold
        ; hold := !hold lsr 16
        ; bits := !bits - 16
        ; o_pos := !o_pos + 2
        ; if e.o_max - !o_pos + 1 > 1 then emit e) in

    (* [emit] is recursive to consume until [!bits] >= 16. Otherwise, process
       can overflow [!hold]. *)
    let ltree, dtree =
      match e.blk with
      | {kind= Dynamic dynamic; _} -> dynamic.ltree.T.tree, dynamic.dtree.T.tree
      | {kind= Fixed; _} -> _static_ltree, _static_dtree
      | _ -> assert false in

    try
      while e.o_max - !o_pos + 1 > 1 && not (Queue.is_empty e.b) do
        let cmd = Queue.peek_exn e.b in

        if not (exists (Queue.code cmd) e.blk) then raise_notrace Leave

        ; Queue.unsafe_junk e.b

        ; if cmd == 256 then raise_notrace End

        ; match cmd land 0x2000000 == 0 with
          | true ->
            let len, v = Lookup.get ltree cmd in

            hold := (v lsl !bits) lor !hold
            ; bits := !bits + len
            ; emit e
          | false ->
            (* XXX(dinosaure): explanation is needed here.

               At the beginning, encode was made on a 64-bit processor. [int] is
               63-bit in this architecture. By this way, in ANY context, any
               _op-code_ can fit into [hold] (bigger _op-code_ is 48 bits).

               However, in 32-bit processor, this assertion is false. We reach
               sometimes the limit (31 bits) and must emit [short] to avoid
               overflow. However, in some cases, we can not:
               - store more bits into [hold]
               - emit [short] into output

               In this REAL bad case, we raise [Flush_bits] with delayed bits.
               Then, we ask the client to flush output and store current [hold]
               and delayed bits into new output. An final assertion is to have an
               output bigger than 2 bytes in any case which is fair enough, I
               think ...

               [flush_bits] can be reached with [news] Calgary file. *)
            let off, len = cmd land 0xffff, (cmd lsr 16) land 0x1ff in

            let code = _length.(len + 3) in
            let len0, v0 = Lookup.get ltree (code + 256 + 1) in
            let len1, v1 =
              _extra_lbits.(code), len - _base_length.(code land 0x1f) in

            let code = _distance off in
            let len2, v2 = Lookup.get dtree code in
            let len3, v3 =
              _extra_dbits.(code land 0x1f), off - _base_dist.(code) in

            (* len0_max: 15, 15 + 15 = 30. *)
            hold := (v0 lsl !bits) lor !hold
            ; bits := !bits + len0
            ; emit e

            ; (* len1_max: 5, 15 + 5 = 20 *)
              hold := (v1 lsl !bits) lor !hold
            ; bits := !bits + len1
            ; if e.o_max - !o_pos + 1 > 1 then emit e
              else
                raise_notrace
                  (Flush_bits {bits= len2 + len3; hold= (v3 lsl len2) lor v2})

            ; (* len2_max: 15, 15 + 15 = 30 *)
              hold := (v2 lsl !bits) lor !hold
            ; bits := !bits + len2
            ; if e.o_max - !o_pos + 1 > 1 then emit e
              else if !bits + len3 + 15 > 31 then
                raise_notrace (Flush_bits {bits= len3; hold= v3})

            ; (* len3_max: 13, 15 + 13 = 28 *)
              hold := (v3 lsl !bits) lor !hold
            ; bits := !bits + len3

            ; if e.o_max - !o_pos + 1 > 1 then emit e
              else if !bits + 15 > 31 then
                raise_notrace (Flush_bits {bits= 0; hold= 0})
      done

      ; e.hold <- !hold
      ; e.bits <- !bits
      ; e.o_pos <- !o_pos

      ; (* XXX(dinosaure): at least we need 2 bytes in any case. *)
        if o_rem e > 1 then k_ok e else flush write e
    with
    | Flush_bits {bits= bits'; hold= hold'} ->
      e.hold <- !hold
      ; e.bits <- !bits
      ; e.o_pos <- !o_pos

      ; k_flush_bits ~bits:bits' ~hold:hold' e
    | Leave -> (
      match e.blk with
      | {kind= Dynamic dynamic; _} ->
        let len, v = Lookup.get dynamic.ltree.T.tree 256 in
        hold := (v lsl !bits) lor !hold
        ; bits := !bits + len
        ; emit e

        ; e.hold <- !hold
        ; e.bits <- !bits
        ; e.o_pos <- !o_pos

        ; k_nw e
      | {kind= Fixed; _} ->
        let len, v = Lookup.get _static_ltree 256 in
        hold := (v lsl !bits) lor !hold
        ; bits := !bits + len
        ; emit e

        ; e.hold <- !hold
        ; e.bits <- !bits
        ; e.o_pos <- !o_pos

        ; k_nw e
      | _ -> assert false)
    | End -> (
      match e.blk with
      | {kind= Dynamic dynamic; _} ->
        let len, v = Lookup.get dynamic.ltree.T.tree 256 in
        hold := (v lsl !bits) lor !hold
        ; bits := !bits + len
        ; emit e

        ; e.hold <- !hold
        ; e.bits <- !bits
        ; e.o_pos <- !o_pos

        ; if e.blk.last then pending_bits k_ok e else k_nw e
      | {kind= Fixed; _} ->
        let len, v = Lookup.get _static_ltree 256 in
        hold := (v lsl !bits) lor !hold
        ; bits := !bits + len
        ; emit e

        ; e.hold <- !hold
        ; e.bits <- !bits
        ; e.o_pos <- !o_pos

        ; if e.blk.last then pending_bits k_ok e else k_nw e
      | _ -> assert false)

  and force blk e =
    let emit e =
      if e.bits >= 16 then (
        unsafe_set_uint16_le e.o e.o_pos e.hold
        ; e.hold <- e.hold lsr 16
        ; e.bits <- e.bits - 16
        ; e.o_pos <- e.o_pos + 2) in

    match e.blk with
    | {kind= Dynamic dynamic; _} ->
      let len, v = Lookup.get dynamic.ltree.T.tree 256 in
      e.hold <- (v lsl e.bits) lor e.hold
      ; e.bits <- e.bits + len
      ; emit e

      ; block e (`Block blk)
    | {kind= Fixed; _} ->
      let len, v = Lookup.get _static_ltree 256 in
      e.hold <- (v lsl e.bits) lor e.hold
      ; e.bits <- e.bits + len
      ; emit e

      ; block e (`Block blk)
    | {kind= Flat; _} ->
      emit e
      ; block e (`Block blk)

  (* XXX(dinosaure): should never occur! *)
  and write_flat e =
    let o_pos = ref e.o_pos in
    let flat = ref e.flat in

    while
      e.o_max - !o_pos + 1 > 0 && (not (Queue.is_empty e.b)) && !flat < e.fmax
    do
      let cmd = Queue.pop_exn e.b in

      if not (cmd land 0x2000000 == 0) then
        invalid_arg "Impossible to emit a copy code in a Flat block (%08x)" cmd
      ; if not (cmd == 256) then (
          unsafe_set_uint8 e.o !o_pos (cmd land 0xff)
          ; incr o_pos
          ; incr flat)
    done

    ; e.flat <- !flat
    ; e.o_pos <- !o_pos

    ; if !flat == e.fmax then begin
        e.fmax <- 0
        ; (* XXX(dinosaure): clean it! *)
          if e.blk.last then flush (fun _ -> `Ok) e
          else (
            e.k <-
              (fun e v ->
                match v with
                | `Block _ -> block e v
                | _ ->
                  e.k <- block
                  ; `Block)
            ; `Ok)
      end
      else if o_rem e == 0 then flush write_flat e
      else `Ok

  (* assert (Queue.is_empty e.b ) *)
  and encode e = function
    | `Await ->
      e.k <- encode
      ; `Ok (* XXX(dinosaure): do nothing. *)
    | `Flush -> (
      match e.blk.kind with
      | Flat -> write_flat e
      | Dynamic _ | Fixed -> write e)
    | `Block blk ->
      if e.blk.last then
        invalid_arg
          "Impossible to make a new block when the current block is the last \
           one"

      ; if o_rem e > 1 then force blk e else flush (fun e -> force blk e) e

  let first_entry e v =
    match v with
    | `Block blk ->
      e.k <- encode
      ; block e (`Block blk)
    | (`Flush | `Await) as v -> (
      match e.blk.kind with
      | Dynamic dynamic ->
        encode_dynamic_header e.blk.last dynamic
          (fun e ->
            e.k <- encode
            ; encode e v)
          e
      | Fixed ->
        encode_fixed_header e.blk.last
          (fun e ->
            e.k <- encode
            ; encode e v)
          e
      | Flat ->
        if Queue.end_with_eob e.b then Queue.rem_exn e.b 1
        ; let len = min (Queue.length e.b) 0xffff in
          e.fmax <- len
          ; encode_flat_header e.blk.last
              (fun e ->
                e.k <- encode
                ; encode e v)
              e)

  let dst_rem d = o_rem d

  let bits_rem t =
    match t.bits_rem with
    | `Rem rem -> rem
    | `Pending -> invalid_arg "Encoder does not reach EOB of last block"

  let encoder dst ~q =
    let o, o_pos, o_max =
      match dst with
      | `Manual -> bigstring_empty, 1, 0
      | `Buffer _ | `Channel _ ->
        bigstring_create io_buffer_size, 0, io_buffer_size - 1 in
    {
      dst
    ; blk= {kind= Fixed; last= false}
    ; hold= 0
    ; bits= 0
    ; bits_rem= `Pending
    ; flat= 0
    ; fmax= 0
    ; o
    ; o_pos
    ; o_max
    ; b= q
    ; k= first_entry
    }

  let encode e = e.k e

  module Ns = struct
    let _min_block_length = 10000
    let _end_padding = 8
    let _max_match_offset = 32768
    let _max_max_codeword_len = 15
    let _num_litlen_syms = 288
    let _max_litlen_codeword_len = 14
    let _num_offset_syms = 32
    let _max_offset_codeword_len = 15
    let _max_num_syms = 288
    let _num_symbol_bits = 10
    let _symbol_mask = 0b1111111111
    let _min_match_len = 3
    let _max_match_len = 258
    let _soft_max_block_length = 300000
    let _num_precode_syms = 19
    let _end_of_block = 256
    let _max_pre_codeword_len = 7
    let _max_extra_length_bits = 5
    let _max_extra_offset_bits = 14

    type error = [ `Invalid_compression_level | `Unexpected_end_of_output ]

    let pp_error ppf e =
      let s =
        match e with
        | `Invalid_compression_level -> "Invalid compression level"
        | `Unexpected_end_of_output -> "Unexpected end of output" in
      Format.fprintf ppf "%s" s

    exception Malformed of error

    let err_invalid_compression_level () =
      raise (Malformed `Invalid_compression_level)

    let err_unexpected_end_of_output () =
      raise (Malformed `Unexpected_end_of_output)

    type lit_off = {litlen: int array; offset: int array}
    type codes = {codewords: lit_off; lens: lit_off}

    type encoder = {
        level: int
      ; min_size_to_compress: int
      ; max_search_depth: int
      ; nice_match_length: int
      ; offset_slot_fast: int array
      ; freqs: lit_off
      ; codes: codes
      ; static_codes: codes
      ; precode_freqs: int array
      ; precode_lens: int array
      ; precode_codewords: int array
      ; precode_items: int array
      ; mutable num_litlen_syms: int
      ; mutable num_offset_syms: int
      ; mutable num_explicit_lens: int
      ; mutable num_precode_items: int
    }

    type output_bitstream = {
        i: bigstring
      ; mutable i_pos: int
      ; i_len: int
      ; o: bigstring
      ; mutable o_pos: int
      ; o_len: int
      ; mutable hold: int
      ; mutable bits: int
    }

    type hc_matchfinder = {
        hash4_tab: int array
      ; mutable next_hash4: int
      ; next_tab: int array
    }

    let hc_matchfinder_hash4_order = 16
    let window_size = 1 lsl 15

    let hc_matchfinder_init () =
      let hash4_tab =
        Array.make (1 lsl hc_matchfinder_hash4_order) (-window_size) in
      let next_tab = Array.make window_size 0 in
      {hash4_tab; next_hash4= 0; next_tab}

    type sequence = {
        mutable litrunlen_and_length: int
      ; mutable offset: int
      ; mutable offset_symbol: int
      ; mutable length_slot: int
    }

    let num_literal_observation_types = 8
    let num_match_observation_types = 2

    let num_observation_types =
      num_literal_observation_types + num_match_observation_types

    type block_split_stats = {
        new_observations: int array
      ; observations: int array
      ; mutable num_new_observations: int
      ; mutable num_observations: int
    }

    let split_stats =
      {
        new_observations= Array.make num_observation_types 0
      ; observations= Array.make num_observation_types 0
      ; num_new_observations= 0
      ; num_observations= 0
      }

    type lens = {mutable best: int; mutable nice: int; mutable max: int}

    let blocktype_uncompressed = 0
    let blocktype_static_huffman = 1
    let blocktype_dynamic_huffman = 2

    let init_output i o =
      {
        i
      ; i_pos= 0
      ; i_len= bigstring_length i
      ; o
      ; o_pos= 0
      ; o_len= bigstring_length o - _end_padding
      ; hold= 0
      ; bits= 0
      }

    let get_num_counter num_syms = (num_syms + (3 / 4) + 3) land lnot 3

    let sort_symbols num_syms freqs lens symout =
      let counters = Array.make (get_num_counter _max_num_syms) 0 in
      let num_counters = get_num_counter num_syms in
      for sym = 0 to num_syms - 1 do
        let i = min freqs.(sym) (num_counters - 1) in
        counters.(i) <- counters.(i) + 1
      done
      ; let num_used_syms = ref 0 in
        for i = 1 to num_counters - 1 do
          let count = counters.(i) in
          counters.(i) <- !num_used_syms
          ; num_used_syms := !num_used_syms + count
        done
        ; for sym = 0 to num_syms - 1 do
            let freq = freqs.(sym) in
            if freq <> 0 then (
              let i = min freq (num_counters - 1) in
              symout.(counters.(i)) <- sym lor (freq lsl _num_symbol_bits)
              ; counters.(i) <- counters.(i) + 1)
            else lens.(sym) <- 0
          done
        ; let counters_pos = counters.(num_counters - 2) in
          let counters_len =
            counters.(num_counters - 1) - counters.(num_counters - 2) in
          let to_sort = Array.sub symout counters_pos counters_len in
          Array.sort
            (fun i j -> match i, j with 0, _ -> 1 | _, 0 -> -1 | _ -> i - j)
            to_sort
          ; Array.blit to_sort 0 symout counters_pos counters_len
          ; !num_used_syms

    let build_tree a sym_count =
      let i = ref 0 in
      let b = ref 0 in
      let e = ref 0 in
      while sym_count - !e > 1 do
        let m, n = ref 0, ref 0 in
        if
          !i <> sym_count
          && (b = e
             || a.(!i) lsr _num_symbol_bits <= a.(!b) lsr _num_symbol_bits)
        then (
          m := !i
          ; incr i)
        else (
          m := !b
          ; incr b)
        ; if
            !i <> sym_count
            && (b = e
               || a.(!i) lsr _num_symbol_bits <= a.(!b) lsr _num_symbol_bits)
          then (
            n := !i
            ; incr i)
          else (
            n := !b
            ; incr b)
        ; let freq_shifted =
            (a.(!m) land lnot _symbol_mask) + (a.(!n) land lnot _symbol_mask)
          in
          a.(!m) <- a.(!m) land _symbol_mask lor (!e lsl _num_symbol_bits)
          ; a.(!n) <- a.(!n) land _symbol_mask lor (!e lsl _num_symbol_bits)
          ; a.(!e) <- a.(!e) land _symbol_mask lor freq_shifted
          ; incr e
      done

    let compute_length_counts a root_idx len_counts max_codeword =
      len_counts.(1) <- 2
      ; a.(root_idx) <- a.(root_idx) land _symbol_mask

      ; let rec f = function
          | -1 -> ()
          | node ->
            let parent = a.(node) lsr _num_symbol_bits in
            let parent_depth = a.(parent) lsr _num_symbol_bits in
            let depth = parent_depth + 1 in
            let len = ref depth in
            a.(node) <-
              a.(node) land _symbol_mask lor (depth lsl _num_symbol_bits)
            ; if !len >= max_codeword then (
                len := max_codeword - 1
                ; while len_counts.(!len) == 0 do
                    decr len
                  done)
            ; len_counts.(!len) <- len_counts.(!len) - 1
            ; len_counts.(!len + 1) <- len_counts.(!len + 1) + 2
            ; f (node - 1) in
        f (root_idx - 1)

    let gen_codewords a lens len_counts max_codeword_len num_syms =
      let next_codewords = Array.make (_max_max_codeword_len + 1) 0 in
      let i = ref 0 in
      let len = ref max_codeword_len in
      while !len <> 0 do
        let count = ref len_counts.(!len) in
        while !count <> 0 do
          lens.(a.(!i) land _symbol_mask) <- !len
          ; incr i
          ; decr count
        done
        ; decr len
      done
      ; next_codewords.(0) <- 0
      ; next_codewords.(1) <- 0
      ; for len = 2 to max_codeword_len do
          next_codewords.(len) <-
            (next_codewords.(len - 1) + len_counts.(len - 1)) lsl 1
        done
      ; for sym = 0 to num_syms - 1 do
          let i = lens.(sym) in
          a.(sym) <- next_codewords.(i)
          ; next_codewords.(i) <- next_codewords.(i) + 1
        done

    let make_canonical_huffman_code
        num_syms max_codeword_len freqs lens codewords =
      let num_used_syms = sort_symbols num_syms freqs lens codewords in
      match num_used_syms with
      | 0 -> ()
      | 1 ->
        let sym = codewords.(0) land _symbol_mask in
        let nonzero_idx = max sym 1 in
        codewords.(0) <- 0
        ; lens.(0) <- 1
        ; codewords.(nonzero_idx) <- 1
        ; lens.(nonzero_idx) <- 1
      | _ ->
        build_tree codewords num_used_syms
        ; let len_counts = Array.make (_max_max_codeword_len + 1) 0 in
          compute_length_counts codewords (num_used_syms - 2) len_counts
            max_codeword_len
          ; gen_codewords codewords lens len_counts max_codeword_len num_syms

    let reverse_codeword codeword len =
      let codeword =
        ((codeword land 0x5555) lsl 1) lor ((codeword land 0xAAAA) lsr 1) in
      let codeword =
        ((codeword land 0x3333) lsl 2) lor ((codeword land 0xCCCC) lsr 2) in
      let codeword =
        ((codeword land 0x0F0F) lsl 4) lor ((codeword land 0xF0F0) lsr 4) in
      let codeword =
        ((codeword land 0x00FF) lsl 8) lor ((codeword land 0xFF00) lsr 8) in
      codeword lsr (16 - len)

    let make_huffman_code num_syms max_codeword_len freqs lens codewords =
      make_canonical_huffman_code num_syms max_codeword_len freqs lens codewords
      ; for sym = 0 to num_syms - 1 do
          codewords.(sym) <- reverse_codeword codewords.(sym) lens.(sym)
        done

    let make_huffman_codes freqs codes =
      make_huffman_code _num_litlen_syms _max_litlen_codeword_len freqs.litlen
        codes.lens.litlen codes.codewords.litlen
      ; make_huffman_code _num_offset_syms _max_offset_codeword_len freqs.offset
          codes.lens.offset codes.codewords.offset

    let init_static_codes freqs static_codes =
      for i = 0 to 143 do
        freqs.litlen.(i) <- 1 lsl (9 - 8)
      done
      ; for i = 144 to 255 do
          freqs.litlen.(i) <- 1 lsl (9 - 9)
        done
      ; for i = 256 to 279 do
          freqs.litlen.(i) <- 1 lsl (9 - 7)
        done
      ; for i = 280 to 287 do
          freqs.litlen.(i) <- 1 lsl (9 - 8)
        done
      ; for i = 0 to 31 do
          freqs.offset.(i) <- 1 lsl (5 - 5)
        done
      ; make_huffman_codes freqs static_codes

    let init_offset_slot_fast offset_slot_fast =
      for offset_slot = 0 to Array.length _base_dist - 3 do
        let offset = _base_dist.(offset_slot) + 1 in
        let offset_end = offset + (1 lsl _extra_dbits.(offset_slot)) in
        for i = offset to offset_end - 1 do
          offset_slot_fast.(i) <- offset_slot
        done
      done

    let add_bits os bits num_bits =
      os.hold <- os.hold lor (bits lsl os.bits)
      ; os.bits <- os.bits + num_bits
      ; if os.bits >= 16 then begin
          if os.o_pos + 1 >= os.o_len then err_unexpected_end_of_output ()
          ; unsafe_set_uint16_le os.o os.o_pos os.hold
          ; os.o_pos <- os.o_pos + 2
          ; os.bits <- os.bits - 16
          ; os.hold <- os.hold lsr 16
        end

    let flush_bits os =
      if os.bits >= 8 then begin
        if os.o_pos >= os.o_len then err_unexpected_end_of_output ()
        ; unsafe_set_uint8 os.o os.o_pos os.hold
        ; os.o_pos <- os.o_pos + 1
        ; os.bits <- os.bits - 8
        ; os.hold <- os.hold lsr 8
      end

    let write_block_header os is_final_block block_type =
      add_bits os (if is_final_block then 1 else 0) 1
      ; add_bits os block_type 2

    let align_bitstream os =
      os.bits <- os.bits + (-os.bits land 7)
      ; flush_bits os

    let put_unaligned_le16 os v =
      unsafe_set_uint8 os.o os.o_pos (v land 0xff)
      ; unsafe_set_uint8 os.o (os.o_pos + 1) ((v lsr 8) land 0xff)
      ; os.o_pos <- os.o_pos + 2

    let memcpy src ~src_off dst ~dst_off ~len =
      let len0 = len land 3 in
      let len1 = len asr 2 in
      for i = 0 to len1 - 1 do
        let i = i * 4 in
        let v = unsafe_get_uint32 src (src_off + i) in
        unsafe_set_uint32 dst (dst_off + i) v
      done
      ; for i = 0 to len0 - 1 do
          let i = (len1 * 4) + i in
          let v = unsafe_get_uint8 src (src_off + i) in
          unsafe_set_uint8 dst (dst_off + i) v
        done

    let write_uncompressed_block os len is_final_block =
      write_block_header os is_final_block blocktype_uncompressed
      ; align_bitstream os
      ; if 4 + len >= os.o_len - os.o_pos then err_unexpected_end_of_output ()
      ; put_unaligned_le16 os len
      ; put_unaligned_le16 os (lnot len)
      ; memcpy os.i ~src_off:os.i_pos os.o ~dst_off:os.o_pos ~len
      ; os.o_pos <- os.o_pos + len

    let rec write_uncompressed_blocks os block_length is_final_block =
      match os.i_len - os.i_pos with
      | 0 -> ()
      | _ ->
        let len = min block_length 65535 in
        write_uncompressed_block os len
          (is_final_block && os.i_pos + len == os.i_len)
        ; write_uncompressed_blocks os block_length is_final_block

    let flush_output os =
      while os.bits > 0 do
        if os.o_pos >= os.o_len then err_unexpected_end_of_output ()
        ; unsafe_set_uint8 os.o os.o_pos os.hold
        ; os.o_pos <- os.o_pos + 1
        ; os.bits <- os.bits - 8
        ; os.hold <- os.hold lsr 8
      done
      ; os.o_pos

    let compress_none _c i o =
      let os = init_output i o in
      write_uncompressed_blocks os os.o_len true
      ; flush_output os

    let compute_precode_items lens num_lens precode_freqs precode_items =
      Array.fill precode_freqs 0 (Array.length precode_freqs) 0
      ; let itemptr = ref 0 in
        let run_start = ref 0 in
        while !run_start <> num_lens do
          let len = lens.(!run_start) in
          let run_end = ref !run_start in
          while !run_end <> num_lens && len == lens.(!run_end) do
            incr run_end
          done
          ; if len == 0 then begin
              while !run_end - !run_start >= 11 do
                let extra_bits = min (!run_end - !run_start - 11) 0x7F in
                precode_freqs.(18) <- precode_freqs.(18) + 1
                ; precode_items.(!itemptr) <- 18 lor (extra_bits lsl 5)
                ; incr itemptr
                ; run_start := !run_start + 11 + extra_bits
              done
              ; if !run_end - !run_start >= 3 then (
                  let extra_bits = min (!run_end - !run_start - 3) 0x7 in
                  precode_freqs.(17) <- precode_freqs.(17) + 1
                  ; precode_items.(!itemptr) <- 17 lor (extra_bits lsl 5)
                  ; incr itemptr
                  ; run_start := !run_start + 3 + extra_bits)
            end
            else if !run_end - !run_start >= 4 then (
              precode_freqs.(len) <- precode_freqs.(len) + 1
              ; precode_items.(!itemptr) <- len
              ; incr itemptr
              ; incr run_start
              ; while !run_end - !run_start >= 3 do
                  let extra_bits = min (!run_end - !run_start - 3) 0x3 in
                  precode_freqs.(16) <- precode_freqs.(16) + 1
                  ; precode_items.(!itemptr) <- 16 lor (extra_bits lsl 5)
                  ; incr itemptr
                  ; run_start := !run_start + 3 + extra_bits
                done)
          ; while !run_start <> !run_end do
              precode_freqs.(len) <- precode_freqs.(len) + 1
              ; precode_items.(!itemptr) <- len
              ; incr itemptr
              ; incr run_start
            done
        done
        ; !itemptr

    let precompute_huffman_header c =
      let rec f num_litlen_syms =
        if
          num_litlen_syms = 257
          || c.codes.lens.litlen.(num_litlen_syms - 1) <> 0
        then c.num_litlen_syms <- num_litlen_syms
        else f (num_litlen_syms - 1) in
      f _num_litlen_syms
      ; let rec g num_offset_syms =
          if
            num_offset_syms = 1
            || c.codes.lens.offset.(num_offset_syms - 1) <> 0
          then c.num_offset_syms <- num_offset_syms
          else g (num_offset_syms - 1) in
        g _num_offset_syms
        ; if c.num_litlen_syms <> _num_litlen_syms then (
            let max1 =
              min (c.num_litlen_syms + c.num_offset_syms) _num_litlen_syms
              - c.num_litlen_syms in
            let max2 = c.num_offset_syms - max1 in
            for i = 0 to max1 - 1 do
              c.codes.lens.litlen.(c.num_litlen_syms + i) <-
                c.codes.lens.offset.(i)
            done
            ; for i = 0 to max2 - 1 do
                c.codes.lens.offset.(i) <- c.codes.lens.offset.(max1 + i)
              done)
        ; c.num_precode_items <-
            compute_precode_items
              (Array.append c.codes.lens.litlen c.codes.lens.offset)
              (c.num_litlen_syms + c.num_offset_syms)
              c.precode_freqs c.precode_items
        ; make_huffman_code _num_precode_syms _max_pre_codeword_len
            c.precode_freqs c.precode_lens c.precode_codewords
        ; let rec h num_explicit_lens =
            if
              _num_precode_syms < 5
              || c.precode_lens.(zigzag.(num_explicit_lens - 1)) <> 0
            then c.num_explicit_lens <- num_explicit_lens
            else h (num_explicit_lens - 1) in
          h _num_precode_syms

          ; if c.num_litlen_syms <> _num_litlen_syms then (
              let max1 =
                min (_num_litlen_syms - c.num_litlen_syms) c.num_offset_syms
              in
              let max2 = max (c.num_offset_syms - max1) 0 in
              for i = 0 to max2 - 1 do
                c.codes.lens.offset.(max1 + max2 - 1 - i) <-
                  c.codes.lens.offset.(max2 - 1 - i)
              done
              ; for i = 0 to max1 - 1 do
                  c.codes.lens.offset.(i) <-
                    c.codes.lens.litlen.(c.num_litlen_syms + i)
                done)

    let write_huffman_header c os =
      add_bits os (c.num_litlen_syms - 257) 5
      ; add_bits os (c.num_offset_syms - 1) 5
      ; add_bits os (c.num_explicit_lens - 4) 4
      ; for i = 0 to c.num_explicit_lens - 1 do
          add_bits os c.precode_lens.(zigzag.(i)) 3
        done
      ; for i = 0 to c.num_precode_items - 1 do
          let precode_item = c.precode_items.(i) in
          let precode_sym = precode_item land 0x1F in
          add_bits os
            c.precode_codewords.(precode_sym)
            c.precode_lens.(precode_sym)
          ; if precode_sym >= 16 then
              if precode_sym == 16 then add_bits os (precode_item lsr 5) 2
              else if precode_sym == 17 then add_bits os (precode_item lsr 5) 3
              else add_bits os (precode_item lsr 5) 7
        done

    let write_sequences os codes sequences in_next in_next_i =
      let f seq =
        let litrunlen = ref (seq.litrunlen_and_length land 0x7FFF) in
        let length = seq.litrunlen_and_length lsr 15 in
        if !litrunlen <> 0 then (
          while !litrunlen >= 4 do
            let lit0 = unsafe_get_uint8 in_next (!in_next_i + 0) in
            let lit1 = unsafe_get_uint8 in_next (!in_next_i + 1) in
            let lit2 = unsafe_get_uint8 in_next (!in_next_i + 2) in
            let lit3 = unsafe_get_uint8 in_next (!in_next_i + 3) in
            add_bits os codes.codewords.litlen.(lit0) codes.lens.litlen.(lit0)
            ; add_bits os codes.codewords.litlen.(lit1) codes.lens.litlen.(lit1)
            ; add_bits os codes.codewords.litlen.(lit2) codes.lens.litlen.(lit2)
            ; add_bits os codes.codewords.litlen.(lit3) codes.lens.litlen.(lit3)
            ; in_next_i := !in_next_i + 4
            ; litrunlen := !litrunlen - 4
          done
          ; if !litrunlen <> 0 then (
              decr litrunlen
              ; add_bits os
                  codes.codewords.litlen.(unsafe_get_uint8 in_next !in_next_i)
                  codes.lens.litlen.(unsafe_get_uint8 in_next !in_next_i)
              ; incr in_next_i
              ; if !litrunlen <> 0 then (
                  decr litrunlen
                  ; add_bits os
                      codes.codewords.litlen.(unsafe_get_uint8 in_next
                                                !in_next_i)
                      codes.lens.litlen.(unsafe_get_uint8 in_next !in_next_i)
                  ; incr in_next_i
                  ; if !litrunlen <> 0 then (
                      decr litrunlen
                      ; add_bits os
                          codes.codewords.litlen.(unsafe_get_uint8 in_next
                                                    !in_next_i)
                          codes.lens.litlen.(unsafe_get_uint8 in_next !in_next_i)
                      ; incr in_next_i))))
        ; if length <> 0 then (
            in_next_i := !in_next_i + length
            ; let length_slot = seq.length_slot in
              let litlen_symbol = 257 + length_slot in
              add_bits os
                codes.codewords.litlen.(litlen_symbol)
                codes.lens.litlen.(litlen_symbol)
              ; add_bits os
                  (length - _base_length.(length_slot) - 3)
                  _extra_lbits.(length_slot)
              ; let offset_symbol = seq.offset_symbol in
                add_bits os
                  codes.codewords.offset.(offset_symbol)
                  codes.lens.offset.(offset_symbol)
                ; add_bits os
                    (seq.offset - _base_dist.(offset_symbol) - 1)
                    _extra_dbits.(offset_symbol)) in
      List.iter f sequences

    let write_end_of_block os codes =
      add_bits os
        codes.codewords.litlen.(_end_of_block)
        codes.lens.litlen.(_end_of_block)
      ; flush_bits os

    let flush_block c os block block_begin block_length is_final_block sequences
        =
      let extra_precode_bits =
        [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2; 3; 7|] in
      let dynamic_cost = ref 0 in
      let static_cost = ref 0 in
      let uncompressed_cost = ref 0 in
      c.freqs.litlen.(_end_of_block) <- c.freqs.litlen.(_end_of_block) + 1
      ; make_huffman_codes c.freqs c.codes

      ; precompute_huffman_header c

      ; dynamic_cost := !dynamic_cost + 5 + 5 + 4 + (3 * c.num_explicit_lens)
      ; for sym = 0 to _num_precode_syms - 1 do
          let extra = extra_precode_bits.(sym) in
          dynamic_cost :=
            !dynamic_cost
            + (c.precode_freqs.(sym) * (extra + c.precode_lens.(sym)))
        done
      ; for sym = 0 to 255 do
          dynamic_cost :=
            !dynamic_cost + (c.freqs.litlen.(sym) * c.codes.lens.litlen.(sym))
        done
      ; for sym = 0 to 143 do
          static_cost := !static_cost + (c.freqs.litlen.(sym) * 8)
        done
      ; for sym = 144 to 255 do
          static_cost := !static_cost + (c.freqs.litlen.(sym) * 9)
        done
      ; dynamic_cost := !dynamic_cost + c.codes.lens.litlen.(256)
      ; static_cost := !static_cost + 7
      ; for sym = 257 to 257 + Array.length _extra_lbits - 3 do
          let extra = _extra_lbits.(sym - 257) in
          dynamic_cost :=
            !dynamic_cost
            + (c.freqs.litlen.(sym) * (extra + c.codes.lens.litlen.(sym)))
          ; static_cost :=
              !static_cost
              + c.freqs.litlen.(sym)
                * (extra + c.static_codes.lens.litlen.(sym))
        done
      ; for sym = 0 to Array.length _extra_dbits - 3 do
          let extra = _extra_dbits.(sym) in
          dynamic_cost :=
            !dynamic_cost
            + (c.freqs.offset.(sym) * (extra + c.codes.lens.offset.(sym)))
          ; static_cost := !static_cost + (c.freqs.offset.(sym) * (extra + 5))
        done
      ; uncompressed_cost :=
          !uncompressed_cost
          + (-(os.bits + 3) land 7)
          + 32
          + (40 * (((block_length + 65535 - 1) / 65535) - 1))
          + (8 * block_length)
      ; let block_type =
          if !dynamic_cost < min !static_cost !uncompressed_cost then
            blocktype_dynamic_huffman
          else if !static_cost < !uncompressed_cost then
            blocktype_static_huffman
          else blocktype_uncompressed in
        if block_type = blocktype_uncompressed then begin
          os.i_pos <- !block_begin
          ; write_uncompressed_blocks os block_length is_final_block
        end
        else begin
          write_block_header os is_final_block block_type
          ; let codes =
              if block_type = blocktype_dynamic_huffman then (
                write_huffman_header c os ; c.codes)
              else c.static_codes in
            write_sequences os codes sequences block block_begin
            ; write_end_of_block os codes
        end

    let init_block_split_stats stats =
      Array.fill stats.new_observations 0 num_observation_types 0
      ; Array.fill stats.observations 0 num_observation_types 0
      ; stats.num_new_observations <- 0
      ; stats.num_observations <- 0

    let reset_symbol_frequencies c =
      Array.fill c.freqs.litlen 0 (Array.length c.freqs.litlen) 0
      ; Array.fill c.freqs.offset 0 (Array.length c.freqs.offset) 0

    let num_observations_per_block_check = 512

    let do_end_block_check stats block_length =
      let f () =
        if stats.num_observations > 0 then (
          let total_delta = ref 0 in
          for i = 0 to num_observation_types - 1 do
            let expected = stats.observations.(i) * stats.num_new_observations in
            let actual = stats.new_observations.(i) * stats.num_observations in
            let delta =
              if actual > expected then actual - expected else expected - actual
            in
            total_delta := !total_delta + delta
          done
          ; if
              !total_delta + (block_length / 4096 * stats.num_observations)
              >= num_observations_per_block_check
                 * 200
                 / 512
                 * stats.num_observations
            then true
            else false)
        else false in
      if f () then true
      else (
        for i = 0 to num_observation_types - 1 do
          stats.num_observations <-
            stats.num_observations + stats.new_observations.(i)
          ; stats.observations.(i) <-
              stats.observations.(i) + stats.new_observations.(i)
          ; stats.new_observations.(i) <- 0
        done
        ; stats.num_new_observations <- 0
        ; false)

    let should_end_block stats in_block_begin in_next in_end =
      if
        stats.num_new_observations < num_observations_per_block_check
        || in_next - in_block_begin < _min_block_length
        || in_end - in_next < _min_block_length
      then false
      else do_end_block_check stats (in_next - in_block_begin)

    let rec _lz_extend i start_pos match_pos len max_len =
      if
        len < max_len
        && unsafe_get_uint8 i (match_pos + len)
           = unsafe_get_uint8 i (start_pos + len)
      then _lz_extend i start_pos match_pos (len + 1) max_len
      else len

    let rec lz_extend i start_pos match_pos len max_len =
      if
        max_len - len >= 4
        && unsafe_get_uint32 i (match_pos + len)
           = unsafe_get_uint32 i (start_pos + len)
      then lz_extend i start_pos match_pos (len + 4) max_len
      else _lz_extend i start_pos match_pos len max_len

    let hc_matchfinder_slide_window mf =
      for i = 0 to Array.length mf.hash4_tab - 1 do
        mf.hash4_tab.(i) <- mf.hash4_tab.(i) - window_size
      done
      ; for i = 0 to Array.length mf.next_tab - 1 do
          mf.next_tab.(i) <- mf.next_tab.(i) - window_size
        done

    let lz_hash i pos num_bits =
      let v = unsafe_get_uint32 i pos in
      Int32.(to_int (shift_right_logical (mul v 0x1E35A7BDl) (32 - num_bits)))

    let rec _matchfinder_longest_rec
        cur_node best_matchptr os lens mf depth_remaining cutoff =
      let matchptr = (os.i_pos land lnot (window_size - 1)) + cur_node in
      if
        unsafe_get_uint8 os.i (matchptr + lens.best)
        <> unsafe_get_uint8 os.i (os.i_pos + lens.best)
      then
        let cur_node = mf.next_tab.(cur_node land (window_size - 1)) in
        let depth_remaining = depth_remaining - 1 in
        if cur_node <= cutoff || depth_remaining = 0 then best_matchptr
        else
          _matchfinder_longest_rec cur_node best_matchptr os lens mf
            depth_remaining cutoff
      else
        let len = lz_extend os.i os.i_pos matchptr 0 lens.max in
        if len >= lens.nice then begin
          lens.best <- len
          ; matchptr
        end
        else
          let best_matchptr =
            if len > lens.best then begin
              lens.best <- len
              ; matchptr
            end
            else best_matchptr in
          let cur_node = mf.next_tab.(cur_node land (window_size - 1)) in
          let depth_remaining = depth_remaining - 1 in
          if cur_node <= cutoff || depth_remaining = 0 then best_matchptr
          else
            _matchfinder_longest_rec cur_node best_matchptr os lens mf
              depth_remaining cutoff

    let hc_matchfinder_longest_match mf os lens max_search_depth =
      let best_matchptr = os.i_pos in
      let cur_pos = os.i_pos land (window_size - 1) in
      if cur_pos = 0 && os.i_pos <> 0 then hc_matchfinder_slide_window mf
      ; let cutoff = cur_pos - window_size in
        if lens.max < 5 then os.i_pos - best_matchptr
        else
          let cur_node4 = mf.hash4_tab.(mf.next_hash4) in
          mf.hash4_tab.(mf.next_hash4) <- cur_pos
          ; mf.next_tab.(cur_pos) <- cur_node4
          ; mf.next_hash4 <-
              lz_hash os.i (os.i_pos + 1) hc_matchfinder_hash4_order
          ; if cur_node4 <= cutoff || lens.best >= lens.nice then
              os.i_pos - best_matchptr
            else
              let best_matchptr =
                _matchfinder_longest_rec cur_node4 best_matchptr os lens mf
                  max_search_depth cutoff in
              os.i_pos - best_matchptr

    let rec _matchfinder_skip_rec os mf remaining =
      match remaining with
      | 0 -> ()
      | remaining ->
        let cur_pos = os.i_pos land (window_size - 1) in
        if cur_pos = 0 && os.i_pos <> 0 then hc_matchfinder_slide_window mf
        ; mf.next_tab.(cur_pos) <- mf.hash4_tab.(mf.next_hash4)
        ; mf.hash4_tab.(mf.next_hash4) <- cur_pos
        ; os.i_pos <- os.i_pos + 1
        ; mf.next_hash4 <- lz_hash os.i os.i_pos hc_matchfinder_hash4_order
        ; _matchfinder_skip_rec os mf (remaining - 1)

    let hc_matchfinder_skip_positions mf os count =
      if count + 5 > os.i_len - os.i_pos then os.i_pos <- os.i_pos + count
      else _matchfinder_skip_rec os mf count

    let choose_literal c literal litrunlen =
      c.freqs.litlen.(literal) <- succ c.freqs.litlen.(literal)
      ; incr litrunlen

    let choose_match c length offset litrunlen =
      let length_slot = _length.(length) in
      let offset_slot = c.offset_slot_fast.(offset) in
      c.freqs.litlen.(257 + length_slot) <-
        succ c.freqs.litlen.(257 + length_slot)
      ; c.freqs.offset.(offset_slot) <- succ c.freqs.offset.(offset_slot)
      ; {
          litrunlen_and_length= (length lsl 15) lor litrunlen
        ; offset
        ; length_slot
        ; offset_symbol= offset_slot
        }

    let observe_match stats length =
      let i = num_literal_observation_types + if length >= 9 then 1 else 0 in
      stats.new_observations.(i) <- succ stats.new_observations.(i)
      ; stats.num_new_observations <- succ stats.num_new_observations

    let observe_literal stats lit =
      let i = (lit lsl 5) land 0x6 lor (lit land 1) in
      stats.new_observations.(i) <- succ stats.new_observations.(i)
      ; stats.num_new_observations <- succ stats.num_new_observations

    let compress_greedy c i o =
      let os = init_output i o in
      let lens =
        {
          best= 0
        ; nice= min c.nice_match_length _max_match_len
        ; max= _max_match_len
        } in
      let hc_mf = hc_matchfinder_init () in
      while os.i_pos <> os.i_len do
        let in_block_begin = ref os.i_pos in
        let in_max_block_end =
          ref (os.i_pos + min (os.i_len - os.i_pos) _soft_max_block_length)
        in
        let litrunlen = ref 0 in
        let seqs = ref [] in
        init_block_split_stats split_stats
        ; reset_symbol_frequencies c
        ; while
            os.i_pos < !in_max_block_end
            && not
                 (should_end_block split_stats !in_block_begin os.i_pos os.i_len)
          do
            if lens.max > os.i_len - os.i_pos then (
              lens.max <- os.i_len - os.i_pos
              ; lens.nice <- min lens.nice lens.max)
            ; lens.best <- _min_match_len - 1
            ; let offset =
                hc_matchfinder_longest_match hc_mf os lens c.max_search_depth
              in
              if lens.best >= _min_match_len then (
                seqs := choose_match c lens.best offset !litrunlen :: !seqs
                ; litrunlen := 0
                ; observe_match split_stats lens.best
                ; os.i_pos <- succ os.i_pos
                ; hc_matchfinder_skip_positions hc_mf os (lens.best - 1))
              else (
                choose_literal c (unsafe_get_uint8 os.i os.i_pos) litrunlen
                ; observe_literal split_stats os.i_pos
                ; os.i_pos <- succ os.i_pos)
          done
        ; seqs :=
            List.rev
            @@ {
                 litrunlen_and_length= !litrunlen
               ; offset= 0
               ; offset_symbol= 0
               ; length_slot= 0
               }
               :: !seqs
        ; flush_block c os i in_block_begin
            (os.i_pos - !in_block_begin)
            (os.i_pos = os.i_len) !seqs
      done
      ; flush_output os

    let compress_lazy _ _ _ = 0 (* clecat: TO DO *)

    let encoder level =
      if level < 0 || level > 12 then err_invalid_compression_level ()
      ; let min_size_to_compress = 56 - (level * 4) in
        let impl, max_search_depth, nice_match_length =
          match level with
          | 0 -> compress_none, 0, 0
          | 1 -> compress_greedy, 2, 8
          | 2 -> compress_greedy, 6, 10
          | 3 -> compress_greedy, 12, 14
          | 4 -> compress_greedy, 24, 24
          | 5 -> compress_lazy, 20, 30
          | 6 -> compress_lazy, 40, 65
          | 7 -> compress_lazy, 100, 130
          | 8 -> compress_lazy, 150, 200
          | _ -> compress_lazy, 200, 258 in
        let offset_slot_fast = Array.make (_max_match_offset + 1) 0 in
        init_offset_slot_fast offset_slot_fast
        ; let litlen = Array.make _num_litlen_syms 0 in
          let offset = Array.make _num_offset_syms 0 in
          let freqs = {litlen; offset} in
          let litlen = Array.make _num_litlen_syms 0 in
          let offset = Array.make _num_offset_syms 0 in
          let lens = {litlen; offset} in
          let litlen = Array.make _num_litlen_syms 0 in
          let offset = Array.make _num_offset_syms 0 in
          let codewords = {litlen; offset} in
          let codes = {lens; codewords} in
          let litlen = Array.make _num_litlen_syms 0 in
          let offset = Array.make _num_offset_syms 0 in
          let lens = {litlen; offset} in
          let litlen = Array.make _num_litlen_syms 0 in
          let offset = Array.make _num_offset_syms 0 in
          let codewords = {litlen; offset} in
          let static_codes = {lens; codewords} in
          init_static_codes freqs static_codes
          ; let precode_freqs = Array.make _num_precode_syms 0 in
            let precode_lens = Array.make _num_precode_syms 0 in
            let precode_codewords = Array.make _num_precode_syms 0 in
            let precode_items =
              Array.make (_num_litlen_syms + _num_offset_syms) 0 in
            let num_litlen_syms = 0 in
            let num_offset_syms = 0 in
            let num_explicit_lens = 0 in
            let num_precode_items = 0 in
            ( impl
            , {
                level
              ; min_size_to_compress
              ; max_search_depth
              ; nice_match_length
              ; offset_slot_fast
              ; freqs
              ; codes
              ; static_codes
              ; precode_freqs
              ; precode_lens
              ; precode_codewords
              ; precode_items
              ; num_litlen_syms
              ; num_offset_syms
              ; num_explicit_lens
              ; num_precode_items
              } )

    let compress_bound len =
      let max_blocks =
        max 1 ((len + _min_block_length - 1) / _min_block_length) in
      (5 * max_blocks) + len + 1 + _end_padding

    let deflate ?(level = 4) src dst =
      try
        let impl, c = encoder level in
        let res =
          if bigstring_length dst < _end_padding then 0
          else if bigstring_length src < c.min_size_to_compress then (
            let os = init_output src dst in
            write_uncompressed_block os (os.i_len - os.i_pos) true
            ; flush_output os)
          else impl c src dst in
        Ok res
      with Malformed e -> Error (e : error :> [> error ])
  end
end

module Lz77 = struct
  let _max_match = 258
  let _min_match = 3
  let _min_lookahead = _max_match + _min_match + 1
  let ( .!() ) buf pos = unsafe_get_uint32 buf pos
  let ( .![] ) buf pos = unsafe_get_uint16 buf pos
  let ( .!{} ) buf pos = unsafe_get_uint8 buf pos

  type deflate_configuration = {
      max_chain: int
    ; max_lazy: int
    ; good_length: int
    ; nice_length: int
  }

  type configuration = Copy | Deflate of deflate_configuration

  let _0 = Copy
  let _1 = Deflate {good_length= 4; max_lazy= 4; nice_length= 8; max_chain= 4}
  let _2 = Deflate {good_length= 4; max_lazy= 5; nice_length= 16; max_chain= 8}
  let _3 = Deflate {good_length= 4; max_lazy= 6; nice_length= 32; max_chain= 32}
  let _4 = Deflate {good_length= 4; max_lazy= 4; nice_length= 16; max_chain= 16}

  let _5 =
    Deflate {good_length= 8; max_lazy= 16; nice_length= 32; max_chain= 32}

  let _6 =
    Deflate {good_length= 8; max_lazy= 16; nice_length= 128; max_chain= 128}

  let _7 =
    Deflate {good_length= 8; max_lazy= 32; nice_length= 128; max_chain= 256}

  let _8 =
    Deflate {good_length= 32; max_lazy= 128; nice_length= 258; max_chain= 1024}

  let _9 =
    Deflate {good_length= 32; max_lazy= 258; nice_length= 258; max_chain= 4096}

  let _mem_level = 8 (* default *)
  let _hash_bits = _mem_level + 7
  let _hash_size = 1 lsl _hash_bits
  let _too_far = 4096
  let _hash_magic = 0x9e3779b1 (* xxHash *)
  let _hash_shift = 32 - _hash_bits

  (* NOTE(dinosaure): On a 32-bit architecture, we lose 1 bit, but that’s not a
     major issue. This limitation also accounts for the difference between zlib
     and zlib-ng, where the latter takes advantage of the new capabilities of a
     modern CPU.

     As for OCaml, this remains a hash function (xxHash) which does not
     necessarily have to correspond exactly to the string we wish to hash, but
     rather to an identifier that allows us to 'quickly' retrieve possible
     matches. *)
  let hash4 buf off =
    let lo = unsafe_get_uint16 buf off in
    let hi = unsafe_get_uint16 buf (off + 2) in
    let v = lo lor (hi lsl 16) in
    (v * _hash_magic land 0xffffffff) lsr _hash_shift

  type src = [ `Channel of in_channel | `String of string | `Manual ]
  type decode = [ `Await | `Flush | `End ]

  type state = {
      src: src
    ; cfg: configuration
    ; level: int
    ; mutable i: bigstring
    ; mutable i_pos: int
    ; mutable i_len: int
    ; l: literals
    ; d: distances
    ; w: bigstring
    ; wbits: int
    ; mutable lookahead: int
    ; mutable strstart: int
    ; prev: int array
    ; head: int array
    ; mutable match_start: int
    ; mutable match_length: int
    ; mutable match_available: bool
    ; mutable insert: int
    ; mutable prev_length: int
    ; mutable prev_match: int
    ; q: Queue.t
    ; mutable crc: optint
    ; mutable k: configuration -> state -> decode
  }

  let max_dist s = (1 lsl s.wbits) - _min_lookahead

  exception Break

  (* cur is the head of the hash chain for the current string
   * and its distance is <= _max_dist
   * prev_length >= 1
   * len >= _min_lookahead *)
  let longest_match (cfg : deflate_configuration) s cur_match =
    let wmask = (1 lsl s.wbits) - 1 in
    let str_end = s.strstart + (_max_match - 1) in
    let limit = if s.strstart > max_dist s then s.strstart - max_dist s else 0 in

    (* Stop when !cur becomes <= limit. To somplify the code,
     * we prevent matches with the string of window index 0. *)
    let cur_match = ref cur_match in
    (* current match *)
    let chain_length =
      ref
        (if s.prev_length >= cfg.good_length then cfg.max_chain asr 2
         else cfg.max_chain) in
    (* max hash chain length *)
    let scan = ref s.strstart in
    (* current string *)
    let scan_start = s.w.![s.strstart] in
    let scan_end = ref s.w.![s.strstart + s.prev_length - 1] in
    let best_len = ref s.prev_length in

    (* best match length so far *)
    (try
       while
         let match' = ref !cur_match in
         if
           s.w.![!match' + !best_len - 1] <> !scan_end
           || s.w.![!match'] <> scan_start
         then begin
           cur_match := s.prev.(!cur_match land wmask)
           ; decr chain_length
           ; !cur_match > limit && !chain_length != 0
         end
         else begin
           incr scan
           ; incr match'
           ; while !scan < str_end && s.w.!(!scan) = s.w.!(!match') do
               scan := !scan + 4
               ; match' := !match' + 4
             done
           ; while !scan < str_end && s.w.![!scan] == s.w.![!match'] do
               scan := !scan + 2
               ; match' := !match' + 2
             done
           ; while !scan < str_end && s.w.!{!scan} == s.w.!{!match'} do
               scan := !scan + 1
               ; match' := !match' + 1
             done
           ; if s.w.!{!scan} == s.w.!{!match'} then incr scan
           ; let len = _max_match - 1 - (str_end - !scan) in
             scan := str_end - (_max_match - 1)
             ; if len > !best_len then begin
                 s.match_start <- !cur_match
                 ; best_len := len
                 ; if len >= cfg.nice_length then raise Break
                 ; scan_end := s.w.![!scan + !best_len - 1]
               end
             ; cur_match := s.prev.(!cur_match land wmask)
             ; decr chain_length
             ; !cur_match > limit && !chain_length != 0
         end
       do
         ()
       done
     with Break -> ())
    ; if !best_len <= s.lookahead then !best_len else s.lookahead

  let eoi s =
    s.i <- bigstring_empty
    ; s.i_pos <- 0
    ; s.i_len <- min_int

  let src d s j l =
    if j < 0 || l < 0 || j + l > bigstring_length s then invalid_bounds j l
    ; if l == 0 then eoi d
      else (
        d.i <- s
        ; d.i_pos <- j
        ; d.i_len <- j + l - 1)

  let i_rem s = s.i_len - s.i_pos + 1 [@@inline]
  let src_rem s = i_rem s
  let io_buffer_size = 16384

  let refill k s =
    match s.src with
    | `String _ -> eoi s ; k s.cfg s
    | `Channel ic ->
      let res = input_bigstring ic s.i 0 (bigstring_length s.i) in
      src s s.i 0 res ; k s.cfg s
    | `Manual ->
      s.k <- k
      ; `Await

  let memcpy src ~src_off dst ~dst_off ~len =
    let len0 = len land 3 in
    let len1 = len asr 2 in
    for i = 0 to len1 - 1 do
      let i = i * 4 in
      let v = unsafe_get_uint32 src (src_off + i) in
      unsafe_set_uint32 dst (dst_off + i) v
    done
    ; for i = 0 to len0 - 1 do
        let i = (len1 * 4) + i in
        let v = unsafe_get_uint8 src (src_off + i) in
        unsafe_set_uint8 dst (dst_off + i) v
      done

  let update_crc s len =
    s.crc <- Checkseum.Adler32.digest_bigstring s.i s.i_pos len s.crc

  let insert_string s str =
    let wmask = (1 lsl s.wbits) - 1 in
    let hash = hash4 s.w str in
    let res = s.head.(hash) in
    s.prev.(str land wmask) <- res
    ; s.head.(hash) <- str
    ; res

  let succ_length literals length =
    literals.(256 + 1 + _length.(length)) <-
      literals.(256 + 1 + _length.(length)) + 1

  let succ_distance distances distance =
    distances.(_distance (pred distance)) <-
      distances.(_distance (pred distance)) + 1

  let emit_match s ~off ~len =
    Queue.push_exn s.q (Queue.cmd (`Copy (off, len)))
    ; succ_length (s.l :> int array) len
    ; succ_distance (s.d :> int array) off
    ; if Queue.available s.q = 1 then (
        Queue.push_exn s.q Queue.eob
        ; true)
      else false

  let succ_literal literals chr =
    literals.(Char.code chr) <- literals.(Char.code chr) + 1

  let emit_literal s chr =
    Queue.push_exn s.q (Queue.cmd (`Literal chr))
    ; succ_literal (s.l :> int array) chr
    ; if Queue.available s.q = 1 then (
        Queue.push_exn s.q Queue.eob
        ; true)
      else false

  (* XXX(dinosaure): it's possible that it remains one literal. *)
  let trailing s =
    if s.match_available then (
      let flush = emit_literal s (unsafe_get_char s.w (s.strstart - 1)) in
      s.insert <-
        (if s.strstart < _min_match - 1 then s.strstart else _min_match - 1)
      ; if not flush then Queue.push_exn s.q Queue.eob
      ; `End)
    else (
      Queue.push_exn s.q Queue.eob
      ; `End)

  let slide_hash s =
    let wsize = 1 lsl s.wbits in
    let m = ref 0 in
    let n = ref _hash_size in
    let p = ref !n in
    while
      decr p
      ; m := s.head.(!p)
      ; s.head.(!p) <- (if !m >= wsize then !m - wsize else 0)
      ; decr n
      ; !n != 0
    do
      ()
    done
    ; n := wsize
    ; p := !n
    ; while
        decr p
        ; m := s.prev.(!p)
        ; s.prev.(!p) <- (if !m >= wsize then !m - wsize else 0)
        ; decr n
        ; !n != 0
      do
        ()
      done

  let rec fill_window (cfg : configuration) s =
    let wsize = 1 lsl s.wbits in
    let wmask = wsize - 1 in
    let more = (wsize * 2) - s.lookahead - s.strstart in
    let deflate cfg s =
      match cfg, s.level with
      | Copy, _ -> copy s
      | Deflate deflate_cfg, _ -> deflate deflate_cfg s in
    (* max *)
    let more =
      if s.strstart >= wsize + max_dist s then begin
        memcpy s.w ~src_off:wsize s.w ~dst_off:0 ~len:(wsize - more)
        ; s.match_start <- s.match_start - wsize
        ; s.strstart <- s.strstart - wsize
        ; slide_hash s
        ; more + wsize
      end
      else more in
    let rem = i_rem s in
    if rem <= 0 (* if (s->strm->avail_in == 0) break; *) then
      if rem < 0 then if s.lookahead > 0 then deflate cfg s else trailing s
      else refill fill_window s
    else
      try
        let len = min more rem in
        memcpy s.i ~src_off:s.i_pos s.w ~dst_off:(s.strstart + s.lookahead) ~len
        ; (*d*) update_crc s len
        ; s.lookahead <- s.lookahead + len
        ; (*d*) s.i_pos <- s.i_pos + len
        ; if s.lookahead + s.insert >= _min_match then begin
            let str = ref (s.strstart - s.insert) in
            let insert = ref s.insert in
            while s.lookahead + !insert >= _min_match && !insert != 0 do
              let hash = hash4 s.w !str in
              s.prev.(!str land wmask) <- s.head.(hash)
              ; s.head.(hash) <- !str
              ; incr str
              ; decr insert
              ; if s.lookahead + !insert < _min_match then begin
                  s.insert <- !insert
                  ; raise Break
                end
            done
            ; s.insert <- !insert
          end
        ; if s.lookahead < _min_lookahead && i_rem s >= 0 then
            refill fill_window s
          else deflate cfg s
      with Break -> deflate cfg s

  and enough (cfg : configuration) s =
    if s.lookahead < _min_lookahead then fill_window cfg s
    else
      match cfg, s.level with
      | Copy, _ -> copy s
      | Deflate deflate_cfg, _ -> deflate deflate_cfg s

  and deflate (cfg : deflate_configuration) s =
    let hash_head = ref 0 in
    if s.lookahead >= _min_match then hash_head := insert_string s s.strstart
    ; s.prev_length <- s.match_length
    ; s.prev_match <- s.match_start
    ; s.match_length <- _min_match - 1
    ; (if
         !hash_head != 0
         && s.prev_length < cfg.max_lazy
         && s.strstart - !hash_head <= max_dist s
       then
         let match_length = longest_match cfg s !hash_head in
         if
           match_length <= 5
           && match_length == _min_match
           && s.strstart - s.match_start > _too_far
         then s.match_length <- _min_match - 1
         else s.match_length <- match_length)
    ; if s.prev_length >= _min_match && s.match_length <= s.prev_length then begin
        let max_insert = s.strstart + s.lookahead - _min_match in
        let flush =
          emit_match s ~off:(s.strstart - 1 - s.prev_match) ~len:s.prev_length
        in
        s.lookahead <- s.lookahead - (s.prev_length - 1)
        ; s.prev_length <- s.prev_length - 2
        ; while
            s.strstart <- s.strstart + 1
            ; if s.strstart <= max_insert then
                hash_head := insert_string s s.strstart
            ; s.prev_length <- s.prev_length - 1
            ; s.prev_length <> 0
          do
            ()
          done
        ; s.match_available <- false
        ; s.match_length <- _min_match - 1
        ; s.strstart <- s.strstart + 1
        ; if flush then (
            s.k <- enough
            ; `Flush)
          else enough (Deflate cfg) s
      end
      else if s.match_available then
        begin match emit_literal s (unsafe_get_char s.w (s.strstart - 1)) with
        | true ->
          s.strstart <- s.strstart + 1
          ; s.lookahead <- s.lookahead - 1
          ; s.k <- enough
          ; `Flush
        | false ->
          s.strstart <- s.strstart + 1
          ; s.lookahead <- s.lookahead - 1
          ; enough (Deflate cfg) s
        end
      else begin
        s.match_available <- true
        ; s.strstart <- s.strstart + 1
        ; s.lookahead <- s.lookahead - 1
        ; enough (Deflate cfg) s
      end

  and copy s =
    let flush = ref (Queue.available s.q <= 1) in
    while (not !flush) && s.lookahead > 0 do
      flush := emit_literal s (unsafe_get_char s.w s.strstart)
      ; s.strstart <- s.strstart + 1
      ; s.lookahead <- s.lookahead - 1
    done
    ; match !flush with
      | true ->
        s.k <- enough
        ; `Flush
      | false -> enough Copy s

  let _literals = 256
  let _length_codes = 29
  let _l_codes = _literals + 1 + _length_codes
  let _d_codes = 30
  let checksum {crc; _} = crc
  let distances {d; _} = d
  let literals {l; _} = l

  let ctz x =
    let n = ref 0 and x = ref x and y = ref 0 in
    if Sys.word_size = 64 then (
      n := 63
      ; y := !x lsl 32
      ; if !y != 0 then (
          n := !n - 32
          ; x := !y))
    else n := 31
    ; y := !x lsl 16
    ; if !y != 0 then (
        n := !n - 16
        ; x := !y)
    ; y := !x lsl 8
    ; if !y != 0 then (
        n := !n - 8
        ; x := !y)
    ; y := !x lsl 4
    ; if !y != 0 then (
        n := !n - 4
        ; x := !y)
    ; y := !x lsl 2
    ; if !y != 0 then (
        n := !n - 2
        ; x := !y)
    ; y := !x lsl 1
    ; if !y != 0 then n := !n - 1
    ; !n

  let state ?(level = 4) ~q ~w src =
    let wbits = ctz (bigstring_length w / 2) - 1 in
    let wsize = 1 lsl wbits in
    let cfg =
      match level with
      | 0 -> _0
      | 1 -> _1
      | 2 -> _2
      | 3 -> _3
      | 4 -> _4
      | 5 -> _5
      | 6 -> _6
      | 7 -> _7
      | 8 -> _8
      | 9 -> _9
      | _ -> invalid_arg "Invalid level of compression: %d" level in
    let i, i_pos, i_len =
      match src with
      | `Manual -> bigstring_empty, 1, 0
      | `String x -> bigstring_of_string x, 0, String.length x - 1
      | `Channel _ -> bigstring_create io_buffer_size, 1, 0 in
    {
      src
    ; i
    ; i_pos
    ; i_len
    ; cfg
    ; level
    ; l= make_literals ()
    ; d= make_distances ()
    ; w
    ; wbits
    ; lookahead= 0
    ; strstart= 0
    ; prev= Array.make wsize 0
    ; head= Array.make _hash_size 0
    ; match_start= 0
    ; match_length= 0
    ; match_available= false
    ; insert= 0
    ; prev_length= 0
    ; prev_match= 0
    ; q
    ; crc= Checkseum.Adler32.default
    ; k= enough
    }

  let compress s = s.k s.cfg s

  type window = bigstring

  let make_window ~bits = bigstring_create ((1 lsl bits) * 2)
  let no_compression s = s.cfg = Copy
end

module Higher = struct
  let compress ~w ~q ~refill ~flush i o =
    let state = Lz77.state `Manual ~w ~q in
    let encoder = Def.encoder `Manual ~q in

    let block ~last =
      let literals = Lz77.literals state in
      let distances = Lz77.distances state in
      Def.block_of_frequencies ~last ~literals ~distances in

    let rec compress () =
      match Lz77.compress state with
      | `Await ->
        let len = refill i in
        Lz77.src state i 0 len ; compress ()
      | `Flush -> encode (Def.encode encoder (`Block (block ~last:false)))
      | `End -> pending (Def.encode encoder (`Block (block ~last:true)))
    and encode = function
      | `Partial ->
        let len = bigstring_length o - Def.dst_rem encoder in
        flush o len
        ; Def.dst encoder o 0 (bigstring_length o)
        ; encode (Def.encode encoder `Await)
      | `Ok -> compress ()
      | `Block -> encode (Def.encode encoder (`Block (block ~last:false)))
    and pending = function
      | `Block -> assert false (* XXX(dinosaure): should never appear. *)
      | `Partial ->
        let len = bigstring_length o - Def.dst_rem encoder in
        flush o len
        ; Def.dst encoder o 0 (bigstring_length o)
        ; pending (Def.encode encoder `Await)
      | `Ok -> () in

    Queue.reset q
    ; Def.dst encoder o 0 (bigstring_length o)
    ; compress ()

  let uncompress ~w ~refill ~flush i o =
    let decoder = Inf.decoder `Manual ~o ~w in

    let rec decompress () =
      match Inf.decode decoder with
      | `Await ->
        let len = refill i in
        Inf.src decoder i 0 len ; decompress ()
      | `End ->
        let len = bigstring_length o - Inf.dst_rem decoder in
        if len > 0 then flush o len
        ; Ok ()
      | `Flush ->
        let len = bigstring_length o - Inf.dst_rem decoder in
        flush o len ; Inf.flush decoder ; decompress ()
      | `Malformed err -> Error (`Msg err) in
    decompress ()

  let of_string ~o ~w input ~flush =
    let decoder = Inf.decoder (`String input) ~o ~w in
    let rec decompress () =
      match Inf.decode decoder with
      | `Await -> assert false
      | `End ->
        let len = bigstring_length o - Inf.dst_rem decoder in
        if len > 0 then flush o len
        ; Ok ()
      | `Flush ->
        let len = bigstring_length o - Inf.dst_rem decoder in
        flush o len ; Inf.flush decoder ; decompress ()
      | `Malformed err -> Error (`Msg err) in
    decompress ()

  let to_string ?(buffer = 4096) ~w ~q ~refill i =
    let buf = Buffer.create buffer in
    let state = Lz77.state `Manual ~q ~w in
    let encoder = Def.encoder (`Buffer buf) ~q in

    let block ~last =
      let literals = Lz77.literals state in
      let distances = Lz77.distances state in
      Def.block_of_frequencies ~last ~literals ~distances in

    let rec compress () =
      match Lz77.compress state with
      | `Await ->
        let len = refill i in
        Lz77.src state i 0 len ; compress ()
      | `Flush -> encode (Def.encode encoder (`Block (block ~last:false)))
      | `End -> pending (Def.encode encoder (`Block (block ~last:true)))
    and encode = function
      | `Partial -> assert false
      | `Ok -> compress ()
      | `Block -> encode (Def.encode encoder (`Block (block ~last:false)))
    and pending = function `Partial | `Block -> assert false | `Ok -> () in

    Queue.reset q ; compress () ; Buffer.contents buf
end
