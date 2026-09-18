type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type error =
  [ `Malformed of string | `Invalid_argument of string | `Invalid_dictionary ]

let pf = Format.fprintf

let pp_error ppf = function
  | `Malformed err -> pf ppf "%s" err
  | `Invalid_argument err -> pf ppf "%s" err
  | `Invalid_dictionary -> pf ppf "Invalid dictionary"

let bigstring_length x = Bigarray.Array1.dim x [@@inline]

let bigstring_create l =
  Bigarray.Array1.create Bigarray.char Bigarray.c_layout l

let bigstring_sub buf off len = Bigarray.Array1.sub buf off len
let bigstring_empty = bigstring_create 0

(* XXX(dinosaure): we want to control which exception is raised if we want to have
   a bad access. In this case, [Out_of_bound]. *)

exception Out_of_bound

external unsafe_get_char : bigstring -> int -> char = "%caml_ba_ref_1"

let get_char buf ofs =
  if ofs < 0 || ofs > bigstring_length buf - 1 then raise Out_of_bound
  ; unsafe_get_char buf ofs

external unsafe_get_int16 : bigstring -> int -> int = "%caml_bigstring_get16u"
external unsafe_get_int8 : bigstring -> int -> int = "%caml_ba_ref_1"

let get_int8 buf ofs =
  if ofs < 0 || ofs > bigstring_length buf - 1 then raise Out_of_bound
  ; unsafe_get_int8 buf ofs

external unsafe_set_int8 : bigstring -> int -> int -> unit = "%caml_ba_set_1"

let set_int8 buf ofs x =
  if ofs < 0 || ofs > bigstring_length buf - 1 then raise Out_of_bound
  ; unsafe_set_int8 buf ofs x

external unsafe_get_int32 : bigstring -> int -> int32 = "%caml_bigstring_get32"
external unsafe_get_int64 : bigstring -> int -> int64 = "%caml_bigstring_get64"

let get_int32 buf ofs =
  if ofs < 0 || ofs > bigstring_length buf - 4 then raise Out_of_bound
  ; unsafe_get_int32 buf ofs

let get_int64 buf ofs =
  if ofs < 0 || ofs > bigstring_length buf - 8 then raise Out_of_bound
  ; unsafe_get_int64 buf ofs

external unsafe_set_int32 : bigstring -> int -> int32 -> unit
  = "%caml_bigstring_set32"

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "caml_int32_bswap"
external swap64 : int64 -> int64 = "caml_int64_bswap"

(* XXX(dinosaure): assume that LZO does need [memcpy] behaviour. *)
let unsafe_blit src src_off dst dst_off len =
  let len0 = len land 3 in
  let len1 = len asr 2 in

  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = unsafe_get_int32 src (src_off + i) in
    unsafe_set_int32 dst (dst_off + i) v
  done

  ; for i = 0 to len0 - 1 do
      let i = (len1 * 4) + i in
      let v = unsafe_get_int8 src (src_off + i) in
      unsafe_set_int8 dst (dst_off + i) v
    done

let blit src src_off dst dst_off len =
  if
    len < 0
    || src_off < 0
    || src_off > bigstring_length src - len
    || dst_off < 0
    || dst_off > bigstring_length dst - len
  then raise Out_of_bound
  ; unsafe_blit src src_off dst dst_off len

external bytes_unsafe_set_int32 : bytes -> int -> int32 -> unit
  = "%caml_bytes_set32"

let bytes_unsafe_set_int8 buf off chr = Bytes.set buf off (Char.unsafe_chr chr)

let unsafe_bigstring_to_string buf ofs len =
  let res = Bytes.create len in
  let len0 = len land 3 in
  let len1 = len asr 2 in

  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = unsafe_get_int32 buf (ofs + i) in
    bytes_unsafe_set_int32 res i v
  done

  ; for i = 0 to len0 - 1 do
      let i = (len1 * 4) + i in
      let v = unsafe_get_int8 buf (ofs + i) in
      bytes_unsafe_set_int8 res i v
    done

  ; Bytes.unsafe_to_string res

let bigstring_to_string buf ofs len =
  if ofs < 0 || len < 0 || ofs > bigstring_length buf - len then
    raise Out_of_bound
  ; unsafe_bigstring_to_string buf ofs len

let unsafe_get_int16_le =
  if Sys.big_endian then fun buf off -> swap16 (unsafe_get_int16 buf off)
  else fun buf off -> unsafe_get_int16 buf off

let unsafe_get_int16_be =
  if Sys.big_endian then fun buf off -> unsafe_get_int16 buf off
  else fun buf off -> swap16 (unsafe_get_int16 buf off)

let unsafe_get_int16 buf ofs = function
  | `LE -> unsafe_get_int16_le buf ofs
  | `BE -> unsafe_get_int16_be buf ofs

let get_int16 buf ofs endian =
  if ofs < 0 || ofs > bigstring_length buf - 2 then raise Out_of_bound
  ; unsafe_get_int16 buf ofs endian

let get_int32_le =
  if Sys.big_endian then fun buf off -> swap32 (get_int32 buf off)
  else fun buf off -> get_int32 buf off

let get_int64_le =
  if Sys.big_endian then fun buf off -> swap64 (get_int64 buf off)
  else fun buf off -> get_int64 buf off

let kstrf k fmt = Format.kasprintf k fmt

type sub = {off: int; len: int}

module State : sig
  type t = private int

  val of_int : int -> t
  val _0 : t
  val _3 : t
  val _no_extra : t
end = struct
  type t = int

  let of_int x = x
  let _0 = 0
  let _3 = 3
  let _no_extra = -1
end

type ('a, 'error) t =
  | Return : 'a -> ('a, 'error) t
  | Bind : ('a, 'error) t * ('a -> ('b, 'error) t) -> ('b, 'error) t
  | State : (state, 'error) t
  | Transmit : int * state -> (unit, 'error) t
  | Count : (int, 'error) t
  | Copy : sub * state -> (unit, 'error) t
  | Fail : 'error -> ('a, 'error) t
  | Peek : 'a value -> ('a, 'error) t
  | Junk : 'a value -> (unit, 'error) t
  | Fix : (('a, 'error) t -> ('a, 'error) t) -> ('a, 'error) t
  | Lazy : ('a, 'error) t Lazy.t -> ('a, 'error) t

and 'a value = Byte : char value | Short : [ `LE | `BE ] -> int value
and state = State.t

type v = {
    i: bigstring
  ; mutable i_pos: int
  ; mutable o: bigstring
  ; mutable o_pos: int
  ; mutable state: State.t
}

let transmit v len =
  blit v.i v.i_pos v.o v.o_pos len
  ; v.i_pos <- v.i_pos + len
  ; v.o_pos <- v.o_pos + len
  ; Ok ()

let copy v ~off ~len =
  blit v.o (v.o_pos - off) v.o v.o_pos len
  ; v.o_pos <- v.o_pos + len
  ; Ok ()

let transmit_to_buffer buf v len =
  let res = bigstring_to_string v.i v.i_pos len in
  Buffer.add_string buf res
  ; v.i_pos <- v.i_pos + len
  ; v.o_pos <- v.o_pos + len
  ; Ok ()

let copy_to_buffer buf _v ~off ~len =
  let rec go off len =
    if len = 0 then Ok ()
    else
      let pos = Buffer.length buf - off in
      let rem = Buffer.length buf - pos in
      let cpy = min rem len in
      let res = Buffer.sub buf pos cpy in
      Buffer.add_string buf res
      ; go (off + cpy) (len - cpy) in
  if Buffer.length buf >= off then go off len else Error `Invalid_dictionary

let count t =
  let res = ref 0 in
  let idx = ref t.i_pos in
  let max = bigstring_length t.i in

  while (not (!idx > max - 4)) && get_int32_le t.i !idx = 0l do
    idx := !idx + 4
    ; res := !res + 4
  done
  ; while !idx + 1 <= max && get_int8 t.i !idx = 0 do
      incr idx ; incr res
    done

  ; if !idx < max then (
      t.i_pos <- !idx + 1
      ; Ok ((!res * 255) + get_int8 t.i !idx))
    else Error (`Malformed "Invalid input")

type ('a, 'b) k = Ok of 'a | Error of 'b

let ( >>= ) :
    ('a, 'err) result -> ('a -> ('b, 'err) result) -> ('b, 'err) result =
 fun x f -> match x with Ok x -> f x | Error err -> Error err

let copy_done ~transmit t =
  let state = (t.state :> int) in
  transmit t (state land 3)

let run :
       transmit:(v -> int -> (unit, 'error) result)
    -> copy:(v -> off:int -> len:int -> (unit, 'error) result)
    -> ('a, 'error) t
    -> v
    -> ('a, 'error) result =
 fun ~transmit ~copy fiber t ->
  let rec go : type a.
      v -> (a, ([> `Malformed of string ] as 'error)) t -> (a, 'error) k =
   fun t instr ->
    match instr with
    | Fail err -> Error err
    | Return v -> Ok v
    | Bind (x, f) -> (
      match go t x with Ok v -> go t (f v) | Error _ as err -> err)
    | Fix fix ->
      (* XXX(dinosaure): [Kontinuation] exists to break the stack-overflow with [js_of_ocaml] but
         it was not implemented yet. *)
      let rec m = lazy (fix r) and r = Lazy m in
      go t r
    | Lazy m -> go t (Lazy.force m)
    | State -> Ok t.state
    | _ when t.i_pos >= bigstring_length t.i ->
      Error (`Malformed "Unexpected end of input")
    | Peek Byte -> Ok (get_char t.i t.i_pos)
    | Junk Byte ->
      if t.i_pos < bigstring_length t.i then (
        t.i_pos <- t.i_pos + 1
        ; Ok ())
      else raise Out_of_bound
    | Peek (Short endian) -> Ok (get_int16 t.i t.i_pos endian)
    | Junk (Short _) ->
      if t.i_pos + 1 < bigstring_length t.i then (
        t.i_pos <- t.i_pos + 2
        ; Ok ())
      else raise Out_of_bound
    | Count -> ( match count t with Ok v -> Ok v | Error err -> Error err)
    | Transmit (len, state) -> (
      t.state <- state
      ; match transmit t len with Ok v -> Ok v | Error err -> Error err)
    | Copy ({off; len}, state) -> (
      t.state <- state
      ; let fiber =
          copy t ~off ~len:(len + 2) >>= fun () -> copy_done ~transmit t in
        match fiber with Ok v -> Ok v | Error err -> Error err) in
  let unroll t fiber : _ result =
    match go t fiber with Ok v -> Ok v | Error err -> Error err in
  unroll t fiber

module DSL = struct
  let return x = Return x
  let ( >>= ) x f = Bind (x, f)
  let peek v = Peek v
  let junk v = Junk v
  let byte = Byte
  let state = State
  let transmit ~len state = Transmit (len, state)
  let count = Count
  let copy ~off ~len state = Copy ({off; len}, state)
  let leshort = Short `LE
  let end_of_lzo = Return ()
  let fix f = Fix f
  let malformedf fmt = kstrf (fun s -> Fail (`Malformed s)) fmt

  let read v =
    peek v >>= fun r ->
    junk v >>= fun () -> return r
end

let fiber : (unit, [> error ]) t =
  let open DSL in
  fix @@ fun m ->
  read byte >>= fun chr ->
  state >>= fun state ->
  match chr, (state :> int) land 3 with
  | '\001' .. '\015', 0 ->
    transmit ~len:(Char.code chr + 3) State._no_extra >>= fun () -> m
  | '\000', 0 ->
    count >>= fun count ->
    let len = 3 + 15 + count in
    transmit ~len State._no_extra >>= fun () -> m
  | '\000' .. '\015', (1 | 2 | 3) ->
    let d, state = Char.code chr lsr 2, State.of_int (Char.code chr land 0b11) in
    read byte >>= fun h ->
    let off = (Char.code h lsl 2) + d + 1 in
    copy ~off ~len:0 state >>= fun () -> m
  | '\000' .. '\015', -1 ->
    read byte >>= fun h ->
    let state = State.of_int (Char.code chr land 0b11) in
    let off = (Char.code h lsl 2) + (Char.code chr lsr 2) + 2049 in
    copy ~off ~len:1 state >>= fun () -> m
  | '\016' .. '\031', _ ->
    let length = Char.code chr land 0b111 in
    let with_length len =
      read leshort >>= fun s ->
      let off =
        let h = (Char.code chr land 8) lsr 3 in
        16384 + (h lsl 14) + (s lsr 2) in
      let state = State.of_int (s land 0xff) in
      if off = 16384 then end_of_lzo else copy ~off ~len state >>= fun () -> m
    in
    if length = 0 then count >>= fun count -> with_length (7 + count)
    else with_length length
  | '\032' .. '\063', _ ->
    let with_length len =
      read leshort >>= fun s ->
      let state = State.of_int (s land 0xff) in
      let off = succ (s lsr 2) in
      copy ~off ~len state >>= fun () -> m in
    let length = Char.code chr land 0b11111 in
    if length = 0 then count >>= fun count -> with_length (31 + count)
    else with_length length
  | '\064' .. '\255', _ ->
    let state = State.of_int (Char.code chr) in
    let len, d =
      ( (Char.code chr lsr 5) - 1
      , (* t = (t >> 5) - 1 *)
        (Char.code chr lsr 2) land 7 )
      (* m_pos = (t >> 2) & 7 *) in
    read byte >>= fun h ->
    let off = (Char.code h lsl 3) + d + 1 in
    copy ~off ~len state >>= fun () -> m
  | _ -> assert false

(* TODO: replace it by something else to ensure exhaustive pattern-matching. *)

let fiber : (unit, [> error ]) t =
  let open DSL in
  peek byte >>= fun chr ->
  match chr with
  | '\016' -> malformedf "No dictionary at offset 0 available"
  | '\000' .. '\017' -> fiber
  | '\018' ->
    junk byte >>= fun () ->
    transmit ~len:1 State._0 >>= fun () -> fiber
  | '\019' ->
    junk byte >>= fun () ->
    transmit ~len:2 State._0 >>= fun () -> fiber
  | '\020' ->
    junk byte >>= fun () ->
    transmit ~len:3 State._0 >>= fun () -> fiber
  | '\021' ->
    junk byte >>= fun () ->
    transmit ~len:4 State._0 >>= fun () -> fiber
  | '\022' .. '\255' as chr ->
    let len = Char.code chr - 17 in
    junk byte >>= fun () ->
    transmit ~len State._0 >>= fun () -> fiber

let uncompress input output : (bigstring, [> error ]) result =
  let v = {i= input; i_pos= 0; o= output; o_pos= 0; state= State._0} in
  match run ~transmit ~copy fiber v with
  | Ok () ->
    Ok (bigstring_sub output 0 v.o_pos)
    (* TODO(dinosaure): we can replace it by [unsafe_sub]. *)
  | Error (#error as err) -> Error err
  | exception Out_of_bound ->
    Error (`Invalid_argument "Input is malformed or output is not large enough")

let uncompress_with_buffer ?(chunk = 0x1000) input : (string, [> error ]) result
    =
  let v = {i= input; i_pos= 0; o= bigstring_empty; o_pos= 0; state= State._0} in
  let buf = Buffer.create chunk in
  let transmit v len = transmit_to_buffer buf v len in
  let copy v ~off ~len = copy_to_buffer buf v ~off ~len in
  match run ~transmit ~copy fiber v with
  | Ok () -> Ok (Buffer.contents buf)
  | Error (#error as err) -> Error err
  | exception Out_of_bound -> Error (`Malformed "Malformed input")

(* inflate *)

let _m3_marker = 32
let _m4_marker = 16
let _m2_max_len = 8
let _m3_max_len = 33
let _m4_max_len = 9
let _m2_max_offset = 0x0800
let _m3_max_offset = 0x4000
let ( .%[] ) buf ofs = get_int8 buf ofs
let ( .%[]<- ) buf ofs v = set_int8 buf ofs v

let index =
  [|
     0; 1; 2; 53; 3; 7; 54; 27; 4; 38; 41; 8; 34; 55; 48; 28; 62; 5; 39; 46; 44
   ; 42; 22; 9; 24; 35; 59; 56; 49; 18; 29; 11; 63; 52; 6; 26; 37; 40; 33; 47
   ; 61; 45; 43; 21; 23; 58; 17; 10; 51; 25; 36; 32; 60; 20; 57; 16; 50; 31; 19
   ; 15; 30; 14; 13; 12
  |]

let ctz v =
  let neg = Int64.neg in
  let ( land ) = Int64.logand in
  let ( * ) = Int64.mul in
  let ( >> ) = Int64.shift_right_logical in
  let idx = v land neg v * 0x022fdd63cc95386dL >> 58 in
  index.(Int64.to_int idx)

let record_match ~off ~len out_data _anchor out_pos =
  let out_pos = ref out_pos in

  (if len <= _m2_max_len && off <= _m2_max_offset then (
     let off = off - 1 in
     out_data.%[!out_pos] <- ((len - 1) lsl 5) lor ((off land 7) lsl 2)
     ; incr out_pos
     ; out_data.%[!out_pos] <- off asr 3
     ; incr out_pos)
   else if off <= _m3_max_offset then (
     let off = off - 1 in
     (if len <= _m3_max_len then (
        out_data.%[!out_pos] <- _m3_marker lor (len - 2)
        ; incr out_pos)
      else
        let len = ref (len - _m3_max_len) in
        out_data.%[!out_pos] <- _m3_marker lor 0
        ; incr out_pos
        ; while !len > 255 do
            len := !len - 255
            ; out_data.%[!out_pos] <- 0
            ; incr out_pos
          done
        ; out_data.%[!out_pos] <- !len
        ; incr out_pos)
     ; out_data.%[!out_pos] <- off lsl 2
       ; incr out_pos
       ; out_data.%[!out_pos] <- off asr 6
       ; incr out_pos)
   else
     let off = off - 0x4000 in
     (if len <= _m4_max_len then (
        out_data.%[!out_pos] <-
          _m4_marker lor ((off asr 11) land 8) lor (len - 2)
        ; incr out_pos)
      else
        let len = ref (len - _m4_max_len) in
        out_data.%[!out_pos] <- _m4_marker lor ((off asr 11) land 8)
        ; incr out_pos
        ; while !len > 255 do
            len := !len - 255
            ; out_data.%[!out_pos] <- 0
            ; incr out_pos
          done
        ; out_data.%[!out_pos] <- !len
        ; incr out_pos)
     ; out_data.%[!out_pos] <- off lsl 2
       ; incr out_pos
       ; out_data.%[!out_pos] <- off asr 6
       ; incr out_pos)
  ; !out_pos

let record_literals ~off ~len in_data out_data _anchor out_pos =
  let out_pos = ref out_pos in
  let in_pos = ref off in

  if len > 0 then
    if len <= 3 then (
      out_data.%[!out_pos - 2] <- out_data.%[!out_pos - 2] lor len
      ; blit in_data off out_data !out_pos 4
      ; out_pos := !out_pos + len)
    else if len <= 16 then (
      out_data.%[!out_pos] <- len - 3
      ; incr out_pos
      ; blit in_data off out_data !out_pos 8
      ; blit in_data (off + 8) out_data (!out_pos + 8) 8
      ; out_pos := !out_pos + len)
    else (
      (if len <= 18 then (
         out_data.%[!out_pos] <- len - 3
         ; incr out_pos)
       else
         let len' = ref (len - 18) in
         out_data.%[!out_pos] <- 0
         ; incr out_pos
         ; while !len' > 255 do
             len' := !len' - 255
             ; out_data.%[!out_pos] <- 0
             ; incr out_pos
           done
         ; out_data.%[!out_pos] <- !len'
         ; incr out_pos)
      ; blit in_data off out_data !out_pos len
        ; out_pos := !out_pos + len
        ; in_pos := !in_pos + len)
  ; !out_pos, !in_pos

let record_trailer ~off ~len in_data out_data out_pos =
  let out_pos = ref out_pos in

  if len > 0 then (
    (if !out_pos = 0 && len < 238 then (
       out_data.%[!out_pos] <- 17 + len
       ; incr out_pos)
     else if len <= 3 then
       out_data.%[!out_pos - 2] <- out_data.%[!out_pos - 2] lor len
     else if len <= 18 then (
       out_data.%[!out_pos] <- len - 3
       ; incr out_pos)
     else
       let len' = ref (len - 18) in
       out_data.%[!out_pos] <- 0
       ; incr out_pos
       ; while !len' > 255 do
           len' := !len' - 255
           ; out_data.%[!out_pos] <- 0
           ; incr out_pos
         done
       ; out_data.%[!out_pos] <- !len'
       ; incr out_pos)
    ; blit in_data off out_data !out_pos len)
  ; out_pos := !out_pos + len
  ; out_data.%[!out_pos] <- _m4_marker lor 1
  ; incr out_pos
  ; out_data.%[!out_pos] <- 0
  ; incr out_pos
  ; out_data.%[!out_pos] <- 0
  ; incr out_pos
  ; !out_pos

let compress in_data in_pos in_len out_data out_pos _out_len t wrkmem =
  let idx_end = max 0 (in_len - 20) in

  let rec literal idx0 idx1 op t =
    (* literal: *)
    let idx0 = idx0 + (1 + ((idx0 - idx1) asr 5)) in
    (* TODO: check [lsr]. *)
    next idx0 idx1 op t
  and next idx0 idx1 op t =
    (* next: *)
    if idx0 - in_pos >= idx_end then
      (* break *)
      let idx1 = idx1 - t and t = 0 in
      in_len - (idx1 - in_pos - t), op
    else
      let v = get_int32_le in_data idx0 in
      let index =
        Int32.(
          logand
            (shift_right (mul 0x1824429dl v) (32 - 14))
            (sub (shift_left 1l 14) 1l)) in
      let index = Int32.to_int index in
      let reference = wrkmem.{index} + in_pos in
      wrkmem.{index} <- idx0 - in_pos

      ; if v <> get_int32_le in_data reference then literal idx0 idx1 op t
        else
          let idx1 = idx1 - t in
          let t = 0 in
          let unrecorded = idx0 - idx1 in
          let op, _idx1 =
            record_literals ~off:idx1 ~len:unrecorded in_data out_data out_pos
              op in

          let len = ref 4 in
          while
            idx0 + !len - in_pos < idx_end
            && get_int64_le in_data (idx0 + !len)
               = get_int64_le in_data (reference + !len)
            (* XXX(dinosaure): may be [_le] is not needed. *)
          do
            len := !len + 8
          done
          (* XXX(dinosaure): it seems that [minilzo] does not call [ctz] at the end of [progl].
             May be we do an unsafe access! TODO! *)
          ; if idx0 + !len - in_pos < in_len then
              len :=
                !len
                + ctz
                    (Int64.logxor
                       (get_int64_le in_data (idx0 + !len))
                       (get_int64_le in_data (reference + !len)))
                  / 8
          ; let op =
              record_match ~off:(idx0 - reference) ~len:!len out_data out_pos op
            in
            next (idx0 + !len) (idx0 + !len) op t in
  let idx0 = in_pos + if t < 4 then 4 - t else 0 in
  literal idx0 in_pos out_pos t

type wrkmem =
  (int, Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let make_wrkmem () =
  Bigarray.Array1.create Bigarray.int16_unsigned Bigarray.c_layout (1 lsl 14)

module Wrkmem = struct let memset t v = Bigarray.Array1.fill t v end

let compress in_data in_len out_data out_len wrkmem =
  let rec go idx len out_pos t =
    if len <= 20 then trailer idx len out_pos t
    else
      (* len > 20 *)
      let ll = min len 49152 in
      if (t + ll) lsr 5 <= 0 then trailer idx len out_pos t
      else (
        Wrkmem.memset wrkmem 0
        ; let t, out_pos =
            compress in_data idx ll out_data out_pos out_len t wrkmem in
          go (idx + ll) (len - ll) out_pos t)
  and trailer _idx len out_pos t =
    let t = t + len in
    let out_pos =
      record_trailer ~off:(in_len - t) ~len:t in_data out_data out_pos in
    out_pos in
  try go 0 in_len 0 0
  with Out_of_bound -> invalid_arg "lzo: output is not large enough"

let compress in_data out_data wrkmem =
  Wrkmem.memset wrkmem 0
  ; compress in_data (bigstring_length in_data) out_data
      (bigstring_length out_data)
      wrkmem
