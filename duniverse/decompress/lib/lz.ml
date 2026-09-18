[@@@landmark "auto"]

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let bigstring_empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0
let bigstring_length x = Bigarray.Array1.dim x [@@inline]

let bigstring_create l =
  Bigarray.Array1.create Bigarray.char Bigarray.c_layout l

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let invalid_bounds off len =
  invalid_arg "Out of bounds (off: %d, len: %d)" off len

external unsafe_get_uint8 : bigstring -> int -> int = "%caml_ba_ref_1"
external unsafe_get_char : bigstring -> int -> char = "%caml_ba_ref_1"
external unsafe_set_uint8 : bigstring -> int -> int -> unit = "%caml_ba_set_1"
external unsafe_get_uint16 : bigstring -> int -> int = "%caml_bigstring_get16"
external unsafe_get_uint32 : bigstring -> int -> int32 = "%caml_bigstring_get32"

external unsafe_set_uint32 : bigstring -> int -> int32 -> unit
  = "%caml_bigstring_set32"

external bytes_unsafe_get_uint32 : bytes -> int -> int32 = "%caml_bytes_get32"

let bytes_unsafe_get_uint8 : bytes -> int -> int =
 fun buf off -> Char.code (Bytes.get buf off)

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

external string_unsafe_get_uint32 : string -> int -> int32
  = "%caml_string_get32"

let string_unsafe_get_uint8 : string -> int -> int =
 fun buf off -> Char.code buf.[off]

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

let _length =
  [|
     0; 1; 2; 3; 4; 5; 6; 7; 8; 8; 9; 9; 10; 10; 11; 11; 12; 12; 12; 12; 13; 13
   ; 13; 13; 14; 14; 14; 14; 15; 15; 15; 15; 16; 16; 16; 16; 16; 16; 16; 16; 17
   ; 17; 17; 17; 17; 17; 17; 17; 18; 18; 18; 18; 18; 18; 18; 18; 19; 19; 19; 19
   ; 19; 19; 19; 19; 20; 20; 20; 20; 20; 20; 20; 20; 20; 20; 20; 20; 20; 20; 20
   ; 20; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 22; 22
   ; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 23; 23; 23; 23; 23
   ; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 24; 24; 24; 24; 24; 24; 24; 24
   ; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24
   ; 24; 24; 24; 24; 24; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25
   ; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 26
   ; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26
   ; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 27; 27; 27; 27; 27; 27; 27
   ; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27
   ; 27; 27; 27; 27; 27; 28
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

let _max_match = 258
let _min_match = 3
let _min_lookahead = _max_match + _min_match + 1
let ( .!() ) buf pos = unsafe_get_uint32 buf pos
let ( .![] ) buf pos = unsafe_get_uint16 buf pos
let ( .!{} ) buf pos = unsafe_get_uint8 buf pos

type configuration = {
    max_chain: int
  ; max_lazy: int
  ; good_length: int
  ; nice_length: int
}

let _4 = {good_length= 4; max_lazy= 4; nice_length= 16; max_chain= 16}
let _5 = {good_length= 8; max_lazy= 16; nice_length= 32; max_chain= 32}
let _6 = {good_length= 8; max_lazy= 16; nice_length= 128; max_chain= 128}
let _7 = {good_length= 8; max_lazy= 32; nice_length= 128; max_chain= 256}
let _8 = {good_length= 32; max_lazy= 128; nice_length= 258; max_chain= 1024}
let _9 = {good_length= 32; max_lazy= 258; nice_length= 258; max_chain= 4096}
let _mem_level = 8 (* default *)
let _hash_bits = _mem_level + 7
let _hash_size = 1 lsl _hash_bits
let _hash_mask = _hash_size - 1
let _hash_shift = (_hash_bits + _min_match - 1) / _min_match
let _too_far = 4096
let update_hash hash chr = (hash lsl _hash_shift) lxor chr land _hash_mask

type src = [ `Channel of in_channel | `String of string | `Manual ]
type decode = [ `Await | `Flush | `End ]
type literals = De.literals
type distances = De.distances
type optint = Optint.t

type state = {
    src: src
  ; cfg: configuration
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
  ; mutable hash: int
  ; mutable match_start: int
  ; mutable match_length: int
  ; mutable match_available: bool
  ; mutable insert: int
  ; mutable prev_length: int
  ; mutable prev_match: int
  ; q: De.Queue.t
  ; mutable crc: optint
  ; mutable k: configuration -> state -> decode
}

let max_dist s = (1 lsl s.wbits) - _min_lookahead

exception Break

(* cur is the head of the hash chain for the current string
 * and its distance is <= _max_dist
 * prev_length >= 1
 * len >= _min_lookahead *)
let longest_match cfg s cur_match =
  let wsize = 1 lsl s.wbits in
  let wmask = wsize - 1 in
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
  let wsize = 1 lsl s.wbits in
  let wmask = wsize - 1 in
  s.hash <- update_hash s.hash s.w.!{str + (_min_match - 1)}
  ; let res = s.head.(s.hash) in
    s.prev.(str land wmask) <- res
    ; s.head.(s.hash) <- str
    ; res

let succ_length literals length =
  assert (length >= 3 && length <= 255 + 3)
  ; literals.(256 + 1 + _length.(length - 3)) <-
      literals.(256 + 1 + _length.(length - 3)) + 1

let succ_distance distances distance =
  assert (distance >= 1 && distance <= 32767 + 1)
  ; distances.(_distance (pred distance)) <-
      distances.(_distance (pred distance)) + 1

let emit_match s ~off ~len =
  De.Queue.push_exn s.q (De.Queue.cmd (`Copy (off, len)))
  ; succ_length (s.l :> int array) len
  ; succ_distance (s.d :> int array) off
  ; if De.Queue.available s.q = 1 then (
      De.Queue.push_exn s.q De.Queue.eob
      ; true)
    else false

let succ_literal literals chr =
  literals.(Char.code chr) <- literals.(Char.code chr) + 1

let emit_literal s chr =
  De.Queue.push_exn s.q (De.Queue.cmd (`Literal chr))
  ; succ_literal (s.l :> int array) chr
  ; if De.Queue.available s.q = 1 then (
      De.Queue.push_exn s.q De.Queue.eob
      ; true)
    else false

(* XXX(dinosaure): it's possible that it remains one literal. *)
let trailing s =
  if s.match_available then (
    let _ = emit_literal s (unsafe_get_char s.w (s.strstart - 1)) in
    s.insert <-
      (if s.strstart < _min_match - 1 then s.strstart else _min_match - 1)
    ; `End)
  else `End

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
    if rem < 0 then if s.lookahead > 0 then deflate_slow cfg s else trailing s
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
          s.hash <- s.w.!{!str}
          ; s.hash <- update_hash s.hash s.w.!{!str + 1}
          ; while s.lookahead + !insert >= _min_match && !insert != 0 do
              s.hash <- update_hash s.hash s.w.!{!str + _min_match - 1}
              ; s.prev.(!str land wmask) <- s.head.(s.hash)
              ; s.head.(s.hash) <- !str
              ; incr str
              ; decr insert
              ; if s.lookahead + !insert < _min_match then (
                  s.insert <- !insert
                  ; raise Break)
            done
          ; s.insert <- !insert
        end
      ; if s.lookahead < _min_lookahead && i_rem s >= 0 then
          refill fill_window s
        else deflate_slow cfg s
    with Break -> deflate_slow cfg s

and enough cfg s =
  if s.lookahead < _min_lookahead then fill_window cfg s else deflate_slow cfg s

and deflate_slow cfg s =
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
        else enough cfg s
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
        ; enough cfg s
      end
    else begin
      s.match_available <- true
      ; s.strstart <- s.strstart + 1
      ; s.lookahead <- s.lookahead - 1
      ; enough cfg s
    end

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
    | 0 | 1 | 2 | 3 | 4 -> _4
    | 5 -> _5
    | 6 -> _6
    | 7 -> _7
    | 8 -> _8
    | 9 -> _9
    | _ -> invalid_arg "Invalid compression level: %d" level in
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
  ; l= De.make_literals ()
  ; d= De.make_distances ()
  ; w
  ; wbits
  ; lookahead= 0
  ; strstart= 0
  ; prev= Array.make wsize 0
  ; head= Array.make _hash_size 0
  ; hash= 0
  ; match_start= 0
  ; match_length= _min_match - 1
  ; match_available= false
  ; insert= 0
  ; prev_length= _min_match - 1
  ; prev_match= 0
  ; q
  ; crc= Checkseum.Adler32.default
  ; k= enough
  }

let compress s = s.k s.cfg s

type window = bigstring

let make_window ~bits = bigstring_create ((1 lsl bits) * 2)
