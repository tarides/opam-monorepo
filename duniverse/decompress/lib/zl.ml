let io_buffer_size = 65536
let kstrf k fmt = Format.kasprintf k fmt
let invalid_arg fmt = Format.kasprintf invalid_arg fmt

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type window = De.window

let bigstring_create l =
  Bigarray.Array1.create Bigarray.char Bigarray.c_layout l

let bigstring_empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0
let bigstring_length x = Bigarray.Array1.dim x [@@inline]
let bigstring_sub x = Bigarray.Array1.sub x [@@inline]

external unsafe_get_uint8 : bigstring -> int -> int = "%caml_ba_ref_1"
external unsafe_get_uint16 : bigstring -> int -> int = "%caml_bigstring_get16"
external unsafe_get_uint32 : bigstring -> int -> int32 = "%caml_bigstring_get32"
external unsafe_set_uint8 : bigstring -> int -> int -> unit = "%caml_ba_set_1"
external swap16 : int -> int = "%bswap16"

let _unsafe_get_uint16_be v i =
  if Sys.big_endian then unsafe_get_uint16 v i
  else swap16 (unsafe_get_uint16 v i)

let unsafe_get_uint16_le v i =
  if Sys.big_endian then swap16 (unsafe_get_uint16 v i)
  else unsafe_get_uint16 v i

external unsafe_set_uint16 : bigstring -> int -> int -> unit
  = "%caml_bigstring_set16"

external unsafe_set_uint32 : bigstring -> int -> int32 -> unit
  = "%caml_bigstring_set32"

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "caml_int32_bswap"

let string_unsafe_get_uint8 : string -> int -> int =
 fun buf off -> Char.code buf.[off]

external string_unsafe_get_uint32 : string -> int -> int32
  = "%caml_string_get32"

let bytes_unsafe_get_uint8 : bytes -> int -> int =
 fun buf off -> Char.code (Bytes.get buf off)

external bytes_unsafe_get_uint32 : bytes -> int -> int32 = "%caml_bytes_get32"

let bytes_unsafe_set_uint8 : bytes -> int -> int -> unit =
 fun buf off v -> Bytes.set buf off (Char.unsafe_chr (v land 0xff))

external bytes_unsafe_set_uint32 : bytes -> int -> int32 -> unit
  = "%caml_bytes_set32"

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

let output_bigstring oc buf off len =
  (* XXX(dinosaure): stupidly slow! *)
  let v = Bigarray.Array1.sub buf off len in
  let v = bigstring_to_string v in
  output_string oc v

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

let unsafe_get_uint32_be =
  if Sys.big_endian then fun buf off -> unsafe_get_uint32 buf off
  else fun buf off -> swap32 (unsafe_get_uint32 buf off)

let unsafe_set_uint32_be =
  if Sys.big_endian then fun buf off v -> unsafe_set_uint32 buf off v
  else fun buf off v -> unsafe_set_uint32 buf off (swap32 v)

let unsafe_set_uint16_be =
  if Sys.big_endian then fun buf off v -> unsafe_set_uint16 buf off v
  else fun buf off v -> unsafe_set_uint16 buf off (swap16 v)

let _unsafe_set_uint16_le =
  if Sys.big_endian then fun buf off v -> unsafe_set_uint16 buf off (swap16 v)
  else fun buf off v -> unsafe_set_uint16 buf off v

let invalid_bounds off len =
  invalid_arg "Out of bounds (off: %d, len: %d)" off len

let _deflated = 8 (* Compression method *)

module Inf = struct
  type src = [ `Channel of in_channel | `String of string | `Manual ]

  (* XXX(dinosaure): immutable style. *)
  type decoder = {
      src: De.Inf.src
    ; i: bigstring
    ; i_pos: int
    ; i_len: int
    ; f: bool
    ; wr: int
    ; hd: int
    ; dd: dd
    ; fdict: bool
    ; flevel: int
    ; cinfo: int
    ; allocate: int -> De.window
    ; t: bigstring
    ; t_need: int
    ; t_len: int
    ; k: decoder -> signal
  }

  and dd =
    | Dd of {state: De.Inf.decoder; window: De.window; o: De.bigstring}
    | Hd of {o: De.bigstring}

  and signal =
    [ `Await of decoder
    | `Flush of decoder
    | `End of decoder
    | `Malformed of string ]

  let malformedf fmt = kstrf (fun s -> `Malformed s) fmt
  let err_unexpected_end_of_input _ = malformedf "Unexpected end of input"

  let err_invalid_checksum has expect _ =
    malformedf "Invalid checksum (expect:%04lx, has:%04lx)" expect
      (Optint.to_unsigned_int32 has)

  let err_invalid_header _ = malformedf "Invalid Zlib header"

  (* remaining bytes to read [d.i] *)
  let i_rem d = d.i_len - d.i_pos + 1 [@@inline]

  (* End of input [eoi] is signalled by [d.i_pos = 0] and [d.i_len = min_int]
     which implies [i_rem d < 0] is [true]. *)

  let eoi d = {d with i= bigstring_empty; i_pos= 0; i_len= min_int}

  let refill k d =
    match d.dd, d.src with
    | Dd {state; _}, `String _ ->
      De.Inf.src state bigstring_empty 0 0
      ; k (eoi d)
    | Dd {state; _}, `Channel ic ->
      let res = input_bigstring ic d.i 0 (bigstring_length d.i) in
      De.Inf.src state d.i 0 res ; k d
    | (Dd _ | Hd _), `Manual -> `Await {d with k}
    | Hd _, `String _ -> k (eoi d)
    | Hd _, `Channel ic ->
      let res = input_bigstring ic d.i 0 (bigstring_length d.i) in
      if res == 0 then k (eoi d) else k {d with i_pos= 0; i_len= res - 1}

  let flush k d = `Flush {d with k}

  let blit src ~src_off dst ~dst_off ~len =
    let a = Bigarray.Array1.sub src src_off len in
    let b = Bigarray.Array1.sub dst dst_off len in
    Bigarray.Array1.blit a b

  let rec t_fill k d =
    let blit d len =
      blit d.i ~src_off:d.i_pos d.t ~dst_off:d.t_len ~len
      ; {d with i_pos= d.i_pos + len; t_len= d.t_len + len} in
    let rem = i_rem d in
    if rem < 0 then malformedf "Unexpected end of input"
    else
      let need = d.t_need - d.t_len in
      if rem < need then
        let d = blit d rem in
        refill (t_fill k) d
      else
        let d = blit d need in
        k {d with t_need= 0}

  let t_need n d = {d with t_need= n}

  let checksum d =
    let k d =
      match d.dd with
      | Dd {state; _} ->
        let a = De.Inf.checksum state in
        let b = unsafe_get_uint32_be d.t 0 in

        if
          Optint.to_unsigned_int32 a = b
          (* FIXME: Optint.equal a (Optint.of_int32 b) bugs! *)
        then `End d
        else err_invalid_checksum a b d
      | Hd _ -> assert false in

    t_fill k (t_need 4 d)

  let rec header d =
    let k d =
      let[@warning "-partial-match"] (Hd {o}) = d.dd in
      let cmf = unsafe_get_uint16_le d.t 0 in
      let cm = cmf land 0b1111 in
      let cinfo = (cmf lsr 4) land 0b1111 in
      let flg = cmf lsr 8 in
      let fdict = (flg lsr 5) land 0b1 in
      let flevel = (flg lsr 6) land 0b11 in
      let window = d.allocate (cinfo + 8) in
      let state = De.Inf.decoder `Manual ~o ~w:window in
      let dd = Dd {state; window; o} in
      if (((cmf land 0xff) lsl 8) + (cmf lsr 8)) mod 31 != 0 || cm != _deflated
      then err_invalid_header d
      else (
        if i_rem d > 0 then De.Inf.src state d.i d.i_pos (i_rem d)
        ; decode
            {
              d with
              hd= unsafe_get_uint16_le d.t 0
            ; k= decode
            ; dd
            ; t_need= 0
            ; t_len= 0
            ; fdict= fdict == 1
            ; flevel
            ; cinfo
            }) in
    if i_rem d >= 2 then (
      unsafe_set_uint16 d.t 0 (unsafe_get_uint16 d.i d.i_pos)
      ; k {d with i_pos= d.i_pos + 2})
    else if i_rem d < 0 then err_unexpected_end_of_input d
    else if i_rem d == 0 then refill header d
    else t_fill k (t_need 2 d)

  and decode d =
    match d.dd with
    | Hd _ -> header d
    | Dd {state; o; _} -> (
      match De.Inf.decode state with
      | `Flush ->
        if d.f then flush decode d
        else
          let len = bigstring_length o - De.Inf.dst_rem state in
          flush decode {d with wr= d.wr + len; f= true}
      | `Await ->
        let len = i_rem d - De.Inf.src_rem state in
        refill decode {d with i_pos= d.i_pos + len}
      | `End ->
        if d.f then flush decode d
        else
          let len = bigstring_length o - De.Inf.dst_rem state in
          if len > 0 then
            flush decode
              {
                d with
                i_pos= d.i_pos + (i_rem d - De.Inf.src_rem state)
              ; wr= d.wr + len
              ; f= true
              }
          else
            checksum {d with i_pos= d.i_pos + (i_rem d - De.Inf.src_rem state)}
      | `Malformed err -> `Malformed err)

  let src d s j l =
    if j < 0 || l < 0 || j + l > bigstring_length s then invalid_bounds j l
    ; let d =
        if l == 0 then eoi d else {d with i= s; i_pos= j; i_len= j + l - 1}
      in
      match d.dd with Dd {state; _} -> De.Inf.src state s j l ; d | Hd _ -> d

  let flush d =
    match d.dd with
    | Hd _ -> {d with f= false}
    | Dd {state; _} -> De.Inf.flush state ; {d with f= false}

  let dst_rem d =
    match d.dd with Hd _ -> 0 | Dd {state; _} -> De.Inf.dst_rem state

  let src_rem d = i_rem d
  let write {wr; _} = wr

  let decoder src ~o ~allocate =
    let i, i_pos, i_len =
      match src with
      | `Manual -> bigstring_empty, 1, 0
      | `String x -> bigstring_of_string x, 0, String.length x - 1
      | `Channel _ -> bigstring_create io_buffer_size, 1, 0 in
    {
      i
    ; i_pos
    ; i_len
    ; src
    ; f= false
    ; wr= 0
    ; hd= 0
    ; dd= Hd {o}
    ; fdict= false
    ; flevel= 2
    ; cinfo= 8
    ; allocate
    ; t= bigstring_create 4
    ; t_need= 0
    ; t_len= 0
    ; k= decode
    }

  let reset d =
    let i, i_pos, i_len =
      match d.src with
      | `Manual -> bigstring_empty, 1, 0
      | `String x -> bigstring_of_string x, 0, String.length x - 1
      | `Channel _ -> bigstring_create io_buffer_size, 1, 0 in
    let o = match d.dd with Hd {o} -> o | Dd {o; _} -> o in
    {
      i
    ; i_pos
    ; i_len
    ; src= d.src
    ; f= false
    ; wr= 0
    ; hd= 0
    ; dd= Hd {o}
    ; fdict= false
    ; flevel= 2
    ; cinfo= 8
    ; allocate= d.allocate
    ; t= d.t
    ; t_need= 0
    ; t_len= 0
    ; k= decode
    }

  let decode d = d.k d

  module Ns = struct
    type error = [ `Invalid_header | `Invalid_checksum | De.Inf.Ns.error ]

    let pp_error ppf e =
      match e with
      | `Invalid_header -> Format.fprintf ppf "Invalid header"
      | `Invalid_checksum -> Format.fprintf ppf "Invalid checksum"
      | #De.Inf.Ns.error as e -> De.Inf.Ns.pp_error ppf e

    let header src =
      let cmf = unsafe_get_uint16_le src 0 in
      let cm = cmf land 0b1111 in
      let _cinfo = (cmf lsr 4) land 0b1111 in
      let flg = cmf lsr 8 in
      let _fdict = (flg lsr 5) land 0b1 in
      let _flevel = (flg lsr 6) land 0b11 in
      (((cmf land 0xff) lsl 8) + (cmf lsr 8)) mod 31 != 0 || cm != _deflated

    let inflate src dst =
      let src_len = bigstring_length src in
      if src_len < 2 then Error `Unexpected_end_of_input
      else if header src then Error `Invalid_header
      else
        let sub_src = bigstring_sub src 2 (bigstring_length src - 6) in
        let res = De.Inf.Ns.inflate sub_src dst in
        match res with
        | Ok (i, o) ->
          if src_len < i + 6 then Error `Unexpected_end_of_input
          else
            let i_adl32 = unsafe_get_uint32_be src (i + 2) in
            let o_adl32 =
              Optint.to_int32
                Checkseum.Adler32.(unsafe_digest_bigstring dst 0 o default)
            in
            if i_adl32 <> o_adl32 then Error `Invalid_checksum else Ok (i + 6, o)
        | Error e -> Error (e : De.Inf.Ns.error :> [> error ])
  end
end

module Def = struct
  type src = [ `Channel of in_channel | `String of string | `Manual ]
  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]

  type encoder = {
      src: src
    ; dst: dst
    ; level: int
    ; dynamic: bool
    ; i: bigstring
    ; i_pos: int
    ; i_len: int
    ; o: bigstring
    ; o_pos: int
    ; o_len: int
    ; q: De.Queue.t
    ; s: De.Lz77.state
    ; e: De.Def.encoder
    ; w: De.Lz77.window
    ; state: state
    ; first: bool
    ; k: encoder -> [ `Await of encoder | `Flush of encoder | `End of encoder ]
  }

  and state =
    | Hd
    (* header process *)
    | Dd

  (* DEFLATE process *)

  type ret = [ `Await of encoder | `End of encoder | `Flush of encoder ]

  let o_rem e = e.o_len - e.o_pos + 1
  let i_rem s = s.i_len - s.i_pos + 1

  let eoi e =
    De.Lz77.src e.s bigstring_empty 0 0
    ; {e with i= bigstring_empty; i_pos= 0; i_len= min_int}

  let src e s j l =
    if j < 0 || l < 0 || j + l > bigstring_length s then invalid_bounds j l
    ; De.Lz77.src e.s s j l
    ; if l == 0 then eoi e else {e with i= s; i_pos= j; i_len= j + l - 1}

  let dst e s j l =
    if j < 0 || l < 0 || j + l > bigstring_length s then invalid_bounds j l
    ; (match e.state with Hd -> () | Dd -> De.Def.dst e.e s j l)
    ; {e with o= s; o_pos= j; o_len= j + l - 1}

  let refill k e =
    match e.src with
    | `String _ -> k (eoi e)
    | `Channel ic ->
      let res = input_bigstring ic e.i 0 (bigstring_length e.i) in
      k (src e e.i 0 res)
    | `Manual -> `Await {e with k}

  let flush k e =
    match e.dst with
    | `Buffer b ->
      let len = bigstring_length e.o - o_rem e in
      for i = 0 to len - 1 do
        Buffer.add_char b e.o.{i}
      done
      ; k (dst e e.o 0 (bigstring_length e.o))
    | `Channel oc ->
      output_bigstring oc e.o 0 (bigstring_length e.o - o_rem e)
      ; k (dst e e.o 0 (bigstring_length e.o))
    | `Manual -> `Flush {e with k}

  let identity e = `End e

  let rec checksum e =
    let k e =
      let checksum = Optint.to_int32 (De.Lz77.checksum e.s) in
      unsafe_set_uint32_be e.o e.o_pos checksum
      ; flush identity {e with o_pos= e.o_pos + 4} in
    if o_rem e >= 4 then k e else flush checksum e

  let make_block ?(last = false) e =
    if De.Lz77.no_compression e.s then {De.Def.kind= De.Def.Flat; last}
    else if e.dynamic then
      let literals = De.Lz77.literals e.s in
      let distances = De.Lz77.distances e.s in
      De.Def.block_of_frequencies ~last ~literals ~distances
    else {De.Def.kind= De.Def.Fixed; last}

  let rec encode e =
    match e.state with
    | Hd ->
      let k e =
        let window_bits = 15 in
        let header = (_deflated + ((window_bits - 8) lsl 4)) lsl 8 in
        let header = header lor (e.level lsl 6) in
        let header = header + (31 - (header mod 31)) in
        unsafe_set_uint16_be e.o e.o_pos header
        ; if i_rem e > 0 then De.Lz77.src e.s e.i e.i_pos (i_rem e)
        ; (* XXX(dinosaure): we need to protect [e.s] against EOI signal. *)
          De.Def.dst e.e e.o (e.o_pos + 2) (o_rem e - 2)
        ; encode {e with state= Dd; o_pos= e.o_pos + 2} in
      if o_rem e >= 2 then k e else flush encode e
    | Dd ->
      let rec partial k e = k e (De.Def.encode e.e `Await)
      and compress e =
        match De.Lz77.compress e.s with
        | `Await ->
          refill compress
            {e with i_pos= e.i_pos + (i_rem e - De.Lz77.src_rem e.s)}
        | `Flush when e.first ->
          encode_deflate {e with first= false}
            (De.Def.encode e.e (`Block (make_block e)))
        | `Flush -> encode_deflate e (De.Def.encode e.e `Flush)
        | `End ->
          let block = make_block ~last:true e in
          trailing e (De.Def.encode e.e (`Block block))
      and encode_deflate e = function
        | `Partial ->
          let len = o_rem e - De.Def.dst_rem e.e in
          flush (partial encode_deflate) {e with o_pos= e.o_pos + len}
        | `Ok -> compress e
        | `Block ->
          let block = make_block e in
          encode_deflate e (De.Def.encode e.e (`Block block))
      and trailing e = function
        | `Partial ->
          let len = o_rem e - De.Def.dst_rem e.e in
          flush (partial trailing) {e with o_pos= e.o_pos + len}
        | `Ok ->
          let len = o_rem e - De.Def.dst_rem e.e in
          checksum {e with o_pos= e.o_pos + len}
        | `Block -> assert false
        (* XXX(dinosaure): should never occur! *) in

      compress e

  let src_rem = i_rem
  let dst_rem = o_rem

  let encoder ?(dynamic = true) ~q ~w ~level src dst =
    let i, i_pos, i_len =
      match src with
      | `Manual -> bigstring_empty, 1, 0
      | `String x -> bigstring_of_string x, 0, String.length x - 1
      | `Channel _ -> bigstring_create io_buffer_size, 1, 0 in
    let o, o_pos, o_len =
      match dst with
      | `Manual -> bigstring_empty, 1, 0
      | `Buffer _ | `Channel _ ->
        bigstring_create io_buffer_size, 0, io_buffer_size - 1 in
    {
      src
    ; dst
    ; i
    ; i_pos
    ; i_len
    ; o
    ; o_pos
    ; o_len
    ; level=
        (match level with 0 -> 0 | 1 | 2 | 3 | 4 | 5 -> 1 | 6 -> 2 | _ -> 3)
    ; dynamic
    ; e= De.Def.encoder `Manual ~q
    ; s= De.Lz77.state ~level `Manual ~q ~w
    ; q
    ; w
    ; state= Hd
    ; first= true
    ; k= encode
    }

  let encode e = e.k e

  module Ns = struct
    type error = De.Def.Ns.error

    let pp_error ppf e =
      match e with #De.Def.Ns.error as e -> De.Def.Ns.pp_error ppf e

    let compress_bound len = De.Def.Ns.compress_bound len + 6

    let header dst level =
      let window_bits = 15 in
      let header = (_deflated + ((window_bits - 8) lsl 4)) lsl 8 in
      let level =
        match level with 0 | 1 -> 0 | 2 | 3 | 4 | 5 -> 1 | 6 -> 2 | _ -> 3 in
      let header = header lor (level lsl 6) in
      let header = header + (31 - (header mod 31)) in
      unsafe_set_uint16_be dst 0 header

    let deflate ?(level = 4) src dst =
      if bigstring_length dst < 2 then Error `Unexpected_end_of_output
      else begin
        header dst level
        ; let sub_dst = bigstring_sub dst 2 (bigstring_length dst - 2) in
          let res = De.Def.Ns.deflate ~level src sub_dst in
          match res with
          | Ok res ->
            let adl32 =
              Checkseum.Adler32.(
                unsafe_digest_bigstring src 0 (bigstring_length src) default)
            in
            if bigstring_length sub_dst - res < 2 then
              Error `Unexpected_end_of_output
            else (
              unsafe_set_uint32_be sub_dst res (Optint.to_int32 adl32)
              ; Ok (res + 6))
          | Error e -> Error (e : De.Def.Ns.error :> [> error ])
      end
  end
end

module Higher = struct
  let compress ?(level = 6) ?dynamic ~w ~q ~refill ~flush i o =
    let encoder = Def.encoder `Manual `Manual ?dynamic ~q ~w ~level in
    let rec go encoder =
      match Def.encode encoder with
      | `Await encoder ->
        let len = refill i in
        go (Def.src encoder i 0 len)
      | `Flush encoder ->
        let len = bigstring_length o - Def.dst_rem encoder in
        flush o len
        ; go (Def.dst encoder o 0 (bigstring_length o))
      | `End encoder ->
        let len = bigstring_length o - Def.dst_rem encoder in
        if len > 0 then flush o len in
    go (Def.dst encoder o 0 (bigstring_length o))

  let uncompress ~allocate ~refill ~flush i o =
    let decoder = Inf.decoder `Manual ~allocate ~o in
    let rec go decoder =
      match Inf.decode decoder with
      | `Await decoder ->
        let len = refill i in
        go (Inf.src decoder i 0 len)
      | `Flush decoder ->
        let len = bigstring_length o - Inf.dst_rem decoder in
        flush o len
        ; go (Inf.flush decoder)
      | `End decoder ->
        let len = bigstring_length o - Inf.dst_rem decoder in
        if len > 0 then flush o len
        ; Ok ()
      | `Malformed err -> Error (`Msg err) in
    go decoder
end
