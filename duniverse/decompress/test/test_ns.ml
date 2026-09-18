let seed = "kle6/0eMVRsY+AlbHjLTMQ=="

let () =
  let raw = Base64.decode_exn seed in
  let res = Array.make 8 0 in
  for i = 0 to 7 do
    res.(i) <- (Char.code raw.[i] lsl 8) lor Char.code raw.[i + 1]
  done
  ; Random.full_init res

let random len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set res i (Char.chr (Random.int 256))
  done
  ; Bytes.unsafe_to_string res

open De (* au detail *)

external string_unsafe_get_uint32 : string -> int -> int32
  = "%caml_string_get32"

let string_unsafe_get_uint8 : string -> int -> int =
 fun buf off -> Char.code buf.[off]

external unsafe_set_uint8 : bigstring -> int -> int -> unit = "%caml_ba_set_1"

external unsafe_set_uint32 : bigstring -> int -> int32 -> unit
  = "%caml_bigstring_set32"

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

let w = make_window ~bits:15
let src = bigstring_create io_buffer_size
let dst = bigstring_create io_buffer_size
let q = Queue.create 4096
let wrkmem = Lzo.make_wrkmem ()
let unsafe_get_uint8 b i = Char.code (Bstr.get b i)
let unsafe_get_uint32_be b i = Bstr.get_int32_be b i

let pp_chr =
  Fmt.using (function '\032' .. '\126' as x -> x | _ -> '.') Fmt.char

let pp_scalar : type buffer.
    get:(buffer -> int -> char) -> length:(buffer -> int) -> buffer Fmt.t =
 fun ~get ~length ppf b ->
  let l = length b in
  for i = 0 to l / 16 do
    Fmt.pf ppf "%08x: " (i * 16)
    ; let j = ref 0 in
      while !j < 16 do
        if (i * 16) + !j < l then
          Fmt.pf ppf "%02x" (Char.code @@ get b ((i * 16) + !j))
        else Fmt.pf ppf "  "
        ; if !j mod 2 <> 0 then Fmt.pf ppf " "
        ; incr j
      done
      ; Fmt.pf ppf "  "
      ; j := 0
      ; while !j < 16 do
          if (i * 16) + !j < l then
            Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
          else Fmt.pf ppf " "
          ; incr j
        done
      ; Fmt.pf ppf "@\n"
  done

let pp_string = pp_scalar ~get:String.get ~length:String.length
let str = Alcotest.testable pp_string String.equal

let check_decode =
  let pp ppf =
    Fmt.result ppf
      ~ok:(Fmt.pair ~sep:Fmt.comma Fmt.int Fmt.int)
      ~error:Inf.Ns.pp_error in
  let equal r1 r2 =
    match r1, r2 with
    | Ok (i1, o1), Ok (i2, o2) -> i1 == i2 && o1 == o2
    | Error e1, Error e2 -> e1 == e2
    | _ -> false in
  Alcotest.testable pp equal

let check_decode_o = function
  | Ok (_, v) -> v
  | Error _ -> raise Alcotest.Test_error

let check_encode = function Ok v -> v | Error _ -> raise Alcotest.Test_error

let encode ~block:kind lst =
  let res = Buffer.create 16 in
  let q = Queue.of_list lst in
  let encoder = Def.encoder (`Buffer res) ~q in
  match Def.encode encoder (`Block {kind; last= true}) with
  | `Block -> assert false
  | `Partial -> assert false
  | `Ok -> (
    match Def.encode encoder `Flush with
    | `Ok -> Buffer.contents res
    | `Block -> Alcotest.fail "Bad block"
    | `Partial -> assert false)

let encode_dynamic lst =
  let literals = make_literals () in
  let distances = make_distances () in
  List.iter
    (function
      | `Literal chr -> succ_literal literals chr
      | `Copy (off, len) ->
        succ_length literals len
        ; succ_distance distances off
      | _ -> ())
    lst
  ; let dynamic = Def.dynamic_of_frequencies ~literals ~distances in
    encode ~block:(Def.Dynamic dynamic) lst

let invalid_complement_of_length () =
  Alcotest.test_case "invalid complement of length" `Quick @@ fun () ->
  let src = bigstring_of_string "\x00\x00\x00\x00\x00" in
  Alcotest.(check check_decode)
    "invalid complement of length" (Error `Invalid_complement_of_length)
    (Inf.Ns.inflate src dst)

let invalid_kind_of_block () =
  Alcotest.test_case "invalid kind of block" `Quick @@ fun () ->
  let src = bigstring_of_string "\x06" in
  Alcotest.(check check_decode)
    "invalid kind of block" (Error `Invalid_kind_of_block)
    (Inf.Ns.inflate src dst)

let invalid_code_lengths () =
  Alcotest.test_case "invalid code lengths" `Quick @@ fun () ->
  let src = bigstring_of_string "\x04\x00\xfe\xff" in
  Alcotest.(check check_decode)
    "invalid code lengths" (Error `Invalid_dictionary) (Inf.Ns.inflate src dst)

let invalid_bit_length_repeat () =
  Alcotest.test_case "invalid bit length repeat" `Quick @@ fun () ->
  let src = bigstring_of_string "\x04\x00\x24\x49\x00" in
  Alcotest.(check check_decode)
    "invalid bit length repeat" (Error `Invalid_dictionary)
    (Inf.Ns.inflate src dst)

let invalid_codes () =
  Alcotest.test_case "invalid codes -- missing end-of-block" `Quick @@ fun () ->
  let src = bigstring_of_string "\x04\x00\x24\xe9\xff\x6d" in
  Alcotest.(check check_decode)
    "invalid codes -- missing end-of-block" (Error `Invalid_dictionary)
    (Inf.Ns.inflate src dst)

let invalid_lengths () =
  Alcotest.test_case "invalid literals/lengths" `Quick @@ fun () ->
  let src =
    bigstring_of_string
      "\x04\x80\x49\x92\x24\x49\x92\x24\x49\x92\x24\x71\xff\xff\x93\x11\x00"
  in
  Alcotest.(check check_decode)
    "invalid literals/lengths" (Error `Invalid_dictionary)
    (Inf.Ns.inflate src dst)

let invalid_distances () =
  Alcotest.test_case "invalid distances" `Quick @@ fun () ->
  let src =
    bigstring_of_string
      "\x04\x80\x49\x92\x24\x49\x92\x24\x0f\xb4\xff\xff\xc3\x84" in
  Alcotest.(check check_decode)
    "invalid distances" (Error `Invalid_dictionary) (Inf.Ns.inflate src dst)

let too_many_length_or_distance_symbols () =
  Alcotest.test_case "too many length of distance symbols" `Quick @@ fun () ->
  let src = bigstring_of_string "\xfc\x00\x00" in
  Alcotest.(check check_decode)
    "too many length of distance symbols" (Error `Unexpected_end_of_input)
    (Inf.Ns.inflate src dst)

(* XXX(dinosaure): error is not conform to what we expect (best will be [Invalid
   dictionary]), TODO! *)

let invalid_distance_code () =
  Alcotest.test_case "invalid distance code" `Quick @@ fun () ->
  let src = bigstring_of_string "\x02\x7e\xff\xff" in
  Alcotest.(check check_decode)
    "invalid distance code" (Error `Invalid_distance_code)
    (Inf.Ns.inflate src dst)

(* XXX(dinosaure): see [Inf.base_dist]'s comment about this behavior. *)

let invalid_distance_too_far_back () =
  Alcotest.test_case "invalid distance too far back" `Quick @@ fun () ->
  let src =
    bigstring_of_string "\x0c\xc0\x81\x00\x00\x00\x00\x00\x90\xff\x6b\x04\x00"
  in
  Alcotest.(check check_decode)
    "invalid distance too far back" (Error `Invalid_distance)
    (Inf.Ns.inflate src dst)

let invalid_flat_not_enough_output () =
  Alcotest.test_case "invalid output with flat block" `Quick @@ fun () ->
  let src = bigstring_of_string "\x01\x04\x00\xfb\xff\xde\xad\xbe\xef" in
  let dst = bigstring_create 0 in
  Alcotest.(check check_decode)
    "invalid distance too far back" (Error `Unexpected_end_of_output)
    (Inf.Ns.inflate src dst)

let invalid_literal_not_enough_output () =
  Alcotest.test_case "invalid output with literal" `Quick @@ fun () ->
  let q = Queue.of_list [`Literal 'a'; `End] in
  let b = Buffer.create 16 in
  let encoder = Def.encoder (`Buffer b) ~q in
  let go = function
    | `Ok -> Buffer.contents b
    | `Partial -> assert false
    | `Block -> assert false in
  let res = go (Def.encode encoder (`Block {Def.kind= Def.Fixed; last= true})) in
  let src = bigstring_of_string res in
  let dst = bigstring_create 0 in
  Alcotest.(check check_decode)
    "invalid distance too far back" (Error `Unexpected_end_of_output)
    (Inf.Ns.inflate src dst)

let invalid_copy_not_enough_output () =
  Alcotest.test_case "invalid output with copy" `Quick @@ fun () ->
  let q = Queue.of_list [`Literal 'a'; `Copy (1, 3); `End] in
  let b = Buffer.create 16 in
  let encoder = Def.encoder (`Buffer b) ~q in
  let go = function
    | `Ok -> Buffer.contents b
    | `Partial -> assert false
    | `Block -> assert false in
  let res = go (Def.encode encoder (`Block {Def.kind= Def.Fixed; last= true})) in
  let src = bigstring_of_string res in
  let dst = bigstring_create 1 in
  Alcotest.(check check_decode)
    "invalid distance too far back" (Error `Unexpected_end_of_output)
    (Inf.Ns.inflate src dst)

let fixed () =
  Alcotest.test_case "fixed" `Quick @@ fun () ->
  let src = bigstring_of_string "\x03\x00" in
  let res = Inf.Ns.inflate src dst in
  let expected = "" in
  Alcotest.(check check_decode)
    "fixed"
    (Ok (De.bigstring_length src, String.length expected))
    res
  ; Alcotest.(check string)
      "empty" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let stored () =
  Alcotest.test_case "stored" `Quick @@ fun () ->
  let src = bigstring_of_string "\x01\x01\x00\xfe\xff\x00" in
  let res = Inf.Ns.inflate src dst in
  let expected = "\x00" in
  Alcotest.(check check_decode)
    "stored"
    (Ok (De.bigstring_length src, String.length expected))
    res
  ; Alcotest.(check string)
      "0x00" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let length_extra () =
  Alcotest.test_case "length extra" `Quick @@ fun () ->
  let src =
    bigstring_of_string
      "\xed\xc0\x01\x01\x00\x00\x00\x40\x20\xff\x57\x1b\x42\x2c\x4f" in
  let res = Inf.Ns.inflate src dst in
  let expected = String.make 516 '\x00' in
  Alcotest.(check check_decode)
    "length extra"
    (Ok (De.bigstring_length src, String.length expected))
    res
  ; Alcotest.(check string)
      "0x00 * 516" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let long_distance_and_extra () =
  Alcotest.test_case "long distance and extra" `Quick @@ fun () ->
  let src =
    bigstring_of_string
      "\xed\xcf\xc1\xb1\x2c\x47\x10\xc4\x30\xfa\x6f\x35\x1d\x01\x82\x59\x3d\xfb\xbe\x2e\x2a\xfc\x0f\x0c"
  in
  let res = Inf.Ns.inflate src dst in
  let expected = String.make 518 '\x00' in
  Alcotest.(check check_decode)
    "long distance and extra"
    (Ok (De.bigstring_length src, String.length expected))
    res
  ; Alcotest.(check string)
      "0x00 * 518" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let window_end () =
  Alcotest.test_case "window end" `Quick @@ fun () ->
  let src =
    bigstring_of_string
      "\xed\xc0\x81\x00\x00\x00\x00\x80\xa0\xfd\xa9\x17\xa9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06"
  in
  let res = Inf.Ns.inflate src dst in
  let expected = String.make 33025 '\x00' in
  Alcotest.(check check_decode)
    "window end"
    (Ok (De.bigstring_length src, String.length expected))
    res
  ; Alcotest.(check string)
      "0x00 * 33025" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let flat_of_string () =
  Alcotest.test_case "flat of string" `Quick @@ fun () ->
  let src = bigstring_of_string "\x01\x00\x00\xff\xff" in
  let res = Inf.Ns.inflate src dst in
  let expected = "" in
  Alcotest.(check check_decode)
    "flat of string"
    (Ok (De.bigstring_length src, String.length expected))
    res
  ; Alcotest.(check string)
      "empty" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let flat_block () =
  Alcotest.test_case "flat block" `Quick @@ fun () ->
  let src = bigstring_of_string "\x01\x04\x00\xfb\xff\xde\xad\xbe\xef" in
  let res = Inf.Ns.inflate src dst in
  let expected = "\xde\xad\xbe\xef" in
  Alcotest.(check check_decode)
    "flat block"
    (Ok (De.bigstring_length src, String.length expected))
    res
  ; Alcotest.(check string)
      "deadbeef" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let huffman_length_extra () =
  Alcotest.test_case "huffman length extra" `Quick @@ fun () ->
  let literals = make_literals () in
  succ_literal literals '\000'
  ; succ_literal literals '\000'
  ; succ_length literals 258
  ; succ_length literals 256
  ; let distances = make_distances () in
    succ_distance distances 1
    ; succ_distance distances 1
    ; let dynamic = Def.dynamic_of_frequencies ~literals ~distances in
      let res =
        encode ~block:(Def.Dynamic dynamic)
          [
            `Literal '\000'; `Literal '\000'; `Copy (1, 258); `Copy (1, 256)
          ; `End
          ] in
      Alcotest.(check str)
        "encoding" res "\237\193\001\001\000\000\000@ \255W\027B\193\234\004"

      ; let src = bigstring_of_string res in
        let res = Inf.Ns.inflate src dst in
        let expected = String.make (258 + 256 + 2) '\000' in
        Alcotest.(check check_decode)
          "huffman length extra"
          (Ok (De.bigstring_length src, String.length expected))
          res
        ; Alcotest.(check str)
            "result" expected
            (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let ( <.> ) f g x = f (g x)

let dynamic_and_fixed () =
  Alcotest.test_case "dynamic+fixed" `Quick @@ fun () ->
  let res = Buffer.create 16 in
  let literals = make_literals () in
  let distances = make_distances () in
  Queue.reset q
  ; List.iter (Queue.push_exn q <.> Queue.cmd) [`Literal 'a'; `Copy (1, 3)]
  ; succ_literal literals 'a'
  ; succ_length literals 3
  ; succ_distance distances 1
  ; let dynamic_a = Def.dynamic_of_frequencies ~literals ~distances in
    let encoder = Def.encoder (`Buffer res) ~q in
    let rec go = function
      | [] -> ()
      | `Fill lst :: r ->
        Alcotest.(check bool) "empty queue" (Queue.is_empty q) true
        ; List.iter (Queue.push_exn q <.> Queue.cmd) lst
        ; go r
      | (#Def.encode as x) :: r -> (
        match Def.encode encoder x with
        | `Partial -> Alcotest.fail "Impossible `Partial case"
        | `Block -> Alcotest.fail "Impossible `Block case"
        | `Ok -> go r) in
    go
      [
        `Block {Def.kind= Def.Dynamic dynamic_a; Def.last= false}; `Flush
      ; `Fill [`Literal 'b'; `Copy (1, 3); `End]
      ; `Block {Def.kind= Def.Fixed; Def.last= true}; `Flush
      ]
    ; let src = bigstring_of_string (Buffer.contents res) in
      let res = Inf.Ns.inflate src dst in
      let expected = "aaaabbbb" in
      Alcotest.(check check_decode)
        "dynamic+fixed"
        (Ok (De.bigstring_length src, String.length expected))
        res
      ; Alcotest.(check str)
          "result" expected
          (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let fixed_and_dynamic () =
  Alcotest.test_case "fixed+dynamic" `Quick @@ fun () ->
  let res = Buffer.create 16 in
  let literals = make_literals () in
  let distances = make_distances () in
  Queue.reset q
  ; List.iter (Queue.push_exn q <.> Queue.cmd) [`Literal 'a'; `Copy (1, 3)]
  ; succ_literal literals 'b'
  ; succ_length literals 3
  ; succ_distance distances 1
  ; let dynamic_b = Def.dynamic_of_frequencies ~literals ~distances in
    let encoder = Def.encoder (`Buffer res) ~q in
    let rec go = function
      | [] -> ()
      | `Fill lst :: r ->
        Alcotest.(check bool) "empty queue" (Queue.is_empty q) true
        ; List.iter (Queue.push_exn q <.> Queue.cmd) lst
        ; go r
      | (#Def.encode as x) :: r -> (
        match Def.encode encoder x with
        | `Partial -> Alcotest.fail "Impossible `Partial case"
        | `Block -> Alcotest.fail "Impossible `Block case"
        | `Ok -> go r) in
    go
      [
        `Flush; `Block {Def.kind= Def.Dynamic dynamic_b; last= true}
      ; `Fill [`Literal 'b'; `Copy (1, 3); `End]; `Flush
      ]
    ; Fmt.epr "> %S.\n%!" (Buffer.contents res)
    ; let src = bigstring_of_string (Buffer.contents res) in
      let res = Inf.Ns.inflate src dst in
      let expected = "aaaabbbb" in
      Alcotest.(check check_decode)
        "fixed+dynamic"
        (Ok (De.bigstring_length src, String.length expected))
        res
      ; Alcotest.(check str)
          "result" expected
          (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let dynamic_and_dynamic () =
  Alcotest.test_case "dynamic+dynamic" `Quick @@ fun () ->
  let res = Buffer.create 16 in
  let literals = make_literals () in
  let distances = make_distances () in
  Queue.reset q
  ; List.iter
      (Queue.push_exn q <.> Queue.cmd)
      [`Literal 'a'; `Copy (1, 3); `Literal 'b'; `Copy (1, 3); `End]

  ; succ_literal literals 'a'
  ; succ_length literals 3
  ; succ_distance distances 1
  ; let dynamic_a = Def.dynamic_of_frequencies ~literals ~distances in
    succ_literal literals 'b'
    ; succ_length literals 3
    ; succ_distance distances 1
    ; let dynamic_b = Def.dynamic_of_frequencies ~literals ~distances in

      let encoder = Def.encoder (`Buffer res) ~q in
      let rec go = function
        | [] -> ()
        | x :: `Block block :: r -> (
          match Def.encode encoder x with
          | `Partial -> Alcotest.fail "Impossible `Partial case"
          | `Block -> go (`Block block :: r)
          | `Ok -> Alcotest.fail "Unexpected `Ok case")
        | x :: r -> (
          match Def.encode encoder x with
          | `Ok -> go r
          | `Partial -> Alcotest.fail "Impossible `Partial case"
          | `Block -> Alcotest.fail "Impossible `Block case") in
      go
        [
          `Block {Def.kind= Def.Dynamic dynamic_a; Def.last= false}
        ; `Block {Def.kind= Def.Dynamic dynamic_b; Def.last= true}; `Flush
        ]

      ; Fmt.epr "> %S.\n%!" (Buffer.contents res)
      ; let src = bigstring_of_string (Buffer.contents res) in
        let res = Inf.Ns.inflate src dst in
        let expected = "aaaabbbb" in
        Alcotest.(check check_decode)
          "dynamic+dynamic"
          (Ok (De.bigstring_length src, String.length expected))
          res
        ; Alcotest.(check str)
            "result" expected
            (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let max_flat () =
  Alcotest.test_case "biggest flat block" `Quick @@ fun () ->
  let inputs = Bytes.make (0xFFFF + 1 + 4) '\x00' in
  Bytes.set inputs 0 '\x01'
  ; (* last *)
    Bytes.set inputs 1 '\xff'
  ; Bytes.set inputs 2 '\xff'
  ; (* len *)
    let src = bigstring_of_string (Bytes.unsafe_to_string inputs) in
    let res = Inf.Ns.inflate src dst in
    let expected = String.make 0xffff '\x00' in
    Alcotest.(check check_decode)
      "biggest flat block"
      (Ok (De.bigstring_length src, String.length expected))
      res
    ; Alcotest.(check string)
        "0xffff * \x00" expected
        (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let flat () =
  Alcotest.test_case "encode flat" `Quick @@ fun () ->
  let q =
    Queue.of_list
      [`Literal '\xDE'; `Literal '\xAD'; `Literal '\xBE'; `Literal '\xEF'] in
  let b = Buffer.create 16 in
  let encoder = Def.encoder (`Buffer b) ~q in

  let go = function
    | `Ok -> Buffer.contents b
    | `Partial | `Block -> assert false in
  let res0 = go (Def.encode encoder (`Block {Def.kind= Def.Flat; last= true})) in
  Alcotest.(check string)
    "deadbeef deflated" "\x01\x04\x00\xfb\xff\xde\xad\xbe\xef" res0
  ; let src = bigstring_of_string res0 in
    let res = Inf.Ns.inflate src dst in
    let expected = "\xde\xad\xbe\xef" in
    Alcotest.(check check_decode)
      "encode flat"
      (Ok (De.bigstring_length src, String.length expected))
      res
    ; Alcotest.(check string)
        "deadbeef" expected
        (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let fixed_and_flat () =
  Alcotest.test_case "fixed+flat" `Quick @@ fun () ->
  let q =
    Queue.of_list
      [
        `Literal 'a'; `Copy (1, 3); `End; `Literal '\xDE'; `Literal '\xAD'
      ; `Literal '\xBE'; `Literal '\xEF'
      ] in
  let b = Buffer.create 16 in
  let encoder = Def.encoder (`Buffer b) ~q in

  let rec go = function
    | `Ok -> Buffer.contents b
    | `Partial -> assert false
    | `Block ->
      go (Def.encode encoder (`Block {Def.kind= Def.Flat; last= true})) in
  let res0 = go (Def.encode encoder `Flush) in
  let src = bigstring_of_string res0 in
  let res = Inf.Ns.inflate src dst in
  let expected = "aaaa\xde\xad\xbe\xef" in
  Alcotest.(check check_decode)
    "fixed+flat"
    (Ok (De.bigstring_length src, String.length expected))
    res
  ; Alcotest.(check string)
      "aaaadeadbeef" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let flat_and_fixed () =
  Alcotest.test_case "flat+fixed" `Quick @@ fun () ->
  let q =
    Queue.of_list
      [
        `Literal '\xDE'; `Literal '\xAD'; `Literal '\xBE'; `Literal '\xEF'
      ; `Literal 'a'
      ] in
  let b = Buffer.create 16 in
  let encoder = Def.encoder (`Buffer b) ~q in

  let rec go0 = function
    | `Ok ->
      Queue.push_exn q (Queue.cmd (`Copy (1, 3)))
      ; Queue.push_exn q Queue.eob
      ; go0 (Def.encode encoder `Flush)
    | `Partial -> assert false
    | `Block ->
      go1 (Def.encode encoder (`Block {Def.kind= Def.Fixed; last= true}))
  and go1 = function
    | `Partial | `Block -> assert false
    | `Ok -> Buffer.contents b in
  let res0 =
    go0 (Def.encode encoder (`Block {Def.kind= Def.Flat; last= false})) in

  let src = bigstring_of_string res0 in
  let res = Inf.Ns.inflate src dst in
  let expected = "\xde\xad\xbe\xefaaaa" in
  Alcotest.(check check_decode)
    "flat+fixed"
    (Ok (De.bigstring_length src, String.length expected))
    res
  ; Alcotest.(check string)
      "deadbeefaaaa" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let fuzz0 () =
  Alcotest.test_case "fuzz0" `Quick @@ fun () ->
  let src =
    bigstring_of_string
      "{\220\n\
      \ \
       s\017\027\211\\\006\211w\176`\142\2007\156oZBo\163\136\017\247\158\247\012e\241\234sn_$\210\223\017\213\138\147]\129M\137<\242\1867\021c\194\156\135\194\167-wo\006\200\198"
  in
  let res = Inf.Ns.inflate src dst in
  let expected = "\xe3\x85" in
  Alcotest.(check check_decode) "fuzz0" (Ok (4, String.length expected)) res
  ; Alcotest.(check string)
      "0x00 * 33025" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let fuzz1 () =
  Alcotest.test_case "fuzz1" `Quick @@ fun () ->
  let src = bigstring_of_string "\019\208nO\200\189r\020\176" in
  let res = Inf.Ns.inflate src dst in
  let expected = "\016+\135`m\212\197" in
  Alcotest.(check check_decode)
    "fuzz1"
    (Ok (De.bigstring_length src, String.length expected))
    res
  ; Alcotest.(check string)
      "fuzz1" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let fuzz2 () =
  Alcotest.test_case "fuzz2" `Quick @@ fun () ->
  let src =
    bigstring_of_string "\x93\x3a\x55\x47\x12\x80\x51\x56\x3a\x01\x00\x00" in
  let res = Inf.Ns.inflate src dst in
  let expected =
    [
      "\x1a\xca\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e"
      (* ..~~~~~~~~~~~~~~ *)
    ; "\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e"
      (* ~~~~~~~~~~~~~~~~ *)
    ; "\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e"
      (* ~~~~~~~~~~~~~~~~ *)
    ; "\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x7e\x3a\x2c\x50" (* ~~~~~~~~:,P *)
    ] in
  let expected = String.concat "" expected in
  (* All of src is not used (last byte is useless) *)
  Alcotest.(check check_decode)
    "fuzz2"
    (Ok (De.bigstring_length src - 1, String.length expected))
    res
  ; Alcotest.(check string)
      "fuzz2" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let fuzz3 () =
  Alcotest.test_case "fuzz3" `Quick @@ fun () ->
  let src =
    bigstring_of_string "\x93\x3a\x55\x47\x12\x3a\x51\x36\x0a\x01\x00\x00" in
  let res = Inf.Ns.inflate src dst in
  let expected =
    [
      "\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a"
      (* ..~..~..~..~..~. *)
    ; "\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca"
      (* .~..~..~..~..~.. *)
    ; "\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e"
      (* ~..~..~..~..~..~ *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76"
      (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76"
      (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76"
      (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76"
      (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76"
      (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76"
      (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76"
      (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76"
      (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76"
      (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76"
      (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76"
      (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76"
      (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76"
      (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76"
      (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76"
      (* .v.v.v.v.v.v.v.v *)
    ; "\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76\xc8\x76"
      (* .v.v.v.v.v.v.v.v *); "\xc8\x76\xc8\x76" (* .v.v *)
    ] in
  let expected = String.concat "" expected in
  (* All of src is not used (last byte is useless) *)
  Alcotest.(check check_decode)
    "fuzz3"
    (Ok (De.bigstring_length src - 2, String.length expected))
    res
  ; Alcotest.(check string)
      "fuzz3" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let fuzz4 () =
  Alcotest.test_case "fuzz4" `Quick @@ fun () ->
  let src =
    bigstring_of_string "\x93\x3a\x55\x47\x12\x3a\x51\x56\x0a\x06\x80\x00" in
  let res = Inf.Ns.inflate src dst in
  let expected =
    [
      "\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a"
      (* ..~..~..~..~..~. *)
    ; "\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca"
      (* .~..~..~..~..~.. *)
    ; "\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e\x1a\xca\x7e"
      (* ~..~..~..~..~..~ *); "\xc8\x76\x75\x75\x75\x75\x75\x75" (* .vuuuuuu *)
    ] in
  let expected = String.concat "" expected in
  (* All of src is not used (last byte is useless) *)
  Alcotest.(check check_decode)
    "fuzz4"
    (Ok (De.bigstring_length src - 1, String.length expected))
    res
  ; Alcotest.(check string)
      "fuzz4" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let fuzz5 () =
  Alcotest.test_case "fuzz5" `Quick @@ fun () ->
  let src =
    [
      "\x93\x3a\x55\x01\x01\x01\x01\xe6\x01\x01\x01\x01\x01\x01\x01\x01"
      (* .:U............. *); "\x01\x01\x01\x01\x01\x00\x00" (* ....... *)
    ] in
  let src = bigstring_of_string (String.concat "" src) in
  let res = Inf.Ns.inflate src dst in
  let expected =
    [
      "\x1a\xca\x78\x78\x78\x78\x78\x78\x78\x50\x50\x37\x50\x50\x50\x50"
      (* ..xxxxxxxPP7PPPP *)
    ; "\x50\x50\x50\x50\x50\x50\x50\x50\x50" (* PPPPPPPPP *)
    ] in
  let expected = String.concat "" expected in
  (* All of src is not used (last byte is useless) *)
  Alcotest.(check check_decode)
    "fuzz5"
    (Ok (De.bigstring_length src - 1, String.length expected))
    res
  ; Alcotest.(check string)
      "fuzz5" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let fuzz6 () =
  Alcotest.test_case "fuzz6" `Quick @@ fun () ->
  let src =
    bigstring_of_string "\x93\x8c\x8c\x8c\x8c\x7b\x8c\x8c\x8c\x01\x00\x00" in
  let res = Inf.Ns.inflate src dst in
  let expected =
    [
      "\x19\x59\x59\x59\x5e\xe3\x59\x5e\xe3\x59\x5e\xe3\x59\x5e\xe3\x59"
      (* .YYY^.Y^.Y^.Y^.Y *); "\x5e\xe3\x33" (* ^.3 *)
    ] in
  let expected = String.concat "" expected in
  (* All of src is not used (last 2 bytes are useless) *)
  Alcotest.(check check_decode)
    "fuzz6"
    (Ok (De.bigstring_length src - 2, String.length expected))
    res
  ; Alcotest.(check string)
      "fuzz6" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let fuzz7 () =
  Alcotest.test_case "fuzz7" `Quick @@ fun () ->
  let src =
    bigstring_of_string "\x93\x3a\x55\x69\x12\x3a\x3f\x10\x08\x01\x00\x00" in
  let res = Inf.Ns.inflate src dst in
  let expected = "\x1a\xca\x79\x34\x55\x9f\x51\x9f\x51\x9f" in
  (* All of src is not used (last 2 bytes is useless) *)
  Alcotest.(check check_decode)
    "fuzz7"
    (Ok (De.bigstring_length src - 2, String.length expected))
    res
  ; Alcotest.(check string)
      "fuzz7" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let fuzz8 () =
  Alcotest.test_case "fuzz8" `Quick @@ fun () ->
  let src = bigstring_of_string "\x7a\x37\x6d\x99\x13" in
  Alcotest.(check check_decode)
    "fuzz8" (Error `Unexpected_end_of_input) (Inf.Ns.inflate src dst)

let fuzz9 () =
  Alcotest.test_case "fuzz9" `Quick @@ fun () ->
  let src =
    [
      "\x9b\x01\x95\xfc\x51\xd2\xed\xc8\xce\xc8\xff\x80\x00\x00\x7f\xff"
      (* ....Q........... *)
    ; "\x79\x2f\xe9\x51\x88\x7b\xb8\x2f\xef\xa5\x8c\xf8\xf1\xb6\xce\xc8"
      (* y/.Q.{./........ *)
    ; "\xb8\xc8\xff\x2f\x00\x7f\x88\x7b\xbc" (* .../...{. *)
    ] in
  let src = bigstring_of_string (String.concat "" src) in
  Alcotest.(check check_decode)
    "fuzz9" (Error `Invalid_distance) (Inf.Ns.inflate src dst)

let fuzz10 () =
  Alcotest.test_case "fuzz10" `Quick @@ fun () ->
  let lst =
    [
      `Literal (Char.chr 231); `Literal (Char.chr 60); `Literal (Char.chr 128)
    ; `Copy (1, 19); `End
    ] in
  let src = bigstring_of_string (encode_dynamic lst) in
  let res = Inf.Ns.inflate src dst in
  Alcotest.(check check_decode) "fuzz10" (Ok (De.bigstring_length src, 22)) res

let fuzz11 () =
  Alcotest.test_case "fuzz11" `Quick @@ fun () ->
  let lst =
    [`Literal (Char.chr 228); `Literal (Char.chr 255); `Copy (1, 130); `End]
  in
  let src = bigstring_of_string (encode_dynamic lst) in
  let res = Inf.Ns.inflate src dst in
  let expected = "\228" ^ String.make 131 '\xff' in
  Alcotest.(check check_decode)
    "fuzz11"
    (Ok (De.bigstring_length src, String.length expected))
    res
  ; Alcotest.(check string)
      "fuzz11" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let fuzz12 () =
  Alcotest.test_case "fuzz12" `Quick @@ fun () ->
  let lst =
    [
      `Literal (Char.chr 71); `Literal (Char.chr 0); `Literal (Char.chr 255)
    ; `Copy (2, 249); `End
    ] in
  let src = bigstring_of_string (encode_dynamic lst) in
  let res = Inf.Ns.inflate src dst in
  let expected =
    [
      "\x47\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00"
      (* G............... *)
    ; "\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00"
      (* ................ *)
    ; "\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00"
      (* ................ *)
    ; "\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00"
      (* ................ *)
    ; "\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00"
      (* ................ *)
    ; "\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00"
      (* ................ *)
    ; "\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00"
      (* ................ *)
    ; "\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00"
      (* ................ *)
    ; "\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00"
      (* ................ *)
    ; "\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00"
      (* ................ *)
    ; "\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00"
      (* ................ *)
    ; "\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00"
      (* ................ *)
    ; "\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00"
      (* ................ *)
    ; "\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00"
      (* ................ *)
    ; "\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00"
      (* ................ *)
    ; "\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00\xff\x00" (* ............ *)
    ] in
  let expected = String.concat "" expected in
  Alcotest.(check check_decode)
    "fuzz12"
    (Ok (De.bigstring_length src, String.length expected))
    res
  ; Alcotest.(check string)
      "fuzz12" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let fuzz13 () =
  Alcotest.test_case "fuzz13" `Quick @@ fun () ->
  let src = ["\x9b\x0e\x02\x00" (* .... *)] in
  let src = bigstring_of_string (String.concat "" src) in
  let res = Inf.Ns.inflate src dst in
  let expected = "\x97\x97\x97\x97\x97" in
  Alcotest.(check check_decode)
    "fuzz13"
    (Ok (De.bigstring_length src, String.length expected))
    res
  ; Alcotest.(check string)
      "fuzz13" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let fuzz14 () =
  Alcotest.test_case "fuzz14" `Quick @@ fun () ->
  let src =
    [
      "\x0b\xff\x7f\x0c\x0c\x8f\xcd\x0e\x02\x21\x64\x0c\x04\x73\xff\x80"
      (* .........!d..s.. *)
    ; "\x20\x0c\x8f\x1c\x1c\x1c\x1c\x0c\x0c\x0c\x0c\x64\x1c\x7f\x0c\x0c"
      (*  ..........d.... *); "\x8f\xcd\x0e\x02\x21\xff\xff\x80" (* ....!... *)
    ] in
  let src = bigstring_of_string (String.concat "" src) in
  let res = Inf.Ns.inflate src dst in
  let expected =
    [
      "\x57\xff\xc6\xff\xc6\xff\xc6\xff\xc6\x9b\x52\xc6\x9b\x52\xc6\xc6"
      (* W.........R..R.. *)
    ; "\x9b\x52\xc6\xc6\x9b\x52\xc6\xc6\x9b\x52\xc6\xc6\xc6\xc6\x9d\xfc"
      (* .R...R...R...... *)
    ; "\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc"
      (* ................ *)
    ; "\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc"
      (* ................ *)
    ; "\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc"
      (* ................ *)
    ; "\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc"
      (* ................ *)
    ; "\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc"
      (* ................ *)
    ; "\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc"
      (* ................ *)
    ; "\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc"
      (* ................ *)
    ; "\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc"
      (* ................ *)
    ; "\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc"
      (* ................ *)
    ; "\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc"
      (* ................ *)
    ; "\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc"
      (* ................ *)
    ; "\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc"
      (* ................ *)
    ; "\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc"
      (* ................ *)
    ; "\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc"
      (* ................ *)
    ; "\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc\x9d\xfc"
      (* ................ *)
    ; "\x9d\xfc\x9d\xfc\x9d\xfc\x53\x53\x53\x9b\x52\xc6\x9b\x52\xc6\x9b"
      (* ......SSS.R..R.. *)
    ; "\x52\xc6\x9b\x52\xc6\x9b\x52\xc6\x9b\x52\xc6\x9b\x52\x33\x5f\xc6"
      (* R..R..R..R..R3_. *)
    ; "\x5f\xc6\x5f\xc6\x5f\xc6\x9b\x52\xc6\x9b\x52\xc6\x4f\xff"
      (* _._._..R..R.O. *)
    ] in
  let expected = String.concat "" expected in
  Alcotest.(check check_decode)
    "fuzz14"
    (Ok (De.bigstring_length src, String.length expected))
    res
  ; Alcotest.(check string)
      "fuzz14" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let fuzz15 () =
  Alcotest.test_case "fuzz15" `Quick @@ fun () ->
  let src =
    [
      "\x75\x85\xcd\x0e\x02\x21\x0c\x84\x3d\xf3\x14\x3d\xc2\x65\x63\xb2"
      (* u....!..=..=.ec. *)
    ; "\x0f\x64\xf8\x69\xdc\xc6\xc2\x12\x58\x12\xe4\xe9\x5d\xa3\x28\x26"
      (* .d.i....X...].(& *)
    ; "\xee\xad\xc2\x65\x63\xb2\x0f\x64\xf8\x69\xdc\xc6\xc2\x12\x58\x12"
      (* ...ec..d.i....X. *)
    ; "\xe4\xe9\x5d\x66\xfb\xe8\x57\x57\x18\xf3\x5b\xdd\xcb\x73"
      (* ..]f..WW..[..s *)
    ] in
  let src = bigstring_of_string (String.concat "" src) in
  let res = Inf.Ns.inflate src dst in
  let expected = ["\x78\x20\x5f\x74\x6c\x69\x63" (* x _tlic *)] in
  let expected = String.concat "" expected in
  (* All of src is not used (last byte is useless) *)
  Alcotest.(check check_decode) "fuzz15" (Ok (39, String.length expected)) res
  ; Alcotest.(check string)
      "fuzz15" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let fuzz16 () =
  Alcotest.test_case "fuzz16" `Quick @@ fun () ->
  let lst =
    [
      `Literal '@'; `Copy (1, 212); `Copy (129, 258); `Copy (7, 131)
    ; `Copy (527, 208); `Copy (129, 258); `End
    ] in
  let src = bigstring_of_string (encode_dynamic lst) in
  let res = Inf.Ns.inflate src dst in
  let expected = String.make 1068 '@' in
  Alcotest.(check check_decode)
    "fuzz16"
    (Ok (De.bigstring_length src, String.length expected))
    res
  ; Alcotest.(check string)
      "fuzz16" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let fuzz17 () =
  Alcotest.test_case "fuzz17" `Quick @@ fun () ->
  let lst =
    [
      `Literal (Char.chr 218); `Copy (1, 21); `Literal (Char.chr 190)
    ; `Literal (Char.chr 218); `Literal (Char.chr 0); `End
    ] in
  let src = bigstring_of_string (encode_dynamic lst) in
  let res = Inf.Ns.inflate src dst in
  let expected =
    [
      "\xda\xda\xda\xda\xda\xda\xda\xda\xda\xda\xda\xda\xda\xda\xda\xda"
      (* ................ *)
    ; "\xda\xda\xda\xda\xda\xda\xbe\xda\x00" (* ......... *)
    ] in
  let expected = String.concat "" expected in
  Alcotest.(check check_decode)
    "fuzz17"
    (Ok (De.bigstring_length src, String.length expected))
    res
  ; Alcotest.(check string)
      "fuzz17" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let fuzz18 () =
  Alcotest.test_case "fuzz18" `Quick @@ fun () ->
  let src =
    [
      "\x75\x8f\xcd\x0e\x02\x21\x0c\x84\x3d\xf3\x14\x3d\xfc\x54\x63\xb2"
      (* u....!..=..=.Tc. *)
    ; "\x0f\x64\xf8\x69\xdc\xc6\xc2\x12\x58\x12\xe4\xe9\x5d\xa3\x28\x26"
      (* .d.i....X...].(& *)
    ; "\xee\xad\x33\xcd\xfc\x9d\x1a\x5e\x1e\xcc\xe7\xf9\x24\x99\x40\x06"
      (* ..3....^....$.@. *)
    ; "\xed\x11\x4c\x56\xfb\xe8\x57\x57\x0a\xf3\x5b\xd9\xcb\x60\xd5\xd5"
      (* ..LV..WW..[..`.. *)
    ] in
  let src = bigstring_of_string (String.concat "" src) in
  let res = Inf.Ns.inflate src dst in
  let expected =
    [
      "\x75\x27\x5a\xfb\x64\x64\x2b\x63\x29\x67\x6e\x60\x20\x67\x6e\x60"
      (* u'dd+c)gn` gn` *)
    ; "\x20\x67\x6e\x60\x5e\x28\x20\x5d\x6e\x0a\x63\x29\x67\x6e\x60\x20"
      (*  gn`^( ]n.c)gn`  *)
    ; "\x67\x6e\x60\x20\x67\x6e\x63\x29\x67\x6e\x60\x20\x67\x73\x60\x69"
      (* gn` gnc)gn` gs`i *); "\x63" (* c *)
    ] in
  let expected = String.concat "" expected in
  (* All of src is not used (last byte is useless) *)
  Alcotest.(check check_decode) "fuzz18" (Ok (59, String.length expected)) res
  ; Alcotest.(check string)
      "fuzz18" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_decode_o res))

let w0 = make_window ~bits:15
let w1 = make_window ~bits:15
let i = bigstring_create io_buffer_size
let o = bigstring_create io_buffer_size
let q = Queue.create (2 * 2 * 4096)
let b = Buffer.create 4096

let compress_and_uncompress ic =
  Buffer.clear b
  ; Queue.reset q
  ; let in_len = in_channel_length ic in
    let src_def = bigstring_create in_len in
    for i = 0 to in_len - 1 do
      let v = Char.code (input_char ic) in
      unsafe_set_uint8 src_def i v
    done
    ; let dst_def = bigstring_create (Def.Ns.compress_bound in_len) in
      match Def.Ns.deflate src_def dst_def with
      | Ok len -> (
        let dst_inf = bigstring_create in_len in
        match Inf.Ns.inflate dst_def dst_inf with
        | Ok (i_len, o_len) ->
          Alcotest.(check int) "inflate same len" len i_len
          ; Alcotest.(check int) "keep good length" in_len o_len
          ; Stdlib.seek_in ic 0
          ; Buffer.clear b
          ; for i = 0 to o_len - 1 do
              Buffer.add_char b (Char.unsafe_chr (unsafe_get_uint8 dst_inf i))
            done
          ; let contents = Buffer.contents b in
            let rec slow_compare pos =
              match input_char ic with
              | chr ->
                if pos >= String.length contents then
                  Fmt.invalid_arg "Reach end of contents"
                ; if contents.[pos] <> chr then
                    Fmt.invalid_arg "Contents differ at %08x\n%!" pos
                ; slow_compare (succ pos)
              | exception End_of_file ->
                if pos <> String.length contents then
                  Fmt.invalid_arg "Lengths differ: (contents: %d, file: %d)"
                    (String.length contents) pos in
            slow_compare 0
        | Error err ->
          Alcotest.failf "Error when inflating: %a" Inf.Ns.pp_error err)
      | Error err ->
        Alcotest.failf "Error when inflating: %a" Def.Ns.pp_error err

let zlib_compress_and_uncompress ic =
  Buffer.clear b
  ; Queue.reset q
  ; let in_len = in_channel_length ic in
    let src_def = bigstring_create in_len in
    for i = 0 to in_len - 1 do
      let v = Char.code (input_char ic) in
      unsafe_set_uint8 src_def i v
    done
    ; let dst_def = bigstring_create (Zl.Def.Ns.compress_bound in_len) in
      match Zl.Def.Ns.deflate src_def dst_def with
      | Ok len -> (
        let dst_inf = bigstring_create in_len in
        match Zl.Inf.Ns.inflate dst_def dst_inf with
        | Ok (i_len, o_len) ->
          Alcotest.(check int) "inflate same len" len i_len
          ; Alcotest.(check int) "keep good length" in_len o_len
          ; Stdlib.seek_in ic 0
          ; Buffer.clear b
          ; for i = 0 to o_len - 1 do
              Buffer.add_char b (Char.unsafe_chr (unsafe_get_uint8 dst_inf i))
            done
          ; let contents = Buffer.contents b in
            let rec slow_compare pos =
              match input_char ic with
              | chr ->
                if pos >= String.length contents then
                  Fmt.invalid_arg "Reach end of contents"
                ; if contents.[pos] <> chr then
                    Fmt.invalid_arg "Contents differ at %08x\n%!" pos
                ; slow_compare (succ pos)
              | exception End_of_file ->
                if pos <> String.length contents then
                  Fmt.invalid_arg "Lengths differ: (contents: %d, file: %d)"
                    (String.length contents) pos in
            slow_compare 0
        | Error err ->
          Alcotest.failf "Error when inflating: %a" Zl.Inf.Ns.pp_error err)
      | Error err ->
        Alcotest.failf "Error when inflating: %a" Def.Ns.pp_error err

let test_corpus filename =
  Alcotest.test_case filename `Slow @@ fun () ->
  let ic = open_in Filename.(concat "corpus" filename) in
  compress_and_uncompress ic ; close_in ic

let test_corpus_with_zlib filename =
  Alcotest.test_case filename `Slow @@ fun () ->
  let ic = open_in Filename.(concat "corpus" filename) in
  zlib_compress_and_uncompress ic
  ; close_in ic

let encoder_0 () =
  Alcotest.test_case "encoder 0" `Quick @@ fun () ->
  let src = bigstring_of_string "\x01" in
  let res = Def.Ns.deflate ~level:0 src dst in
  let expected = "\x01\x01\x00\xfe\xff\x01" in
  Alcotest.(check (result int Alcotest.reject))
    "encoder 0"
    (Ok (String.length expected))
    res
  ; Alcotest.(check string)
      "0x00" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_encode res))

let encoder_1 () =
  Alcotest.test_case "encoder 1" `Quick @@ fun () ->
  let src =
    [
      "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
      (* ................ *)
    ; "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
      (* ................ *)
    ; "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
      (* ................ *); "\x00\x00\x00\x00\x00\x00\x00\x00" (* ........ *)
    ] in
  let src = bigstring_of_string (String.concat "" src) in
  let res = Def.Ns.deflate ~level:1 src dst in
  let expected = "\x63\x20\x13\x00\x00" in
  Alcotest.(check (result int Alcotest.reject))
    "encoder 1"
    (Ok (String.length expected))
    res
  ; Alcotest.(check string)
      "0x00" expected
      (Bstr.sub_string dst ~off:0 ~len:(check_encode res))

let tests =
  [
    "ns_encoder", [encoder_0 (); encoder_1 ()]
  ; ( "ns_invalids"
    , [
        invalid_complement_of_length (); invalid_kind_of_block ()
      ; invalid_code_lengths (); invalid_bit_length_repeat (); invalid_codes ()
      ; invalid_lengths (); invalid_distances ()
      ; too_many_length_or_distance_symbols (); invalid_distance_code ()
      ; invalid_distance_too_far_back (); invalid_flat_not_enough_output ()
      ; invalid_literal_not_enough_output (); invalid_copy_not_enough_output ()
      ] )
  ; ( "ns_valids"
    , [
        fixed (); stored (); length_extra (); long_distance_and_extra ()
      ; window_end (); huffman_length_extra (); dynamic_and_fixed ()
      ; fixed_and_dynamic (); dynamic_and_dynamic (); flat_of_string ()
      ; flat_block (); flat (); max_flat (); fixed_and_flat ()
      ; flat_and_fixed ()
      ] )
  ; ( "ns_fuzz"
    , [
        fuzz0 (); fuzz1 (); fuzz2 (); fuzz3 (); fuzz4 (); fuzz5 (); fuzz6 ()
      ; fuzz7 (); fuzz8 (); fuzz9 (); fuzz10 (); fuzz11 (); fuzz12 (); fuzz13 ()
      ; fuzz14 (); fuzz15 (); fuzz16 (); fuzz17 (); fuzz18 ()
      ] )
  ; ( "ns_calgary"
    , [
        test_corpus "bib"; test_corpus "rfc5322.txt"; test_corpus "book1"
      ; test_corpus "book2"; test_corpus "geo"; test_corpus "news"
      ; test_corpus "obj1"; test_corpus "obj2"; test_corpus "paper1"
      ; test_corpus "paper2"; test_corpus "pic"; test_corpus "progc"
      ; test_corpus "progl"; test_corpus "progp"; test_corpus "trans"
      ] )
  ; ( "ns_zlib"
    , [
        test_corpus_with_zlib "bib"; test_corpus_with_zlib "book1"
      ; test_corpus_with_zlib "book2"; test_corpus_with_zlib "geo"
      ; test_corpus_with_zlib "news"; test_corpus_with_zlib "obj1"
      ; test_corpus_with_zlib "obj2"; test_corpus_with_zlib "paper1"
      ; test_corpus_with_zlib "paper2"; test_corpus_with_zlib "pic"
      ; test_corpus_with_zlib "progc"; test_corpus_with_zlib "progl"
      ; test_corpus_with_zlib "progp"; test_corpus_with_zlib "trans"
      ] )
  ]
