open De

let q = Queue.create 0x4000
let o = bigstring_create io_buffer_size
let t = bigstring_create io_buffer_size
let w0 = Lz77.make_window ~bits:15
let w1 = make_window ~bits:15

let load_file filename =
  let ic = open_in filename in
  let ln = in_channel_length ic in
  let rs = really_input_string ic ln in
  close_in ic ; rs

let compare_files a b =
  let x = load_file a and y = load_file b in
  Alcotest.(check string) (Fmt.str "%s:%s" a b) x y

let deflate_with_level ~level filename =
  Alcotest.test_case (Fmt.str "%s (level: %d)" filename level) `Quick
  @@ fun () ->
  let ic = open_in filename in
  let oc = open_out (filename ^ ".o") in
  let state = Lz77.state ~level ~q ~w:w0 (`Channel ic) in
  let encoder = Def.encoder `Manual ~q in
  let decoder = Inf.decoder `Manual ~o ~w:w1 in
  let rec compress () =
    match De.Lz77.compress state with
    | `Await -> assert false
    | `Flush ->
      let literals = Lz77.literals state in
      let distances = Lz77.distances state in
      Fmt.epr "[compress]: `Flush.\n%!"
      ; encode
        @@ Def.encode encoder
             (`Block
                {
                  Def.kind=
                    Dynamic (Def.dynamic_of_frequencies ~literals ~distances)
                ; last= false
                })
    | `End ->
      Fmt.epr "[compress]: `End.\n%!"
      ; close_in ic
      ; encode_rest @@ Def.encode encoder (`Block {Def.kind= Fixed; last= true})
  and encode_rest = function
    | (`Partial | `Ok) as res ->
      let len = bigstring_length t - Def.dst_rem encoder in
      Fmt.epr "[pending]: `Partial (%d byte(s)).\n%!" len
      ; Inf.src decoder t 0 len
      ; decode_rest (res = `Ok) @@ Inf.decode decoder
    | `Block -> assert false
  and encode = function
    | `Partial ->
      let len = bigstring_length t - Def.dst_rem encoder in
      Fmt.epr "[encode]: `Partial (%d byte(s)).\n%!" len
      ; Inf.src decoder t 0 len
      ; decode @@ Inf.decode decoder
    | `Ok ->
      Fmt.epr "[encode] `Ok.\n%!"
      ; compress ()
    | `Block ->
      Fmt.epr "[encode] `Ok.\n%!"
      ; compress ()
  and decode_rest finalize = function
    | `Await when finalize ->
      Inf.src decoder t 0 0
      ; decode_rest finalize @@ Inf.decode decoder
    | `Await ->
      Def.dst encoder t 0 (bigstring_length t)
      ; encode_rest @@ Def.encode encoder `Await
    | (`Flush | `End) as state ->
      let len = bigstring_length o - Inf.dst_rem decoder in
      let str = Bstr.sub_string o ~off:0 ~len in
      output_string oc str
      ; if state = `Flush then (
          Inf.flush decoder
          ; decode_rest finalize @@ Inf.decode decoder)
        else close_out oc
    | `Malformed err -> Alcotest.failf "Malformed compressed input: %S" err
  and decode = function
    | `Await ->
      Def.dst encoder t 0 (bigstring_length t)
      ; encode @@ Def.encode encoder `Await
    | (`Flush | `End) as state ->
      let len = bigstring_length o - Inf.dst_rem decoder in
      let str = Bstr.sub_string o ~off:0 ~len in
      output_string oc str
      ; if state = `Flush then (
          Inf.flush decoder
          ; decode @@ Inf.decode decoder)
        else close_out oc
    | `Malformed err -> Alcotest.failf "Malformed compressed input: %S" err
  in
  Def.dst encoder t 0 (bigstring_length t)
  ; Queue.reset q
  ; compress ()
  ; compare_files filename (filename ^ ".o")

let corpus =
  [
    "corpus/bib"; "corpus/book1"; "corpus/book2"; "corpus/geo"; "corpus/news"
  ; "corpus/obj1"; "corpus/obj2"; "corpus/paper1"; "corpus/paper2"; "corpus/pic"
  ; "corpus/progc"; "corpus/progl"; "corpus/progp"; "corpus/trans"
  ]

let () =
  Alcotest.run "lz"
    [
      "0", List.map (deflate_with_level ~level:0) corpus
    ; "1", List.map (deflate_with_level ~level:1) corpus
    ; "2", List.map (deflate_with_level ~level:2) corpus
    ; "3", List.map (deflate_with_level ~level:3) corpus
    ; "4", List.map (deflate_with_level ~level:4) corpus
    ; "5", List.map (deflate_with_level ~level:5) corpus
    ; "6", List.map (deflate_with_level ~level:6) corpus
    ; "7", List.map (deflate_with_level ~level:7) corpus
    ; "8", List.map (deflate_with_level ~level:8) corpus
    ; "9", List.map (deflate_with_level ~level:9) corpus
    ]
