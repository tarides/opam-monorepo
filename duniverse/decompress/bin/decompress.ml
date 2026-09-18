let w = De.make_window ~bits:15
let l = De.Lz77.make_window ~bits:15
let o = De.bigstring_create De.io_buffer_size
let i = De.bigstring_create De.io_buffer_size
let q = De.Queue.create 4096
let str fmt = Format.asprintf fmt
let msgf fmt = Format.kasprintf (fun msg -> `Msg msg) fmt
let error_msgf fmt = Format.kasprintf (fun err -> Error (`Msg err)) fmt

let bigstring_input ic buf off len =
  let tmp = Bytes.create len in
  try
    let len = input ic tmp 0 len in
    for i = 0 to len - 1 do
      buf.{off + i} <- Bytes.get tmp i
    done
    ; len
  with End_of_file -> 0

let bigstring_output oc buf off len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set res i buf.{off + i}
  done
  ; output_string oc (Bytes.unsafe_to_string res)

let run_inflate ic oc =
  let open De in
  let decoder = Inf.decoder `Manual ~o ~w in
  let rec go () =
    match Inf.decode decoder with
    | `Await ->
      let len = bigstring_input ic i 0 io_buffer_size in
      Inf.src decoder i 0 len ; go ()
    | `Flush ->
      let len = io_buffer_size - Inf.dst_rem decoder in
      bigstring_output oc o 0 len
      ; Inf.flush decoder
      ; go ()
    | `Malformed err -> `Error (false, str "%s." err)
    | `End ->
      let len = io_buffer_size - Inf.dst_rem decoder in
      if len > 0 then bigstring_output oc o 0 len
      ; `Ok 0 in
  go ()

let run_deflate ~level ic oc =
  let open De in
  let state = Lz77.state ~level ~q ~w:l (`Channel ic) in
  let encoder = Def.encoder (`Channel oc) ~q in

  let rec compress () =
    match De.Lz77.compress state with
    | `Await -> assert false
    | `Flush ->
      let literals = Lz77.literals state in
      let distances = Lz77.distances state in
      encode
      @@ Def.encode encoder
           (`Block
              {
                Def.kind=
                  Dynamic (Def.dynamic_of_frequencies ~literals ~distances)
              ; last= false
              })
    | `End ->
      Queue.push_exn q Queue.eob
      ; pending @@ Def.encode encoder (`Block {Def.kind= Fixed; last= true})
  and pending = function `Partial | `Block -> assert false | `Ok -> ()
  and encode = function
    | `Partial -> assert false
    | `Ok | `Block -> compress () in
  Def.dst encoder o 0 io_buffer_size
  ; compress ()
  ; `Ok 0

let run_zlib_inflate ic oc =
  let open Zl in
  let allocate bits = De.make_window ~bits in
  let decoder = Inf.decoder `Manual ~o ~allocate in

  let rec go decoder =
    match Inf.decode decoder with
    | `Await decoder ->
      let len = bigstring_input ic i 0 De.io_buffer_size in
      Inf.src decoder i 0 len |> go
    | `Flush decoder ->
      let len = De.io_buffer_size - Inf.dst_rem decoder in
      bigstring_output oc o 0 len
      ; Inf.flush decoder |> go
    | `Malformed err -> `Error (false, str "%s." err)
    | `End decoder ->
      let len = De.io_buffer_size - Inf.dst_rem decoder in
      if len > 0 then bigstring_output oc o 0 len
      ; `Ok 0 in
  go decoder

let run_zlib_deflate ~level ic oc =
  let open Zl in
  let encoder = Def.encoder `Manual `Manual ~q ~w:l ~level in

  let rec go encoder =
    match Def.encode encoder with
    | `Await encoder ->
      let len = bigstring_input ic i 0 De.io_buffer_size in
      Def.src encoder i 0 len |> go
    | `Flush encoder ->
      let len = De.io_buffer_size - Def.dst_rem encoder in
      bigstring_output oc o 0 len
      ; Def.dst encoder o 0 De.io_buffer_size |> go
    | `End encoder ->
      let len = De.io_buffer_size - Def.dst_rem encoder in
      if len > 0 then bigstring_output oc o 0 len
      ; `Ok 0 in
  Def.dst encoder o 0 De.io_buffer_size |> go

let run_gzip_inflate ic oc =
  let open Gz in
  let decoder = Inf.decoder `Manual ~o in

  let rec go decoder =
    match Inf.decode decoder with
    | `Await decoder ->
      let len = bigstring_input ic i 0 io_buffer_size in
      Inf.src decoder i 0 len |> go
    | `Flush decoder ->
      let len = io_buffer_size - Inf.dst_rem decoder in
      bigstring_output oc o 0 len
      ; Inf.flush decoder |> go
    | `Malformed err -> `Error (false, str "%s." err)
    | `End decoder ->
      let len = io_buffer_size - Inf.dst_rem decoder in
      if len > 0 then bigstring_output oc o 0 len
      ; `Ok 0 in
  go decoder

let now () = Int32.of_float (Unix.gettimeofday ())

let run_gzip_deflate ~level ic oc =
  let open Gz in
  let encoder =
    Def.encoder `Manual `Manual ~q ~w:l ~level ~mtime:(now ()) Gz.Unix in

  let rec go encoder =
    match Def.encode encoder with
    | `Await encoder ->
      let len = bigstring_input ic i 0 io_buffer_size in
      Def.src encoder i 0 len |> go
    | `Flush encoder ->
      let len = io_buffer_size - Def.dst_rem encoder in
      bigstring_output oc o 0 len
      ; Def.dst encoder o 0 io_buffer_size |> go
    | `End encoder ->
      let len = io_buffer_size - Def.dst_rem encoder in
      if len > 0 then bigstring_output oc o 0 len
      ; `Ok 0 in
  Def.dst encoder o 0 io_buffer_size |> go

external string_get_uint32 : string -> int -> int32 = "%caml_string_get32"

external bigstring_set_uint32 : Lzo.bigstring -> int -> int32 -> unit
  = "%caml_bigstring_set32"

let string_get_uint8 str idx = Char.code (String.get str idx)

external bigstring_set_uint8 : Lzo.bigstring -> int -> int -> unit
  = "%caml_ba_set_1"

let run_lzo_deflate ic oc =
  let wrkmem = Lzo.make_wrkmem () in
  let in_contents =
    let buf = Buffer.create 0x1000 in
    let tmp = Bytes.create 0x100 in
    let rec go () =
      match input ic tmp 0 (Bytes.length tmp) with
      | 0 -> Buffer.contents buf
      | len ->
        Buffer.add_subbytes buf tmp 0 len
        ; go ()
      | exception End_of_file -> Buffer.contents buf in
    go () in
  let in_contents =
    let len = String.length in_contents in
    let res = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in
    let len0 = len land 3 in
    let len1 = len asr 2 in
    for i = 0 to len1 - 1 do
      let i = i * 4 in
      let v = string_get_uint32 in_contents i in
      bigstring_set_uint32 res i v
    done
    ; for i = 0 to len0 - 1 do
        let i = (len1 * 4) + i in
        let v = string_get_uint8 in_contents i in
        bigstring_set_uint8 res i v
      done
    ; res in
  let out_contents =
    Bigarray.(Array1.create char c_layout (Array1.dim in_contents * 2)) in
  match Lzo.compress in_contents out_contents wrkmem with
  | len ->
    bigstring_output oc out_contents 0 len
    ; `Ok 0
  | exception Invalid_argument _ -> assert false

let run_lzo_inflate ic oc =
  let in_contents =
    let buf = Buffer.create 0x1000 in
    let tmp = Bytes.create 0x100 in
    let rec go () =
      match input ic tmp 0 (Bytes.length tmp) with
      | 0 -> Buffer.contents buf
      | len ->
        Buffer.add_subbytes buf tmp 0 len
        ; go ()
      | exception End_of_file -> Buffer.contents buf in
    go () in
  let in_contents =
    let len = String.length in_contents in
    let res = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in
    let len0 = len land 3 in
    let len1 = len asr 2 in
    for i = 0 to len1 - 1 do
      let i = i * 4 in
      let v = string_get_uint32 in_contents i in
      bigstring_set_uint32 res i v
    done
    ; for i = 0 to len0 - 1 do
        let i = (len1 * 4) + i in
        let v = string_get_uint8 in_contents i in
        bigstring_set_uint8 res i v
      done
    ; res in
  match Lzo.uncompress_with_buffer in_contents with
  | Ok str -> output_string oc str ; `Ok 0
  | Error err -> `Error (false, str "%a." Lzo.pp_error err)

let run deflate format level filename_ic filename_oc =
  let ic, close_ic =
    match filename_ic with
    | Some filename ->
      let ic = open_in_bin filename in
      ic, fun () -> close_in ic
    | None -> stdin, ignore in
  let oc, close_oc =
    match filename_oc with
    | Some filename ->
      let oc = open_out_bin filename in
      oc, fun () -> close_out oc
    | None -> stdout, ignore in
  let res =
    match deflate, format with
    | true, `Deflate -> run_deflate ~level ic oc
    | false, `Deflate -> run_inflate ic oc
    | true, `Zlib -> run_zlib_deflate ~level ic oc
    | false, `Zlib -> run_zlib_inflate ic oc
    | true, `Gzip -> run_gzip_deflate ~level ic oc
    | false, `Gzip -> run_gzip_inflate ic oc
    | true, `Lzo -> run_lzo_deflate ic oc
    | false, `Lzo -> run_lzo_inflate ic oc in
  close_ic () ; close_oc () ; res

open Cmdliner

let deflate =
  let doc = "Ask to deflate inputs (instead of inflate)." in
  Arg.(value & flag & info ["d"] ~doc)

let format =
  let parser s =
    match String.lowercase_ascii s with
    | "zlib" -> Ok `Zlib
    | "gzip" -> Ok `Gzip
    | "deflate" -> Ok `Deflate
    | "lzo" -> Ok `Lzo
    | x -> error_msgf "Invalid format: %S" x in
  let pp ppf = function
    | `Zlib -> Format.pp_print_string ppf "zlib"
    | `Gzip -> Format.pp_print_string ppf "gzip"
    | `Deflate -> Format.pp_print_string ppf "deflate"
    | `Lzo -> Format.pp_print_string ppf "lzo" in
  let format = Arg.conv (parser, pp) in
  Arg.(value & opt format `Deflate & info ["f"; "format"] ~docv:"<format>")

let input = Arg.(value & pos 0 (some file) None & info [] ~docv:"<filename>")
let output = Arg.(value & pos 1 (some string) None & info [] ~docv:"<filename>")

let level =
  let parser str =
    match int_of_string str with
    | n when n >= 0 -> Ok n
    | _ -> Error (`Msg "The compression level must be positive")
    | exception _ -> Error (`Msg "Invalid level") in
  let positive_int = Arg.conv (parser, Format.pp_print_int) in
  Arg.(value & opt positive_int 4 & info ["l"; "level"] ~docv:"<level>")

let command =
  let doc =
    "A tool to deflate/inflate a stream/file throught a specified format." in
  let man =
    [
      `S Manpage.s_description
    ; `P
        "$(tname) reads from the standard input and writes the \
         deflated/inflated data to the standard output. Several formats \
         exists:"
    ; `I
        ( "DEFLATE"
        , "DEFLATE is a lossless data compression file format that uses a \
           combination of LZ77 and Huffman coding. It is specified in RFC 1951 \
           <https://datatracker.ietf.org/doc/html/rfc1951>." ); `Noblank
    ; `I
        ( "GZip"
        , "GZip is a file format based on the DEFLATE algorithm, which is a \
           combination of LZ77 and Huffman coding. It encodes few informations \
           such as: the timestamp, the filename, or the operating system \
           (which operates the deflation). It generates a CRC-32 checksum at \
           the end of the stream. It is described by the RFC 1952 \
           <https://datatracker.ietf.org/doc/html/rfc1952>." ); `Noblank
    ; `I
        ( "Zlib"
        , "Zlib is an $(i,abstraction) of the DEFLATE algorithm compression \
           algorithm which terminates the stream with an ADLER-32 checksum." )
    ; `Noblank
    ; `I
        ( "Lempel-Ziv-Overhumer (LZO)"
        , "Lempel-Ziv-Oberhumer is a lossless data compression algorithm that \
           is focused on decompression speed." ); `S Manpage.s_examples
    ; `P
        "This is a small example of how to use $(tname) in your favorite shell:"
    ; `Pre
        "\\$ $(tname) -f gzip -d <<EOF > file.gz\n\
         Hello World!\n\
         EOF\n\
         \\$ $(tname) -f gzip < file.gz\n\
         Hello World!\n\
         \\$"; `S Manpage.s_bugs
    ; `P "Check bug reports at <https://github.com/mirage/decompress>"
    ] in
  let term = Term.(ret (const run $ deflate $ format $ level $ input $ output))
  and info = Cmd.info "decompress" ~doc ~man in
  Cmd.v info term

let () = exit (Cmd.eval' command)
