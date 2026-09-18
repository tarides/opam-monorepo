let failwithf fmt = Format.kasprintf failwith fmt
let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt

let median arr =
  let arr = Array.copy arr in
  Array.sort compare arr
  ; let len = Array.length arr in
    if len land 1 = 1 then arr.(len / 2)
    else (arr.((len / 2) - 1) +. arr.(len / 2)) /. 2.

let median ~repeat fn =
  let ts = Array.make repeat 0. in
  for idx = 0 to repeat - 1 do
    Gc.compact ()
    ; let t0 = Unix.gettimeofday () in
      fn ()
      ; let t1 = Unix.gettimeofday () in
        ts.(idx) <- t1 -. t0
  done
  ; median ts

let mbps ~bytes ~seconds =
  if seconds <= 0. then Float.infinity
  else float_of_int bytes /. 1_048_576. /. seconds

let q = De.Queue.create 0x1000
let i = De.bigstring_create De.io_buffer_size
let o = De.bigstring_create De.io_buffer_size
let w0 = De.Lz77.make_window ~bits:15
let w1 = De.make_window ~bits:15
let allocate = Fun.const w1

let de_compress ~level str =
  De.Queue.reset q
  ; let buf = Buffer.create (String.length str / 2) in
    let pos = ref 0 in
    let refill tmp =
      let len = Int.min (De.bigstring_length tmp) (String.length str - !pos) in
      Bstr.blit_from_string str ~src_off:!pos tmp ~dst_off:0 ~len
      ; pos := !pos + len
      ; len in
    let flush tmp len =
      Buffer.add_string buf (Bstr.sub_string tmp ~off:0 ~len) in
    Zl.Higher.compress ~level ~dynamic:true ~w:w0 ~q ~refill ~flush i o
    ; Buffer.contents buf

let cz_compress ~level str =
  let buf = Buffer.create (String.length str / 2) in
  let pos = ref 0 in
  let refill tmp =
    let len = Int.min (Bytes.length tmp) (String.length str - !pos) in
    Bytes.blit_string str !pos tmp 0 len
    ; pos := !pos + len
    ; len in
  let flush tmp len = Buffer.add_subbytes buf tmp 0 len in
  Zlib.compress ~level ~header:true refill flush
  ; Buffer.contents buf

let bt_compress ~level str =
  (* NOTE(dinosaure): It probably exists a better-optimized way to use [Bytesrw_zlib]. *)
  let fn = Bytesrw_zlib.Zlib.compress_reads ~level () in
  let ic = Bytesrw.Bytes.Reader.of_string str in
  let oc = fn ic in
  Bytesrw.Bytes.Reader.to_string oc

let de_decompress str =
  let buf = Buffer.create (String.length str * 3) in
  let pos = ref 0 in
  let refill tmp =
    let len = Int.min (De.bigstring_length tmp) (String.length str - !pos) in
    Bstr.blit_from_string str ~src_off:!pos tmp ~dst_off:0 ~len
    ; pos := !pos + len
    ; len in
  let flush tmp len = Buffer.add_string buf (Bstr.sub_string tmp ~off:0 ~len) in
  let result = Zl.Higher.uncompress ~allocate ~refill ~flush i o in
  match result with
  | Ok () -> Buffer.contents buf
  | Error (`Msg msg) -> failwithf "decompress inflate: %s" msg

let cz_decompress str =
  let buf = Buffer.create (String.length str * 3) in
  let pos = ref 0 in
  let refill tmp =
    let len = Int.min (Bytes.length tmp) (String.length str - !pos) in
    Bytes.blit_string str !pos tmp 0 len
    ; pos := !pos + len
    ; len in
  let flush tmp len = Buffer.add_subbytes buf tmp 0 len in
  Zlib.uncompress ~header:true refill flush
  ; Buffer.contents buf

let bt_decompress str =
  let fn = Bytesrw_zlib.Zlib.decompress_reads () in
  let ic = Bytesrw.Bytes.Reader.of_string str in
  let oc = fn ic in
  Bytesrw.Bytes.Reader.to_string oc

type t = {
    filepath: string
  ; level: int
  ; lib: string
  ; in_bytes: int
  ; out_bytes: int
  ; ratio: float
  ; compress: float
  ; decompress: float
}

let bench ~repeat ~levels filepath =
  let ic = open_in_bin filepath in
  let in_bytes = in_channel_length ic in
  let str = really_input_string ic in_bytes in
  close_in ic
  ; let filepath = Filename.basename filepath in
    let fn level =
      let de_out = de_compress ~level str in
      let cz_out = cz_compress ~level str in
      let bt_out = bt_compress ~level str in
      let de_ctimes =
        median ~repeat (fun () -> ignore (de_compress ~level str)) in
      let cz_ctimes =
        median ~repeat (fun () -> ignore (cz_compress ~level str)) in
      let bt_ctimes =
        median ~repeat (fun () -> ignore (bt_compress ~level str)) in
      let de_dtimes = median ~repeat (fun () -> ignore (de_decompress de_out)) in
      let cz_dtimes = median ~repeat (fun () -> ignore (cz_decompress cz_out)) in
      let bt_dtimes = median ~repeat (fun () -> ignore (bt_decompress bt_out)) in
      let de =
        let lib = "decompress"
        and out_bytes = String.length de_out
        and ratio = float_of_int (String.length de_out) /. float_of_int in_bytes
        and compress = mbps ~bytes:in_bytes ~seconds:de_ctimes
        and decompress = mbps ~bytes:in_bytes ~seconds:de_dtimes in
        {filepath; level; lib; in_bytes; out_bytes; ratio; compress; decompress}
      in
      let cz =
        let lib = "camlzip"
        and out_bytes = String.length cz_out
        and ratio = float_of_int (String.length cz_out) /. float_of_int in_bytes
        and compress = mbps ~bytes:in_bytes ~seconds:cz_ctimes
        and decompress = mbps ~bytes:in_bytes ~seconds:cz_dtimes in
        {filepath; level; lib; in_bytes; out_bytes; ratio; compress; decompress}
      in
      let bt =
        let lib = "bytesrw"
        and out_bytes = String.length bt_out
        and ratio = float_of_int (String.length bt_out) /. float_of_int in_bytes
        and compress = mbps ~bytes:in_bytes ~seconds:bt_ctimes
        and decompress = mbps ~bytes:in_bytes ~seconds:bt_dtimes in
        {filepath; level; lib; in_bytes; out_bytes; ratio; compress; decompress}
      in
      [de; cz; bt] in
    List.concat_map fn levels

let pp ppf t =
  Format.fprintf ppf "%-14s %4d %-11s %10d %10d %7.3f %9.1f %9.1f@." t.filepath
    t.level t.lib t.in_bytes t.out_bytes t.ratio t.compress t.decompress

let is_regular_file filepath = not (Sys.is_directory filepath)

let files_of_dirpath dirpath =
  Sys.readdir dirpath
  |> Array.to_list
  |> List.map (fun filename -> Filename.concat dirpath filename)
  |> List.filter is_regular_file
  |> List.sort compare

let run corpus levels repeat =
  let files = files_of_dirpath corpus in
  let fn = bench ~repeat ~levels in
  let rows = List.concat_map fn files in
  List.iter (Format.printf "%a" pp) rows

open Cmdliner

let corpus =
  let doc = "Corpus directory." in
  let parser str =
    if Sys.file_exists str && Sys.is_directory str then Ok str
    else error_msgf "%s does not exists (or is not a directory)" str in
  let open Arg in
  value
  & pos 0 (conv (parser, Format.pp_print_string)) "test/corpus"
  & info [] ~docv:"DIR" ~doc

let levels =
  let doc = "Comma-separated compression levels (1..9)." in
  let open Arg in
  value & opt (list int) [1; 6; 9] & info ["levels"] ~doc

let repeat =
  let doc = "Number of repetitions per measure." in
  let open Arg in
  value & opt int 5 & info ["repeat"] ~doc

let cmd =
  let doc =
    "Compare decompress, camlzip and bytesrw.zlib (on ratio and speed)." in
  let info = Cmd.info "b" ~doc in
  Cmd.v info Term.(const run $ corpus $ levels $ repeat)

let () = exit (Cmd.eval cmd)
