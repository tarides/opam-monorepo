let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

let invalid_bounds off len =
  invalid_arg "Invalid bounds (off: %d, len: %d)" off len

let strf = Format.asprintf
let io_buffer_size = 65536

type adler32 = Checkseum.Adler32.t
type crc32c = Checkseum.Crc32c.t
type crc32 = Checkseum.Crc32.t
type crc24 = Checkseum.Crc24.t

type 'kind kind =
  | Adler32 : adler32 kind
  | Crc32c : crc32c kind
  | Crc32 : crc32 kind
  | Crc24 : crc24 kind

module Checksum : sig
  type src = [ `Channel of in_channel | `Manual | `String of string ]
  type 'value ret = [ `Await | `End of 'value ]
  type 'value t

  val src : 'value t -> string -> int -> int -> unit
  val encoder : 'value kind -> default:'value -> src -> 'value t
  val encode : 'value t -> 'value ret
end = struct
  type src = [ `Channel of in_channel | `Manual | `String of string ]
  type 'value ret = [ `Await | `End of 'value ]

  type 'value t = {
    src : src;
    kind : 'value kind;
    mutable i : string;
    mutable i_off : int;
    mutable i_pos : int;
    mutable i_len : int;
    mutable value : 'value;
    mutable k : 'value t -> 'value ret;
  }

  let end_of_input encoder =
    encoder.i <- "" ;
    encoder.i_off <- 0 ;
    encoder.i_pos <- 0 ;
    encoder.i_len <- min_int

  let src encoder source off len =
    if off < 0 || len < 0 || off + len > String.length source
    then invalid_bounds off len
    else if len = 0
    then end_of_input encoder
    else (
      encoder.i <- source ;
      encoder.i_off <- off ;
      encoder.i_pos <- 0 ;
      encoder.i_len <- len - 1)

  let refill k encoder =
    match encoder.src with
    | `Manual ->
        encoder.k <- k ;
        `Await
    | `String _ ->
        end_of_input encoder ;
        k encoder
    | `Channel ic ->
        let len =
          input ic
            (Bytes.unsafe_of_string encoder.i)
            0 (String.length encoder.i) in
        src encoder encoder.i 0 len ;
        k encoder

  let ret k encoder =
    encoder.k <- k ;
    encoder.k encoder

  let r : type a. a kind -> a -> int -> int -> int -> string -> a =
   fun kind t off pos rem src ->
    match kind with
    | Adler32 -> Checkseum.Adler32.digest_string src (off + pos) rem t
    | Crc32c -> Checkseum.Crc32c.digest_string src (off + pos) rem t
    | Crc32 -> Checkseum.Crc32.digest_string src (off + pos) rem t
    | Crc24 -> Checkseum.Crc24.digest_string src (off + pos) rem t

  let i_rem encoder = encoder.i_len - encoder.i_pos + 1

  let rec encode encoder =
    let rem = i_rem encoder in
    if rem <= 0
    then if rem < 0 then `End encoder.value else refill encode encoder
    else
      let value =
        r encoder.kind encoder.value encoder.i_off encoder.i_pos rem encoder.i
      in
      encoder.i_pos <- encoder.i_pos + rem ;
      encoder.value <- value ;
      ret encode encoder

  let encoder : type a. a kind -> default:a -> src -> a t =
   fun kind ~default src ->
    let i, i_off, i_pos, i_len =
      match src with
      | `Manual -> ("", 0, 1, 0)
      | `Channel _ -> (String.make io_buffer_size '\000', 0, 1, 0)
      | `String s -> (s, 0, 0, String.length s - 1) in
    { src; kind; i; i_off; i_pos; i_len; value = default; k = encode }

  let encode encoder = encoder.k encoder
end

let ( >>= ) x f = match x with Ok x -> f x | Error err -> Error err
let error_msgf fmt = Format.kasprintf (fun err -> Error (`Msg err)) fmt

type v = Kind : 'kind kind * 'kind -> v

let kind_of_string x =
  match String.lowercase_ascii x with
  | "adler32" -> Ok (Kind (Adler32, Checkseum.Adler32.default))
  | "crc32c" -> Ok (Kind (Crc32c, Checkseum.Crc32c.default))
  | "crc32" -> Ok (Kind (Crc32, Checkseum.Crc32.default))
  | "crc24" -> Ok (Kind (Crc24, Checkseum.Crc24.default))
  | _ -> error_msgf "Invalid kind of checksum: %S." x

let open_in filename =
  if Sys.file_exists filename
  then
    let ic = open_in filename in
    Ok ic
  else error_msgf "File %S does not exist." filename

let pp : type k. k kind -> Format.formatter -> k -> unit = function
  | Adler32 -> Checkseum.Adler32.pp
  | Crc32c -> Checkseum.Crc32c.pp
  | Crc32 -> Checkseum.Crc32.pp
  | Crc24 -> Checkseum.Crc24.pp

let () =
  match Sys.argv with
  | [| _; kind; filename |] -> (
      let fiber () =
        kind_of_string kind >>= fun (Kind (k, default)) ->
        open_in filename >>= fun ic ->
        let encoder = Checksum.encoder k ~default (`Channel ic) in
        match Checksum.encode encoder with
        | `Await -> assert false
        | `End v ->
            close_in ic ;
            Format.printf "%a\n%!" (pp k) v ;
            Ok () in
      match fiber () with
      | Ok () -> ()
      | Error (`Msg err) -> Format.eprintf "%s\n%!" err)
  | [| _; kind |] -> (
      let fiber () =
        kind_of_string kind >>= fun (Kind (k, default)) ->
        let encoder = Checksum.encoder k ~default (`Channel stdin) in
        match Checksum.encode encoder with
        | `Await -> assert false
        | `End v ->
            Format.printf "%a\n%!" (pp k) v ;
            Ok () in
      match fiber () with
      | Ok () -> ()
      | Error (`Msg err) -> Format.eprintf "%s\n%!" err)
  | _ ->
      Format.eprintf "%s {adler32,crc32,crc32c,crc24} [filename]\n%!"
        Sys.argv.(0)
