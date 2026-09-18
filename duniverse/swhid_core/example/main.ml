open Swhid_core

let swhid =
  match
    Object.of_string "swh:1:cnt:bac494ecb6840e6b66f21aae7feb847b23f0745a"
  with
  | Error e ->
    Format.eprintf "error: %s@." e;
    exit 1
  | Ok id -> Format.printf "%a is a valid swhid !@." Object.pp id
