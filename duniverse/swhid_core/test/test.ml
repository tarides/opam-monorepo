open Swhid_core

let () =
  match
    Object.of_string "swh:1:cnt:bac494ecb6840e6b66f21aae7feb847b23f0745a"
  with
  | Ok v ->
    let s = Object.to_string v in
    assert (String.equal "swh:1:cnt:bac494ecb6840e6b66f21aae7feb847b23f0745a" s)
  | Error _e -> assert false
