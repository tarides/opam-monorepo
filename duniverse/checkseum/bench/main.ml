open Bechamel
open Toolkit

let seed = "zYZnfMdSEP7F8JKqY9vt4A=="

let () =
  let raw = Base64.decode_exn seed in
  let arr = Array.make 8 0 in
  for i = 0 to 7 do
    arr.(i) <- (Char.code raw.[i] lsl 8) lor Char.code raw.[i + 1]
  done ;
  Random.full_init arr

let random_string len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set res i (Char.chr (Random.int 256))
  done ;
  Bytes.unsafe_to_string res

let digest_adler32 inputs i =
  let v = Cstruct.to_bigarray inputs.(i) in
  let len = Cstruct.length inputs.(i) in
  Staged.stage (fun () -> Checkseum.Adler32.(digest_bigstring v 0 len default))

let digest_crc32 inputs i =
  let v = Cstruct.to_bigarray inputs.(i) in
  let len = Cstruct.length inputs.(i) in
  Staged.stage (fun () -> Checkseum.Crc32.(digest_bigstring v 0 len default))

let digest_crc32c inputs i =
  let v = Cstruct.to_bigarray inputs.(i) in
  let len = Cstruct.length inputs.(i) in
  Staged.stage (fun () -> Checkseum.Crc32c.(digest_bigstring v 0 len default))

let digest_tcpip inputs v =
  let v = inputs.(v) in
  Staged.stage (fun () -> Tcpip_checksum.ones_complement v)

let inputs = List.init 5 (fun i -> (i, Cstruct.of_string (random_string 4096)))
let args, inputs = List.split inputs

let test_adler32 =
  Test.make_indexed ~name:"ADLER-32" ~fmt:"%s %d" ~args
    (digest_adler32 (Array.of_list inputs))

let test_crc32 =
  Test.make_indexed ~name:"CRC-32" ~fmt:"%s %d" ~args
    (digest_crc32 (Array.of_list inputs))

let test_crc32c =
  Test.make_indexed ~name:"CRC-32C" ~fmt:"%s %d" ~args
    (digest_crc32c (Array.of_list inputs))

let test_tcpip =
  Test.make_indexed ~name:"TCP/IP" ~fmt:"%s %d" ~args
    (digest_tcpip (Array.of_list inputs))

let test =
  Test.make_grouped ~name:"checksum"
    [ test_adler32; test_tcpip; test_crc32; test_crc32c ]

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |] in
  let instances =
    Instance.[ minor_allocated; major_allocated; monotonic_clock ] in
  let cfg =
    Benchmark.cfg ~limit:3000 ~quota:Time.(second 1.) ~kde:(Some 1000) () in
  let raw_results = Benchmark.all cfg instances test in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)

let compare k0 k1 =
  let a = ref 0 and b = ref 0 in
  Scanf.sscanf k0 "%s %d" (fun _ a' -> a := a') ;
  Scanf.sscanf k1 "%s %d" (fun _ b' -> b := b') ;
  compare !a !b

let nothing _ = Ok ()

let () =
  let results = benchmark () in
  let open Bechamel_js in
  match
    emit ~dst:(Channel stdout) nothing ~compare ~x_label:Measure.run
      ~y_label:(Measure.label Instance.monotonic_clock)
      results
  with
  | Ok () -> ()
  | Error (`Msg err) -> failwith err
