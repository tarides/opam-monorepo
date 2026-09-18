open Crowbar

let ( <.> ) f g x = f (g x)

let non_empty_bytes n : string Crowbar.gen =
  let open Crowbar in
  let ( >>= ) = dynamic_bind in

  let rec go acc = function
    | 0 -> concat_gen_list (const "") acc
    | n -> go (map [uint8] (String.make 1 <.> Char.chr) :: acc) (pred n) in
  let gen n = go [] n in

  range n >>= (gen <.> succ)

let wrkmem = Lzo.make_wrkmem ()

let () =
  add_test ~name:"lzo/minilzo" [non_empty_bytes 256] @@ fun str ->
  let bstr = Bstr.string str ~off:0 ~len:(String.length str) in
  let output = Bstr.create 65536 in
  let len = Lzo.compress bstr output wrkmem in
  let buf = Bytes.create (String.length str) in
  let len =
    Minilzo.uncompress
      ~src:(Bstr.sub_string output ~off:0 ~len)
      ~src_off:0 ~src_len:len ~dst:buf ~dst_off:0 in
  check_eq str (Bytes.sub_string buf 0 len)

let () =
  add_test ~name:"minilzo/lzo" [non_empty_bytes 256] @@ fun str ->
  let buf = Bytes.create 65536 in
  let len =
    Minilzo.compress ~src:str ~src_off:0 ~src_len:(String.length str) ~dst:buf
      ~dst_off:0 in
  let bstr = Bstr.string (Bytes.unsafe_to_string buf) ~off:0 ~len in
  match Lzo.uncompress_with_buffer bstr with
  | Ok str' -> check_eq str str'
  | Error _ -> Crowbar.fail "Invalid output of minilzo"
