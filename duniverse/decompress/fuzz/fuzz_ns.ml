open De

let w = make_window ~bits:15
let i = bigstring_create 1024
let o = bigstring_create (Def.Ns.compress_bound 1024)

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
let ( >>= ) = Crowbar.dynamic_bind
let ( <.> ) f g x = f (g x)

let non_empty_bytes n : string Crowbar.gen =
  let open Crowbar in
  let ( >>= ) = dynamic_bind in

  let rec go acc = function
    | 0 -> concat_gen_list (const "") acc
    | n -> go (map [uint8] (String.make 1 <.> Char.chr) :: acc) (pred n) in
  let gen n = go [] n in

  range n >>= (gen <.> succ)

let () =
  Crowbar.add_test ~name:"compress ns/uncompress ns" [non_empty_bytes 1024]
  @@ fun input ->
  let len = String.length input in
  Bstr.blit_from_string input ~src_off:0 i ~dst_off:0 ~len
  ; let src_def = Bstr.sub i ~off:0 ~len in
    let res = Def.Ns.deflate src_def o in
    let[@warning "-partial-match"] (Ok res) = res in
    let src_inf = Bstr.sub o ~off:0 ~len:res in
    let res = Inf.Ns.inflate src_inf i in
    match res with
    | Ok (_, res) ->
      let output = Bstr.sub i ~off:0 ~len:res in
      Crowbar.check_eq ~eq:String.equal ~pp:pp_string ~cmp:String.compare
        (Bstr.to_string output) input
    | Error _err -> Crowbar.fail "iso fail"
