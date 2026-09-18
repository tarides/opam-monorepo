exception LZO of string

let _ = Callback.register_exception "lzo" (LZO "Invalid LZO input/output")

external compress :
  src:string -> src_off:int -> src_len:int -> dst:bytes -> dst_off:int -> int
  = "caml_lzo1x_1_compress"

external uncompress :
  src:string -> src_off:int -> src_len:int -> dst:bytes -> dst_off:int -> int
  = "caml_lzo1x_decompress"

let minilzo () =
  let str = String.make 256 'a' in
  let buf = Bytes.create 65536 in
  let len = compress ~src:str ~src_off:0 ~src_len:256 ~dst:buf ~dst_off:0 in
  Format.eprintf ">>> len:%d\n%!" len
  ; let res = Bytes.sub_string buf 0 len in
    let buf = Bytes.create 256 in
    let len = uncompress ~src:res ~src_off:0 ~src_len:len ~dst:buf ~dst_off:0 in
    assert (str = Bytes.sub_string buf 0 len)
