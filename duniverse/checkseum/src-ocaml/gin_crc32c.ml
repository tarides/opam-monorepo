(* Copyright (c) 2011, Jonathan Derque - MIT licensed *)
(* Copyright (c) 2016, Gabriel de Perthuis - MIT licensed *)

let ffffffff = Optint.of_unsigned_int32 (-1l)
let ff = Optint.of_unsigned_int 0xff

let crc_table =
  Array.map Optint.of_unsigned_int32
    [|
      0x00000000l;
      0xf26b8303l;
      0xe13b70f7l;
      0x1350f3f4l;
      0xc79a971fl;
      0x35f1141cl;
      0x26a1e7e8l;
      0xd4ca64ebl;
      0x8ad958cfl;
      0x78b2dbccl;
      0x6be22838l;
      0x9989ab3bl;
      0x4d43cfd0l;
      0xbf284cd3l;
      0xac78bf27l;
      0x5e133c24l;
      0x105ec76fl;
      0xe235446cl;
      0xf165b798l;
      0x030e349bl;
      0xd7c45070l;
      0x25afd373l;
      0x36ff2087l;
      0xc494a384l;
      0x9a879fa0l;
      0x68ec1ca3l;
      0x7bbcef57l;
      0x89d76c54l;
      0x5d1d08bfl;
      0xaf768bbcl;
      0xbc267848l;
      0x4e4dfb4bl;
      0x20bd8edel;
      0xd2d60dddl;
      0xc186fe29l;
      0x33ed7d2al;
      0xe72719c1l;
      0x154c9ac2l;
      0x061c6936l;
      0xf477ea35l;
      0xaa64d611l;
      0x580f5512l;
      0x4b5fa6e6l;
      0xb93425e5l;
      0x6dfe410el;
      0x9f95c20dl;
      0x8cc531f9l;
      0x7eaeb2fal;
      0x30e349b1l;
      0xc288cab2l;
      0xd1d83946l;
      0x23b3ba45l;
      0xf779deael;
      0x05125dadl;
      0x1642ae59l;
      0xe4292d5al;
      0xba3a117el;
      0x4851927dl;
      0x5b016189l;
      0xa96ae28al;
      0x7da08661l;
      0x8fcb0562l;
      0x9c9bf696l;
      0x6ef07595l;
      0x417b1dbcl;
      0xb3109ebfl;
      0xa0406d4bl;
      0x522bee48l;
      0x86e18aa3l;
      0x748a09a0l;
      0x67dafa54l;
      0x95b17957l;
      0xcba24573l;
      0x39c9c670l;
      0x2a993584l;
      0xd8f2b687l;
      0x0c38d26cl;
      0xfe53516fl;
      0xed03a29bl;
      0x1f682198l;
      0x5125dad3l;
      0xa34e59d0l;
      0xb01eaa24l;
      0x42752927l;
      0x96bf4dccl;
      0x64d4cecfl;
      0x77843d3bl;
      0x85efbe38l;
      0xdbfc821cl;
      0x2997011fl;
      0x3ac7f2ebl;
      0xc8ac71e8l;
      0x1c661503l;
      0xee0d9600l;
      0xfd5d65f4l;
      0x0f36e6f7l;
      0x61c69362l;
      0x93ad1061l;
      0x80fde395l;
      0x72966096l;
      0xa65c047dl;
      0x5437877el;
      0x4767748al;
      0xb50cf789l;
      0xeb1fcbadl;
      0x197448ael;
      0x0a24bb5al;
      0xf84f3859l;
      0x2c855cb2l;
      0xdeeedfb1l;
      0xcdbe2c45l;
      0x3fd5af46l;
      0x7198540dl;
      0x83f3d70el;
      0x90a324fal;
      0x62c8a7f9l;
      0xb602c312l;
      0x44694011l;
      0x5739b3e5l;
      0xa55230e6l;
      0xfb410cc2l;
      0x092a8fc1l;
      0x1a7a7c35l;
      0xe811ff36l;
      0x3cdb9bddl;
      0xceb018del;
      0xdde0eb2al;
      0x2f8b6829l;
      0x82f63b78l;
      0x709db87bl;
      0x63cd4b8fl;
      0x91a6c88cl;
      0x456cac67l;
      0xb7072f64l;
      0xa457dc90l;
      0x563c5f93l;
      0x082f63b7l;
      0xfa44e0b4l;
      0xe9141340l;
      0x1b7f9043l;
      0xcfb5f4a8l;
      0x3dde77abl;
      0x2e8e845fl;
      0xdce5075cl;
      0x92a8fc17l;
      0x60c37f14l;
      0x73938ce0l;
      0x81f80fe3l;
      0x55326b08l;
      0xa759e80bl;
      0xb4091bffl;
      0x466298fcl;
      0x1871a4d8l;
      0xea1a27dbl;
      0xf94ad42fl;
      0x0b21572cl;
      0xdfeb33c7l;
      0x2d80b0c4l;
      0x3ed04330l;
      0xccbbc033l;
      0xa24bb5a6l;
      0x502036a5l;
      0x4370c551l;
      0xb11b4652l;
      0x65d122b9l;
      0x97baa1bal;
      0x84ea524el;
      0x7681d14dl;
      0x2892ed69l;
      0xdaf96e6al;
      0xc9a99d9el;
      0x3bc21e9dl;
      0xef087a76l;
      0x1d63f975l;
      0x0e330a81l;
      0xfc588982l;
      0xb21572c9l;
      0x407ef1cal;
      0x532e023el;
      0xa145813dl;
      0x758fe5d6l;
      0x87e466d5l;
      0x94b49521l;
      0x66df1622l;
      0x38cc2a06l;
      0xcaa7a905l;
      0xd9f75af1l;
      0x2b9cd9f2l;
      0xff56bd19l;
      0x0d3d3e1al;
      0x1e6dcdeel;
      0xec064eedl;
      0xc38d26c4l;
      0x31e6a5c7l;
      0x22b65633l;
      0xd0ddd530l;
      0x0417b1dbl;
      0xf67c32d8l;
      0xe52cc12cl;
      0x1747422fl;
      0x49547e0bl;
      0xbb3ffd08l;
      0xa86f0efcl;
      0x5a048dffl;
      0x8ecee914l;
      0x7ca56a17l;
      0x6ff599e3l;
      0x9d9e1ae0l;
      0xd3d3e1abl;
      0x21b862a8l;
      0x32e8915cl;
      0xc083125fl;
      0x144976b4l;
      0xe622f5b7l;
      0xf5720643l;
      0x07198540l;
      0x590ab964l;
      0xab613a67l;
      0xb831c993l;
      0x4a5a4a90l;
      0x9e902e7bl;
      0x6cfbad78l;
      0x7fab5e8cl;
      0x8dc0dd8fl;
      0xe330a81al;
      0x115b2b19l;
      0x020bd8edl;
      0xf0605beel;
      0x24aa3f05l;
      0xd6c1bc06l;
      0xc5914ff2l;
      0x37faccf1l;
      0x69e9f0d5l;
      0x9b8273d6l;
      0x88d28022l;
      0x7ab90321l;
      0xae7367cal;
      0x5c18e4c9l;
      0x4f48173dl;
      0xbd23943el;
      0xf36e6f75l;
      0x0105ec76l;
      0x12551f82l;
      0xe03e9c81l;
      0x34f4f86al;
      0xc69f7b69l;
      0xd5cf889dl;
      0x27a40b9el;
      0x79b737bal;
      0x8bdcb4b9l;
      0x988c474dl;
      0x6ae7c44el;
      0xbe2da0a5l;
      0x4c4623a6l;
      0x5f16d052l;
      0xad7d5351l;
    |]

let buf_fold_left get f acc buf offset length =
  let acc_r = ref acc in
  for i = offset to offset + length - 1 do
    acc_r := f !acc_r (get buf i)
  done ;
  !acc_r

let update_crc acc c =
  let index =
    Optint.(to_unsigned_int (logand acc ff)) lxor int_of_char c land 0xff in
  Optint.logand
    (Optint.logxor crc_table.(index) (Optint.shift_right_logical acc 8))
    ffffffff

let crc32c :
    type a. get:(a -> int -> char) -> a -> int -> int -> Optint.t -> Optint.t =
 fun ~get buf off len crc ->
  Optint.logxor
    (buf_fold_left get update_crc (Optint.logxor crc ffffffff) buf off len)
    ffffffff

type t = Optint.t

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let equal a b = Optint.equal a b
let pp ppf v = Optint.pp ppf v
let default = Optint.zero
let digest_bigstring a o l v = crc32c ~get:Bigarray.Array1.get a o l v

let unsafe_digest_bigstring a o l v =
  crc32c ~get:Bigarray.Array1.unsafe_get a o l v

let digest_string a o l v = crc32c ~get:String.get a o l v
let unsafe_digest_string a o l v = crc32c ~get:String.unsafe_get a o l v
let digest_bytes a o l v = crc32c ~get:Bytes.get a o l v
let unsafe_digest_bytes a o l v = crc32c ~get:Bytes.unsafe_get a o l v
let to_int32 = Optint.to_unsigned_int32
let of_int32 = Optint.of_unsigned_int32
