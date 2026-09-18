let const x _ = x

let rec unroll :
    (module Checkseum.S) -> Optint.t -> string -> int -> int -> Optint.t =
 fun (module M) crc str off len ->
  if len = 0
  then crc
  else
    let len' = min len 1 in
    let crc = M.(digest_string str off len' crc) in
    unroll (module M) crc str (off + len') (len - len')

let make ~name (module M : Checkseum.S) input expected =
  let checkseum = Alcotest.testable M.pp M.equal in
  ( name,
    `Quick,
    fun () ->
      let crc = unroll (module M) M.default input 0 (String.length input) in
      Alcotest.check checkseum name crc M.(of_int32 expected) )

let bib =
  let ic = open_in "bib" in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln ;
  close_in ic ;
  Bytes.unsafe_to_string rs

let () =
  Alcotest.run "checkseum"
    [
      ( "adler32",
        [
          make ~name:"0" (module Checkseum.Adler32) "" 1l;
          make ~name:"1" (module Checkseum.Adler32) "\x00" 65537l;
          make ~name:"2"
            (module Checkseum.Adler32)
            "\xff\xff\xff\xff" 167379965l;
          make ~name:"3" (module Checkseum.Adler32) "123456789" 0x91E01DEl;
          make ~name:"4"
            (module Checkseum.Adler32)
            (String.concat ""
               [
                 "\x9d\x02\x9d\x02\x90\xad\x14\x72\xb9\xb4\x44\x59\x5d\x21\x05\xb7";
                 "\x34\x4f\x64\xe9\xa8\x5a\x3e\xd9\x91\x1f\x44\x91\xc1\x5c";
               ])
            0xBDE50CD1l;
          make ~name:"5" (module Checkseum.Adler32) bib 0xDE83A296l;
        ] );
      ( "crc32c",
        [
          make ~name:"0" (module Checkseum.Crc32c) "" 0l;
          make ~name:"1" (module Checkseum.Crc32c) "\x00" 0x527d5351l;
          make ~name:"2"
            (module Checkseum.Crc32c)
            "\xff\xff\xff\xff" 0xffffffffl;
          make ~name:"3" (module Checkseum.Crc32c) "123456789" 0xe3069283l;
          make ~name:"4"
            (module Checkseum.Crc32c)
            "Thou hast made me, and shall thy work decay?" 0x866374c0l;
        ] );
      ( "crc32",
        [
          make ~name:"0" (module Checkseum.Crc32) "" 0l;
          make ~name:"1" (module Checkseum.Crc32) "\x00" 0xd202ef8dl;
          make ~name:"2" (module Checkseum.Crc32) "\xff\xff\xff\xff" 0xffffffffl;
          make ~name:"3" (module Checkseum.Crc32) "123456789" 0xcbf43926l;
          make ~name:"4"
            (module Checkseum.Crc32)
            "Thou hast made me, and shall thy work decay?" 0xf1fabe1dl;
          make ~name:"5"
            (module Checkseum.Crc32)
            (String.concat "%" (List.init 1000 (const "abcdef")))
            0xadc436fl;
        ] );
      ( "crc24",
        [
          make ~name:"0" (module Checkseum.Crc24) "" 0xb704cel;
          make ~name:"1" (module Checkseum.Crc24) "a" 0xf25713l;
          make ~name:"2" (module Checkseum.Crc24) "abc" 0xba1c7bl;
          make ~name:"3" (module Checkseum.Crc24) "message digest" 0xdbf0b6l;
        ] );
    ]
