(*
 *	Copyright (C) 2006-2009 Vincent Hanquez <tab@snarc.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * SHA1 & SHA256 OCaml binding test unit
 *)

open OUnit

let cog = "The quick brown fox jumps over the lazy cog"
let dog = "The quick brown fox jumps over the lazy dog"

let ex_strings_sha1 = [
	("",
	"da39a3ee5e6b4b0d3255bfef95601890afd80709");
	(cog,
	"de9f2c7fd25e1b3afad3e85a0bd17d9b100db4b3");
	(dog,
	"2fd4e1c67a2d28fced849ee1bb76e7391b93eb12"); ]

let ex_strings_sha256 = [
	("",
	"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
	(cog,
	"e4c4d8f3bf76b692de791a173e05321150f7a345b46484fe427f6acc7ecc81be");
	(dog,
	"d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"); ]

let ex_strings_sha512 = [
	("",
	"cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e");
	(cog,
	"3eeee1d0e11733ef152a6c29503b3ae20c4f1f3cda4cb26f1bc1a41f91c7fe4ab3bd86494049e201c4bd5155f31ecb7a3c8606843c4cc8dfcab7da11c8ae5045");
	(dog,
	"07e547d9586f6a73f73fbac0435ed76951218fb7d0c8d788a309d785436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6"); ]


let ex_files_sha1 =
	[ ("sample.txt",
	"2fd4e1c67a2d28fced849ee1bb76e7391b93eb12") ]

let ex_files_sha256 =
	[ ("sample.txt",
	"d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592") ]

let ex_files_sha512 =
	[ ("sample.txt",
	"07e547d9586f6a73f73fbac0435ed76951218fb7d0c8d788a309d785436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6") ]

let ex_channels_sha1 =
	[ ("sample.txt", "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12") ]

let ex_channels_sha256 =
	[ ("sample.txt",
	"d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592") ]

let ex_channels_sha512 =
	[ ("sample.txt",
	"07e547d9586f6a73f73fbac0435ed76951218fb7d0c8d788a309d785436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6") ]

let stringfct_hex_sha1 s = Sha1.to_hex (Sha1.string s)
let stringfct_hex_sha256 s = Sha256.to_hex (Sha256.string s)
let stringfct_hex_sha512 s = Sha512.to_hex (Sha512.string s)

let stringfct_bin_sha1 s = Sha1.to_bin (Sha1.string s)
let stringfct_bin_sha256 s = Sha256.to_bin (Sha256.string s)
let stringfct_bin_sha512 s = Sha512.to_bin (Sha512.string s)

let filefct_sha1 s = Sha1.to_hex (Sha1.file s)
let filefct_sha256 s = Sha256.to_hex (Sha256.file s)
let filefct_sha512 s = Sha512.to_hex (Sha512.file s)

let channelfct_sha1 s i = Sha1.to_hex (Sha1.channel s i)
let channelfct_sha256 s i = Sha256.to_hex (Sha256.channel s i)
let channelfct_sha512 s i = Sha512.to_hex (Sha512.channel s i)

let test_strings stringfct arr _ =
	List.iter (fun (s,r) -> assert_equal r (stringfct s)) arr

let test_file filefct arr _ =
	List.iter (fun (f,r) -> assert_equal r (filefct f)) arr

let test_channel channelfct arr _ =
	List.iter (fun (c,r) ->
		let chan = open_in_bin c in
		let digest = channelfct chan (String.length dog) in
		close_in chan;
		assert_equal r digest) arr

let test_equal string eq arr _ =
  List.fold_left (fun s (s', _) ->
	if s = s' then
		assert_bool "sha eq failed" (eq (string s) (string s'))
	else
		assert_bool "sha neq failed" (not (eq (string s) (string s')));
	s)
	(List.hd arr |> fst) arr |> ignore

let test_of stringfct_to stringfct_of arr _ =
	List.iter (fun (s,_) -> assert_equal (stringfct_to s) (stringfct_of s)) arr

(* from: https://www.di-mgt.com.au/sha_testvectors.html *)
module Vectors = struct
  type 'a gen = unit -> 'a option

  let sha1str = "sha1", fun s -> Sha1.to_hex (Sha1.string s)
  let sha1strg = "sha1", fun g ->
      let g = g() in
      let ctx = Sha1.init() in
      let rec loop () = match g() with
        | None -> ()
        | Some s -> Sha1.update_string ctx s; loop ()
      in
      loop();
      Sha1.to_hex (Sha1.finalize ctx)

  let sha256str = "sha256", fun s -> Sha256.to_hex (Sha256.string s)
  let sha256strg = "sha256", fun g ->
      let g = g() in
      let ctx = Sha256.init() in
      let rec loop () = match g() with
        | None -> ()
        | Some s -> Sha256.update_string ctx s; loop ()
      in
      loop();
      Sha256.to_hex (Sha256.finalize ctx)

  let sha512str = "sha512", fun s -> Sha512.to_hex (Sha512.string s)
  let sha512strg = "sha512", fun g ->
      let g = g() in
      let ctx = Sha512.init() in
      let rec loop () = match g() with
        | None -> ()
        | Some s -> Sha512.update_string ctx s; loop ()
      in
      loop();
      Sha512.to_hex (Sha512.finalize ctx)

  let gen_repeat n s =
    let n = ref n in
    fun () ->
      if !n=0 then None
      else (
        decr n;
        Some s
      )

  let test_of ~name pairs ~input : _ list =
    List.map
      (fun ((hash_name, hash), expect) ->
        let test_name = name ^ "_" ^ hash_name in
        test_name >:: fun _ctx ->
          let res = hash input in
          assert_equal ~printer:(Printf.sprintf "%S") expect res)
      pairs

  [@@@ocaml.warning "-5"]

  let suite =
    "test_vectors" >::: List.flatten [
      test_of ~name:"t1" [
        sha1str,
        "a9993e364706816aba3e25717850c26c9cd0d89d";
        sha256str,
        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad";
        sha512str,
        "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f";
      ] ~input:"abc";
      test_of ~name:"t2" [
        sha1str,
"da39a3ee5e6b4b0d3255bfef95601890afd80709";
        sha256str,
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";
        sha512str,
        "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e";
      ] ~input:"";

      test_of ~name:"t3" [
        sha1str,
        "84983e441c3bd26ebaae4aa1f95129e5e54670f1";
        sha256str,
        "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1";
        sha512str,
        "204a8fc6dda82f0a0ced7beb8e08a41657c16ef468b228a8279be331a703c33596fd15c13b1b07f9aa1d3bea57789ca031ad85c7a71dd70354ec631238ca3445";
      ] ~input: "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq";

      test_of ~name:"t4" [
        sha1str,
        "a49b2446a02c645bf419f995b67091253a04a259";
        sha256str,
        "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1";
        sha512str,
        "8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909";
      ] ~input:
        "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu";

      test_of ~name:"t_Ma_1" [
        sha1strg,
        "34aa973cd4c4daa4f61eeb2bdbad27316534016f";
        sha256strg,
        "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0";
        sha512strg,
        "e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b";
      ] ~input:(fun() -> gen_repeat 100_000 (String.make 10 'a'));

      test_of ~name:"t_Ma_2" [
        sha1strg,
        "34aa973cd4c4daa4f61eeb2bdbad27316534016f";
        sha256strg,
        "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0";
        sha512strg,
        "e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b";
      ] ~input:(fun() -> gen_repeat 10_000 (String.make 100 'a'));

      test_of ~name:"t_Ma_3" [
        sha1strg,
        "34aa973cd4c4daa4f61eeb2bdbad27316534016f";
        sha256strg,
        "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0";
        sha512strg,
        "e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b";
      ] ~input: (fun() -> gen_repeat 1_000_000 (String.make 1 'a'));

      test_of ~name:"t_long" [
        sha1strg,
        "7789f0c9ef7bfc40d93311143dfbe69e2017f592";
        sha256strg,
        "50e72a0e26442fe2552dc3938ac58658228c0cbfb1d2ca872ae435266fcd055e";
        sha512strg,
        "b47c933421ea2db149ad6e10fce6c7f93d0752380180ffd7f4629a712134831d77be6091b819ed352c2967a2e2d4fa5050723c9630691f1a05a7281dbe6c1086";
      ] ~input:(fun() -> gen_repeat 16_777_216
                  "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno");
    ]

  end

let suite = "SHA binding test" >:::
	[ "SHA1 example strings" >::
		test_strings stringfct_hex_sha1 ex_strings_sha1;
	  "SHA1 reading a file" >::
		test_file filefct_sha1 ex_files_sha1;
	  "SHA1 reading few byte from channel" >::
		test_channel channelfct_sha1 ex_channels_sha1;
	  "SHA1 equality" >::
		test_equal Sha1.string Sha1.equal ex_strings_sha1;
	  "SHA1 converting from binary representation" >::
		test_of stringfct_bin_sha1 (fun s -> Sha1.(string s |> to_bin |> Bytes.of_string |> of_bin |> to_bin)) ex_strings_sha1;
	  "SHA1 converting from hexadecimal representation" >::
		test_of stringfct_hex_sha1 (fun s -> Sha1.(string s |> to_hex |> of_hex |> to_hex)) ex_strings_sha1;
	  "SHA256 example strings" >::
		test_strings stringfct_hex_sha256 ex_strings_sha256;
	  "SHA256 reading a file" >::
		test_file filefct_sha256 ex_files_sha256;
	  "SHA256 reading few byte from channel" >::
		test_channel channelfct_sha256 ex_channels_sha256;
	  "SHA256 equality" >::
		test_equal Sha256.string Sha256.equal ex_strings_sha256;
	  "SHA256 converting from binary representation" >::
		test_of stringfct_bin_sha256 (fun s -> Sha256.(string s |> to_bin |> Bytes.of_string |> of_bin |> to_bin)) ex_strings_sha256;
	  "SHA256 converting from hexadecimal representation" >::
		test_of stringfct_hex_sha256 (fun s -> Sha256.(string s |> to_hex |> of_hex |> to_hex)) ex_strings_sha256;
	  "SHA512 example strings" >::
		test_strings stringfct_hex_sha512 ex_strings_sha512;
	  "SHA512 reading a file" >::
		test_file filefct_sha512 ex_files_sha512;
	  "SHA512 reading few byte from channel" >::
		test_channel channelfct_sha512 ex_channels_sha512;
	  "SHA512 equality" >::
		test_equal Sha512.string Sha512.equal ex_strings_sha512;
	  "SHA1 converting from binary representation" >::
		test_of stringfct_bin_sha1 (fun s -> Sha1.(string s |> to_bin |> Bytes.of_string |> of_bin |> to_bin)) ex_strings_sha1;
	  "SHA1 converting from hexadecimal representation" >::
		test_of stringfct_hex_sha1 (fun s -> Sha1.(string s |> to_hex |> of_hex |> to_hex)) ex_strings_sha1;

       Vectors.suite;
	]

let _ = run_test_tt_main suite
