open Lwt.Infix

module Int63 = Optint.Int63

module B(Size : sig val sector_size : int end) = struct
  include Block
  include Size

  let convert_path os path =
    let ch = Unix.open_process_in (Printf.sprintf "cygpath -%c -- %s" (match os with `Mixed -> 'm' | `Unix -> 'u' | `Windows -> 'w') path) in
    let line = input_line ch in
    close_in ch;
    line

  let connect name =
    let name = if Sys.win32 then convert_path `Windows name else name in
    connect ~prefered_sector_size:(Some sector_size) name
end

module Block512 = B(struct let sector_size = 512 end)

module Block4096 = B(struct let sector_size = 4096 end)

module type BLOCK = sig
  include module type of Block
  val connect : string -> t Lwt.t
  val sector_size : int
end

let int63_hdr_len = Int63.of_int Tar.Header.length

module Test(B : BLOCK) = struct
  module KV_RW = Tar_mirage.Make_KV_RW(B)

  let kv_rw_error =
    Lwt.wrap1 (Result.fold ~ok:Fun.id ~error:(Alcotest.failf "%a" KV_RW.pp_error))

  let kv_rw_write_error =
    Lwt.wrap1 (Result.fold ~ok:Fun.id ~error:(Alcotest.failf "%a" KV_RW.pp_write_error))

  let connect_block switch =
    let filename = Filename.temp_file "tar-allocate-set-partial-test" ".tar" in
    B.connect filename >|= fun b ->
    Lwt_switch.add_hook (Some switch) (fun () ->
        B.disconnect b >>= fun () -> Lwt_unix.unlink filename);
    b

  let resize b size =
    B.resize b size >|=
    Result.iter_error (Alcotest.failf "%a" B.pp_write_error)

  let allocate_empty_file switch () =
    connect_block switch >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.allocate t (Mirage_kv.Key.v "empty") Int63.zero >>=
    kv_rw_write_error

  let set_partial_no_file switch () =
    connect_block switch >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set_partial t (Mirage_kv.Key.v "nonexistent")
      ~offset:Int63.zero "" >>= function
    | Ok () -> Alcotest.fail "expected set_partial on nonexistent file to fail"
    | Error _ -> Lwt.return_unit

  let allocate_is_zeroed switch () =
    connect_block switch >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    let key = Mirage_kv.Key.v "zeroed" in
    KV_RW.allocate t key int63_hdr_len >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.get t key >>=
    kv_rw_error >|=
    Alcotest.(check string) "is zeroed" (String.make Tar.Header.length '\000')

  let allocate_two_one_byte_files_zeroed switch () =
    connect_block switch >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    let k1 = Mirage_kv.Key.v "zeroed" and k2 = Mirage_kv.Key.v "zeroed2" in
    KV_RW.allocate t k1 Int63.one >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.allocate t k2 Int63.one >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.get t k1 >>=
    kv_rw_error >>= fun s ->
    Alcotest.(check string) "is zero" "\000" s;
    KV_RW.get t k2 >>=
    kv_rw_error >|=
    Alcotest.(check string) "is zero" "\000"

  let allocate_set_partial_first_byte switch () =
    connect_block switch >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    let k = Mirage_kv.Key.v "X" in
    KV_RW.allocate t k Int63.(add one one) >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.set_partial t k ~offset:Int63.zero "X" >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.get t k >>=
    kv_rw_error >|=
    Alcotest.(check string) "partial" "X\000"

  let rename_nonexistent_file switch () =
    connect_block switch >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    let source = Mirage_kv.Key.v "source"
    and dest = Mirage_kv.Key.v "destination" in
    KV_RW.rename t ~source ~dest >|= function
    | Ok () -> Alcotest.fail "Expected failure to rename nonexistent file"
    | Error _ -> ()

  let set_rename switch () =
    connect_block switch >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    let source = Mirage_kv.Key.v "source"
    and dest = Mirage_kv.Key.v "destination" in
    let s =
      String.init (3 * Tar.Header.length)
        (fun i -> "0123456789".[i mod 10])
    in
    KV_RW.set t source s >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.rename t ~source ~dest >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.get t dest >>=
    kv_rw_error >|= fun s' ->
    Alcotest.(check string) "renamed" s s'

  let tests =
    let ( >:: ) desc f =
      Alcotest_lwt.test_case (Printf.sprintf "%s b%d" desc B.sector_size) `Quick f
    in
    [
      "allocate empty file" >:: allocate_empty_file;
      "set_partial nonexistent file" >:: set_partial_no_file;
      "allocate is zeroed" >:: allocate_is_zeroed;
      "allocate two one-byte files" >:: allocate_two_one_byte_files_zeroed;
      "allocate and set first byte" >:: allocate_set_partial_first_byte;
      "set and rename" >:: set_rename;
    ]

end

module Test512 = Test(Block512)
module Test4096 = Test(Block4096)


let () =
  Lwt_main.run @@ Alcotest_lwt.run "tar-allocate-set-partial"
  [
    "Test512", Test512.tests;
    "Test4096", Test4096.tests;
  ]
