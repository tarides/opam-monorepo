open Lwt.Infix

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
  val connect: string -> t Lwt.t
  val sector_size : int
end

module Test(B : BLOCK) = struct
  module KV_RW = Tar_mirage.Make_KV_RW(B)

  let kv_rw_write_error =
    Lwt.wrap1
      (Result.iter_error (Alcotest.failf "%a" KV_RW.pp_write_error))

  let str n =
    Bytes.unsafe_to_string (Bytes.create n)

  let connect_block switch =
    let filename = Filename.temp_file "tar-write-test" ".tar" in
    B.connect filename >|= fun b ->
    Lwt_switch.add_hook (Some switch) (fun () ->
        B.disconnect b >>= fun () ->
        Lwt_unix.unlink filename);
    b

  let resize b size =
    B.resize b size >|= fun x ->
    Result.iter_error (Alcotest.failf "%a" B.pp_write_error)
      x

  let write_empty_file switch () =
    connect_block switch >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t (Mirage_kv.Key.v "barf") "" >>=
    kv_rw_write_error >>= fun () ->
    Lwt.return_unit

  let write_sector_size_file switch () =
    connect_block switch >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t (Mirage_kv.Key.v "barf") (str B.sector_size) >>=
    kv_rw_write_error >>= fun () ->
    Lwt.return_unit

  let write_sector_size switch () =
    connect_block switch >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t (Mirage_kv.Key.v "barf") (str (B.sector_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    Lwt.return_unit

  let write_two_sector_size switch () =
    connect_block switch >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t (Mirage_kv.Key.v "barf") (str (2 * B.sector_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    Lwt.return_unit

  let write_two_files switch () =
    connect_block switch >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t (Mirage_kv.Key.v "first") (str (B.sector_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.set t (Mirage_kv.Key.v "second") (str (2 * B.sector_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    Lwt.return_unit

  let write_two_files_remove_first switch () =
    let first = Mirage_kv.Key.v "first" and second = Mirage_kv.Key.v "second" in
    connect_block switch >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t first (str (B.sector_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.set t second (str (2 * B.sector_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.remove t first >>= function
    | Error _ (* XXX: `Append_only *) ->
      Lwt.return_unit
    | Ok () -> Alcotest.fail "Expected Error `Append_only"

  let write_two_files_remove_second switch () =
    let first = Mirage_kv.Key.v "first" and second = Mirage_kv.Key.v "second" in
    connect_block switch >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t first (str (B.sector_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.set t second (str (2 * B.sector_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.remove t second >>=
    kv_rw_write_error

  let remove_odd_file switch () =
    let first = Mirage_kv.Key.v "first" in
    connect_block switch >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t first (str 1) >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.remove t first >>=
    kv_rw_write_error

  let set_after_remove switch () =
    let first = Mirage_kv.Key.v "first" and second = Mirage_kv.Key.v "second" in
    connect_block switch >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t first "Some data\n" >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.remove t first >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.set t second "More data\n" >>=
    kv_rw_write_error

  let allocate_doesn't_affect_beyond_end_of_archive data_size switch () =
    let first = Mirage_kv.Key.v "first" in
    connect_block switch >>= fun b ->
    let align_sector n = (n + pred B.sector_size) / B.sector_size in
    let align_block n = (n + 511) / 512 in
    let size = B.sector_size * align_sector (512 + 512 * (align_block data_size) + 1024 + 512) in
    resize b (Int64.of_int size) >>= fun () ->
    (* write an empty archive with trailing \xFFs *)
    let config = B.to_config b in
    let oc = open_out config.path in
    output_string oc (String.init 1024 (Fun.const '\000'));
    output_string oc (String.init (size - 1024) (Fun.const '\xFF'));
    close_out oc;
    KV_RW.connect b >>= fun t ->
    KV_RW.allocate t first (Optint.Int63.of_int data_size) >>=
    kv_rw_write_error >|= fun () ->
    let ic = open_in config.path in
    Fun.protect
      (fun () ->
        seek_in ic 512; (* skip header *)
        (* check file content *)
        for _ = 1 to data_size do
          Alcotest.(check char) "corrupt data" '\x00' (input_char ic);
        done;
        for _ = 1 to 512 * align_block data_size - data_size do
          Alcotest.(check char) "corrupt padding" '\x00' (input_char ic);
        done;
        (* check sentinel *)
        for _ = 1 to 1024 do
          Alcotest.(check char) "corrupt sentinel" '\x00' (input_char ic);
        done;
        (* check tail is untouched *)
        for _ = 1 to size - 512 - 512 * align_block data_size - 1024 do
          Alcotest.(check char) "corrupt tail" '\xff' (input_char ic);
        done;
        Alcotest.(check int) "same position" size (pos_in ic)
      ) ~finally:(fun () -> close_in ic)



  let tests =
    let ( >:: ) desc f =
      Alcotest_lwt.test_case (Printf.sprintf "%s b%d" desc B.sector_size) `Quick f
    in
    [
      "write empty" >:: write_empty_file;
      "write block size" >:: write_sector_size_file;
      "write block size" >:: write_sector_size;
      "write two blocks" >:: write_two_sector_size;
      "write two files" >:: write_two_files;
      "write two files remove first" >:: write_two_files_remove_first;
      "write two files remove second" >:: write_two_files_remove_second;
      "remove odd sized file" >:: remove_odd_file;
      "set after remove" >:: set_after_remove;
      "allocate doesn't affect tail after archive 0" >:: allocate_doesn't_affect_beyond_end_of_archive 0;
      "allocate doesn't affect tail after archive 1" >:: allocate_doesn't_affect_beyond_end_of_archive 1;
      "allocate doesn't affect tail after archive 1 sector" >:: allocate_doesn't_affect_beyond_end_of_archive (B.sector_size - 512);
    ]
end

module Test512 = Test(Block512)
module Test4096 = Test(Block4096)

let () =
  Lwt_main.run @@ Alcotest_lwt.run "tar-write" [
    "Test512", Test512.tests;
    "Test4096", Test4096.tests;
  ]
