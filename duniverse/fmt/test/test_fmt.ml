(*---------------------------------------------------------------------------
   Copyright (c) 2015 The fmt programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let str pp v = Format.asprintf "%a" pp v
let snap s = Snap.string s

let test_dump_uchar =
  Test.test "Fmt.Dump.uchar" @@ fun () ->
  let str = str Fmt.Dump.uchar in
  snap (str Uchar.min) @@ __POS_OF__"U+0000";
  snap (str Uchar.(succ min)) @@ __POS_OF__"U+0001";
  snap (str Uchar.(of_int 0xFFFF)) @@ __POS_OF__"U+FFFF";
  snap (str Uchar.(succ (of_int 0xFFFF))) @@ __POS_OF__ "U+10000";
  snap (str Uchar.(pred max)) @@ __POS_OF__ "U+10FFFE";
  snap (str Uchar.max) @@ __POS_OF__ "U+10FFFF";
  ()

let test_utf_8 =
  Test.test "Fmt.utf_8" @@ fun () ->
  let ppf = Format.formatter_of_buffer (Buffer.create 23) in
  Test.bool (Fmt.utf_8 ppf) true;
  Fmt.set_utf_8 ppf false;
  Test.bool (Fmt.utf_8 ppf) false;
  Fmt.set_utf_8 ppf true;
  Test.bool (Fmt.utf_8 ppf) true;
  ()

let test_style_renderer =
  Test.test "Fmt.style_renderer" @@ fun () ->
  let ppf = Format.formatter_of_buffer (Buffer.create 23) in
  assert (Fmt.style_renderer ppf = `None);
  Fmt.set_style_renderer ppf `Ansi_tty;
  assert (Fmt.style_renderer ppf = `Ansi_tty);
  Fmt.set_style_renderer ppf `None;
  assert (Fmt.style_renderer ppf = `None);
  ()

let test_exn_typechecks =
  Test.test "Fmt.{failwith,invalid_arg} types" @@ fun () ->
  let (_ : bool) = true || Fmt.failwith "%s" "" in
  let (_ : bool) = true || Fmt.invalid_arg "%s" "" in
  ()

let test_kstr_str_like_partial_app =
  Test.test "Fmt.{kstr,str_like} partial application" @@ fun () ->
  let assertf f = assert (f "X" = f "X") in
  let test_kstrf fmt = Fmt.kstr (fun x -> x) fmt in
  let test_strf_like fmt = Fmt.str_like Fmt.stderr fmt in
  assertf (test_strf_like "%s");
  assertf (test_kstrf "%s");
  ()

let test_cardinal =
  Test.test "Fmt.cardinal" @@ fun () ->
  let item = Fmt.cardinal ~one:(fun ppf n -> Fmt.pf ppf "%d item" n) () in
  let item n = Fmt.str "%a" item n in
  let children =
    let zero = Fmt.any "are no children" in
    let one = Fmt.any "is a child" in
    let other = Fmt.any "are children" in
    Fmt.cardinal ~zero ~one ~other ()
  in
  let children n = Fmt.str "There %a." children n in
  Snap.string (item 0) @@ __POS_OF__ "0 items";
  Snap.string (item 1) @@ __POS_OF__ "1 item";
  Snap.string (item 3) @@ __POS_OF__ "3 items";
  Snap.string (children 0) @@ __POS_OF__ "There are no children.";
  Snap.string (children 1) @@ __POS_OF__ "There is a child.";
  Snap.string (children 67) @@ __POS_OF__ "There are children.";
  ()

let test_ordinal =
  Test.test "Fmt.ordinal" @@ fun () ->
  let ord = Fmt.ordinal () in
  let ord n = Fmt.str "%a" ord n in
  Snap.string (ord 0) @@ __POS_OF__ "0th";
  Snap.string (ord 1) @@ __POS_OF__ "1st";
  Snap.string (ord 2) @@ __POS_OF__ "2nd";
  Snap.string (ord 3) @@ __POS_OF__ "3rd";
  Snap.string (ord 4) @@ __POS_OF__ "4th";
  Snap.string (ord 11) @@ __POS_OF__ "11th";
  Snap.string (ord 12) @@ __POS_OF__ "12th";
  Snap.string (ord 21) @@ __POS_OF__ "21st";
  Snap.string (ord 22) @@ __POS_OF__ "22nd";
  Snap.string (ord 66) @@ __POS_OF__ "66th";
  Snap.string (ord 101) @@ __POS_OF__ "101st";
  Snap.string (ord 111) @@ __POS_OF__ "111th";
  ()

let test_byte_size =
  Test.test "Fmt.byte_size" @@ fun () ->
  let size = str Fmt.byte_size in
  snap (size 0) @@ __POS_OF__ "0B";
  snap (size 999) @@ __POS_OF__ "999B";
  snap (size 1000) @@ __POS_OF__ "1kB";
  snap (size 1001) @@ __POS_OF__ "1.01kB";
  snap (size 1010) @@ __POS_OF__ "1.01kB";
  snap (size 1011) @@ __POS_OF__ "1.02kB";
  snap (size 1020) @@ __POS_OF__ "1.02kB";
  snap (size 1100) @@ __POS_OF__ "1.1kB";
  snap (size 1101) @@ __POS_OF__ "1.11kB";
  snap (size 1109) @@ __POS_OF__ "1.11kB";
  snap (size 1111) @@ __POS_OF__ "1.12kB";
  snap (size 1119) @@ __POS_OF__ "1.12kB";
  snap (size 1120) @@ __POS_OF__ "1.12kB";
  snap (size 1121) @@ __POS_OF__ "1.13kB";
  snap (size 9990) @@ __POS_OF__ "9.99kB";
  snap (size 9991) @@ __POS_OF__ "10kB";
  snap (size 9999) @@ __POS_OF__ "10kB";
  snap (size 10_000) @@ __POS_OF__ "10kB";
  snap (size 10_001) @@ __POS_OF__ "10.1kB";
  snap (size 10_002) @@ __POS_OF__ "10.1kB";
  snap (size 10_099) @@ __POS_OF__ "10.1kB";
  snap (size 10_100) @@ __POS_OF__ "10.1kB";
  snap (size 10_100) @@ __POS_OF__ "10.1kB";
  snap (size 10_101) @@ __POS_OF__ "10.2kB";
  snap (size 10_199) @@ __POS_OF__ "10.2kB";
  snap (size 10_199) @@ __POS_OF__ "10.2kB";
  snap (size 10_200) @@ __POS_OF__ "10.2kB";
  snap (size 10_201) @@ __POS_OF__ "10.3kB";
  snap (size 99_901) @@ __POS_OF__ "100kB";
  snap (size 99_999) @@ __POS_OF__ "100kB";
  snap (size 100_000) @@ __POS_OF__ "100kB";
  snap (size 100_001) @@ __POS_OF__ "101kB";
  snap (size 100_999) @@ __POS_OF__ "101kB";
  snap (size 101_000) @@ __POS_OF__ "101kB";
  snap (size 101_001) @@ __POS_OF__ "102kB";
  snap (size 999_000) @@ __POS_OF__ "999kB";
  snap (size 999_001) @@ __POS_OF__ "1MB";
  snap (size 999_999) @@ __POS_OF__ "1MB";
  snap (size 1_000_000) @@ __POS_OF__ "1MB";
  snap (size 1_000_001) @@ __POS_OF__ "1.01MB";
  snap (size 1_009_999) @@ __POS_OF__ "1.01MB";
  snap (size 1_010_000) @@ __POS_OF__ "1.01MB";
  snap (size 1_010_001) @@ __POS_OF__ "1.02MB";
  snap (size 1_019_999) @@ __POS_OF__ "1.02MB";
  snap (size 1_020_000) @@ __POS_OF__ "1.02MB";
  snap (size 1_020_001) @@ __POS_OF__ "1.03MB";
  snap (size 1_990_000) @@ __POS_OF__ "1.99MB";
  snap (size 1_990_001) @@ __POS_OF__ "2MB";
  snap (size 1_999_999) @@ __POS_OF__ "2MB";
  snap (size 2_000_000) @@ __POS_OF__ "2MB";
  snap (size 9_990_000) @@ __POS_OF__ "9.99MB";
  snap (size 9_990_001) @@ __POS_OF__ "10MB";
  snap (size 9_990_999) @@ __POS_OF__ "10MB";
  snap (size 10_000_000) @@ __POS_OF__ "10MB";
  snap (size 10_000_001) @@ __POS_OF__ "10.1MB";
  snap (size 10_099_999) @@ __POS_OF__ "10.1MB";
  snap (size 10_100_000) @@ __POS_OF__ "10.1MB";
  snap (size 10_900_001) @@ __POS_OF__ "11MB";
  snap (size 10_999_999) @@ __POS_OF__ "11MB";
  snap (size 11_000_000) @@ __POS_OF__ "11MB";
  snap (size 11_000_001) @@ __POS_OF__ "11.1MB";
  snap (size 99_900_000) @@ __POS_OF__ "99.9MB";
  snap (size 99_900_001) @@ __POS_OF__ "100MB";
  snap (size 99_999_999) @@ __POS_OF__ "100MB";
  snap (size 100_000_000) @@ __POS_OF__ "100MB";
  snap (size 100_000_001) @@ __POS_OF__ "101MB";
  snap (size 100_999_999) @@ __POS_OF__ "101MB";
  snap (size 101_000_000) @@ __POS_OF__ "101MB";
  snap (size 101_000_000) @@ __POS_OF__ "101MB";
  snap (size 999_000_000) @@ __POS_OF__ "999MB";
  snap (size 999_000_001) @@ __POS_OF__ "1GB";
  snap (size 999_999_999) @@ __POS_OF__ "1GB";
  snap (size 1_000_000_000) @@ __POS_OF__ "1GB";
  snap (size 1_000_000_001) @@ __POS_OF__ "1.01GB";
  snap (size 1_000_000_001) @@ __POS_OF__ "1.01GB";
  ()

let test_uint64_ns_span =
  Test.test "Fmt.uint64_ns_span" @@ fun () ->
  let span s = Fmt.str "%a" Fmt.uint64_ns_span (Int64.of_string s) in
  snap (span "0u0") @@ __POS_OF__ "0ns";
  snap (span "0u999") @@ __POS_OF__ "999ns";
  snap (span "0u1_000") @@ __POS_OF__ "1us";
  snap (span "0u1_001") @@ __POS_OF__ "1.01us";
  snap (span "0u1_009") @@ __POS_OF__ "1.01us";
  snap (span "0u1_010") @@ __POS_OF__ "1.01us";
  snap (span "0u1_011") @@ __POS_OF__ "1.02us";
  snap (span "0u1_090") @@ __POS_OF__ "1.09us";
  snap (span "0u1_091") @@ __POS_OF__ "1.1us";
  snap (span "0u1_100") @@ __POS_OF__ "1.1us";
  snap (span "0u1_101") @@ __POS_OF__ "1.11us";
  snap (span "0u1_109") @@ __POS_OF__ "1.11us";
  snap (span "0u1_110") @@ __POS_OF__ "1.11us";
  snap (span "0u1_111") @@ __POS_OF__ "1.12us";
  snap (span "0u1_990") @@ __POS_OF__ "1.99us";
  snap (span "0u1_991") @@ __POS_OF__ "2us";
  snap (span "0u1_999") @@ __POS_OF__ "2us";
  snap (span "0u2_000") @@ __POS_OF__ "2us";
  snap (span "0u2_001") @@ __POS_OF__ "2.01us";
  snap (span "0u9_990") @@ __POS_OF__ "9.99us";
  snap (span "0u9_991") @@ __POS_OF__ "10us";
  snap (span "0u9_999") @@ __POS_OF__ "10us";
  snap (span "0u10_000") @@ __POS_OF__ "10us";
  snap (span "0u10_001") @@ __POS_OF__ "10.1us";
  snap (span "0u10_099") @@ __POS_OF__ "10.1us";
  snap (span "0u10_100") @@ __POS_OF__ "10.1us";
  snap (span "0u10_101") @@ __POS_OF__ "10.2us";
  snap (span "0u10_900") @@ __POS_OF__ "10.9us";
  snap (span "0u10_901") @@ __POS_OF__ "11us";
  snap (span "0u10_999") @@ __POS_OF__ "11us";
  snap (span "0u11_000") @@ __POS_OF__ "11us";
  snap (span "0u11_001") @@ __POS_OF__ "11.1us";
  snap (span "0u11_099") @@ __POS_OF__ "11.1us";
  snap (span "0u11_100") @@ __POS_OF__ "11.1us";
  snap (span "0u11_101") @@ __POS_OF__ "11.2us";
  snap (span "0u99_900") @@ __POS_OF__ "99.9us";
  snap (span "0u99_901") @@ __POS_OF__ "100us";
  snap (span "0u99_999") @@ __POS_OF__ "100us";
  snap (span "0u100_000") @@ __POS_OF__ "100us";
  snap (span "0u100_001") @@ __POS_OF__ "101us";
  snap (span "0u100_999") @@ __POS_OF__ "101us";
  snap (span "0u101_000") @@ __POS_OF__ "101us";
  snap (span "0u101_001") @@ __POS_OF__ "102us";
  snap (span "0u101_999") @@ __POS_OF__ "102us";
  snap (span "0u102_000") @@ __POS_OF__ "102us";
  snap (span "0u999_000") @@ __POS_OF__ "999us";
  snap (span "0u999_001") @@ __POS_OF__ "1ms";
  snap (span "0u999_001") @@ __POS_OF__ "1ms";
  snap (span "0u999_999") @@ __POS_OF__ "1ms";
  snap (span "0u1_000_000") @@ __POS_OF__ "1ms";
  snap (span "0u1_000_001") @@ __POS_OF__ "1.01ms";
  snap (span "0u1_009_999") @@ __POS_OF__ "1.01ms";
  snap (span "0u1_010_000") @@ __POS_OF__ "1.01ms";
  snap (span "0u1_010_001") @@ __POS_OF__ "1.02ms";
  snap (span "0u9_990_000") @@ __POS_OF__ "9.99ms";
  snap (span "0u9_990_001") @@ __POS_OF__ "10ms";
  snap (span "0u9_999_999") @@ __POS_OF__ "10ms";
  snap (span "0u10_000_000") @@ __POS_OF__ "10ms";
  snap (span "0u10_000_001") @@ __POS_OF__ "10.1ms";
  snap (span "0u10_000_001") @@ __POS_OF__ "10.1ms";
  snap (span "0u10_099_999") @@ __POS_OF__ "10.1ms";
  snap (span "0u10_100_000") @@ __POS_OF__ "10.1ms";
  snap (span "0u10_100_001") @@ __POS_OF__ "10.2ms";
  snap (span "0u99_900_000") @@ __POS_OF__ "99.9ms";
  snap (span "0u99_900_001") @@ __POS_OF__ "100ms";
  snap (span "0u99_999_999") @@ __POS_OF__ "100ms";
  snap (span "0u100_000_000") @@ __POS_OF__ "100ms";
  snap (span "0u100_000_001") @@ __POS_OF__ "101ms";
  snap (span "0u100_999_999") @@ __POS_OF__ "101ms";
  snap (span "0u101_000_000") @@ __POS_OF__ "101ms";
  snap (span "0u101_000_001") @@ __POS_OF__ "102ms";
  snap (span "0u999_000_000") @@ __POS_OF__ "999ms";
  snap (span "0u999_000_001") @@ __POS_OF__ "1s";
  snap (span "0u999_999_999") @@ __POS_OF__ "1s";
  snap (span "0u1_000_000_000") @@ __POS_OF__ "1s";
  snap (span "0u1_000_000_001") @@ __POS_OF__ "1.01s";
  snap (span "0u1_009_999_999") @@ __POS_OF__ "1.01s";
  snap (span "0u1_010_000_000") @@ __POS_OF__ "1.01s";
  snap (span "0u1_010_000_001") @@ __POS_OF__ "1.02s";
  snap (span "0u1_990_000_000") @@ __POS_OF__ "1.99s";
  snap (span "0u1_990_000_001") @@ __POS_OF__ "2s";
  snap (span "0u1_999_999_999") @@ __POS_OF__ "2s";
  snap (span "0u2_000_000_000") @@ __POS_OF__ "2s";
  snap (span "0u2_000_000_001") @@ __POS_OF__ "2.01s";
  snap (span "0u9_990_000_000") @@ __POS_OF__ "9.99s";
  snap (span "0u9_999_999_999") @@ __POS_OF__ "10s";
  snap (span "0u10_000_000_000") @@ __POS_OF__ "10s";
  snap (span "0u10_000_000_001") @@ __POS_OF__ "10.1s";
  snap (span "0u10_099_999_999") @@ __POS_OF__ "10.1s";
  snap (span "0u10_100_000_000") @@ __POS_OF__ "10.1s";
  snap (span "0u10_100_000_001") @@ __POS_OF__ "10.2s";
  snap (span "0u59_900_000_000") @@ __POS_OF__ "59.9s";
  snap (span "0u59_900_000_001") @@ __POS_OF__ "1min";
  snap (span "0u59_999_999_999") @@ __POS_OF__ "1min";
  snap (span "0u60_000_000_000") @@ __POS_OF__ "1min";
  snap (span "0u60_000_000_001") @@ __POS_OF__ "1min1s";
  snap (span "0u60_999_999_999") @@ __POS_OF__ "1min1s";
  snap (span "0u61_000_000_000") @@ __POS_OF__ "1min1s";
  snap (span "0u61_000_000_001") @@ __POS_OF__ "1min2s";
  snap (span "0u119_000_000_000") @@ __POS_OF__ "1min59s";
  snap (span "0u119_000_000_001") @@ __POS_OF__ "2min";
  snap (span "0u119_999_999_999") @@ __POS_OF__ "2min";
  snap (span "0u120_000_000_000") @@ __POS_OF__ "2min";
  snap (span "0u120_000_000_001") @@ __POS_OF__ "2min1s";
  snap (span "0u3599_000_000_000") @@ __POS_OF__ "59min59s";
  snap (span "0u3599_000_000_001") @@ __POS_OF__ "1h";
  snap (span "0u3599_999_999_999") @@ __POS_OF__ "1h";
  snap (span "0u3600_000_000_000") @@ __POS_OF__ "1h";
  snap (span "0u3600_000_000_001") @@ __POS_OF__ "1h1min";
  snap (span "0u3659_000_000_000") @@ __POS_OF__ "1h1min";
  snap (span "0u3659_000_000_001") @@ __POS_OF__ "1h1min";
  snap (span "0u3659_999_999_999") @@ __POS_OF__ "1h1min";
  snap (span "0u3660_000_000_000") @@ __POS_OF__ "1h1min";
  snap (span "0u3660_000_000_001") @@ __POS_OF__ "1h2min";
  snap (span "0u3660_000_000_001") @@ __POS_OF__ "1h2min";
  snap (span "0u3660_000_000_001") @@ __POS_OF__ "1h2min";
  snap (span "0u3720_000_000_000") @@ __POS_OF__ "1h2min";
  snap (span "0u3720_000_000_001") @@ __POS_OF__ "1h3min";
  snap (span "0u7140_000_000_000") @@ __POS_OF__ "1h59min";
  snap (span "0u7140_000_000_001") @@ __POS_OF__ "2h";
  snap (span "0u7199_999_999_999") @@ __POS_OF__ "2h";
  snap (span "0u7200_000_000_000") @@ __POS_OF__ "2h";
  snap (span "0u7200_000_000_001") @@ __POS_OF__ "2h1min";
  snap (span "0u86340_000_000_000") @@ __POS_OF__ "23h59min";
  snap (span "0u86340_000_000_001") @@ __POS_OF__ "1d";
  snap (span "0u86400_000_000_000") @@ __POS_OF__ "1d";
  snap (span "0u86400_000_000_001") @@ __POS_OF__ "1d1h";
  snap (span "0u89999_999_999_999") @@ __POS_OF__ "1d1h";
  snap (span "0u90000_000_000_000") @@ __POS_OF__ "1d1h";
  snap (span "0u90000_000_000_001") @@ __POS_OF__ "1d2h";
  snap (span "0u169200_000_000_000") @@ __POS_OF__ "1d23h";
  snap (span "0u169200_000_000_001") @@ __POS_OF__ "2d";
  snap (span "0u169200_000_000_001") @@ __POS_OF__ "2d";
  snap (span "0u172799_999_999_999") @@ __POS_OF__ "2d";
  snap (span "0u172800_000_000_000") @@ __POS_OF__ "2d";
  snap (span "0u172800_000_000_001") @@ __POS_OF__ "2d1h";
  snap (span "0u31536000_000_000_000") @@ __POS_OF__ "365d";
  snap (span "0u31554000_000_000_000") @@ __POS_OF__ "365d5h";
  snap (
    (* Technically this should round to a year but it does get rendered.
       I don't think it matters, it's not inacurate per se. *)
    span "0u31554000_000_000_001") @@ __POS_OF__ "365d6h";
  snap (span "0u31557600_000_000_000") @@ __POS_OF__ "1a";
  snap (span "0u31557600_000_000_001") @@ __POS_OF__ "1a1d";
  snap (span "0u63028800_000_000_000") @@ __POS_OF__ "1a365d";
  snap (span "0u63093600_000_000_000") @@ __POS_OF__ "1a365d";
  snap (span "0u63093600_000_000_001") @@ __POS_OF__ "2a";
  snap (span "0u63115200_000_000_000") @@ __POS_OF__ "2a";
  snap (span "0u63115200_000_000_001") @@ __POS_OF__ "2a1d";
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
