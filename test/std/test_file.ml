let test_strip_ext =
  let make_test ~path ~expected =
    let test_name = Format.sprintf "strip_ext: %s" path in
    let test_fun () =
      let actual = Duniverse_std.File.strip_ext path in
      Alcotest.(check string) path expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test ~path:"file.ext" ~expected:"file";
    make_test ~path:"file.tar.gz" ~expected:"file";
    make_test ~path:"path/to/file.ext" ~expected:"path/to/file"
  ]
