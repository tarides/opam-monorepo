module Uri = struct
  include Uri

  let testable = Alcotest.testable pp equal
end

let test_canonical_uri =
  let make_test ~name ~supplied ~expected =
    let supplied = Uri.of_string supplied in
    let expected = Uri.of_string expected in
    let test_name = Fmt.str "canonicizing: %s" name in
    let test_fun () =
      let actual = Duniverse_lib.Uri_utils.canonicalize supplied in
      Alcotest.(check Uri.testable) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~name:"no-op"
      ~supplied:"git+https://github.com/mirage/mirage-clock.git"
      ~expected:"git+https://github.com/mirage/mirage-clock.git";
    make_test ~name:"scheme: git"
      ~supplied:"git://github.com/mirage/mirage-clock.git"
      ~expected:"git+https://github.com/mirage/mirage-clock.git";
    make_test ~name:"scheme: https"
      ~supplied:"https://github.com/mirage/mirage-clock.git"
      ~expected:"git+https://github.com/mirage/mirage-clock.git";
    make_test ~name:"hash"
      ~supplied:"git+https://github.com/mirage/mirage-clock.git#master"
      ~expected:"git+https://github.com/mirage/mirage-clock.git";
    make_test ~name:".git suffix"
      ~supplied:"git+https://github.com/mirage/mirage-clock"
      ~expected:"git+https://github.com/mirage/mirage-clock.git";
  ]

let suite = ("Uri_utils", test_canonical_uri)
