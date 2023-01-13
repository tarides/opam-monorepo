module Normalized = Duniverse_lib.Uri_utils.Normalized

let test_canonical_uri =
  let make_test ~supplied:(a, b) ~expected =
    let a = Uri.of_string a in
    let a' = Normalized.of_uri a in
    let b = Uri.of_string b in
    let b' = Normalized.of_uri b in
    let test_name = Fmt.str "Comparing %a and %a" Uri.pp a Uri.pp b in
    let test_fun () =
      let actual = Normalized.equal a' b' in
      Alcotest.(check bool) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test
      ~supplied:
        ( "git+https://github.com/mirage/mirage-clock.git",
          "git+https://github.com/mirage/mirage-clock.git" )
      ~expected:true;
    make_test
      ~supplied:
        ( "git://github.com/mirage/mirage-clock.git",
          "git+https://github.com/mirage/mirage-clock.git" )
      ~expected:true;
    make_test
      ~supplied:
        ( "https://github.com/mirage/mirage-clock.git",
          "git+https://github.com/mirage/mirage-clock.git" )
      ~expected:true;
    make_test
      ~supplied:
        ( "git+https://github.com/mirage/mirage-clock.git#master",
          "git+https://github.com/mirage/mirage-clock.git" )
      ~expected:true;
    make_test
      ~supplied:
        ( "git+https://github.com/mirage/mirage-clock",
          "git+https://github.com/mirage/mirage-clock.git" )
      ~expected:true;
    make_test
      ~supplied:
        ( "git+https://github.com/mirage/mirage-foo.git",
          "git+https://github.com/mirage/mirage-bar.git" )
      ~expected:false;
    make_test
      ~supplied:
        ( "git+https://github.com/mirage/mirage.git",
          "git+https://github.com/anchorage/mirage.git" )
      ~expected:false;
  ]

let suite = ("Uri_utils", test_canonical_uri)
