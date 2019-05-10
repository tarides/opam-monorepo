let test_from_archive_url =
  let open Duniverse_lib.Git_ref in
  let make_test ~archive ~expected =
    let test_name = "tag_from_archive: " ^ archive in
    let test_fun () =
      let actual = Duniverse_lib.Git_ref.from_archive_url archive in
      Alcotest.(check (option Testable.git_ref)) archive expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test ~archive:"git+http://a.com/user/repo" ~expected:(Some (make "master"));
    make_test ~archive:"git+https://a.com/user/repo" ~expected:(Some (make "master"));
    make_test ~archive:"git+https://a.com/user/repo#v1.2.3" ~expected:(Some (make "v1.2.3"));
    make_test ~archive:"git+https://github.com/user/repo#v1.2.3" ~expected:(Some (make "v1.2.3"));
    make_test ~archive:"git+ssh://a.com/user/repo#v1.2.3" ~expected:(Some (make "v1.2.3"));
    make_test ~archive:"git+file://a.com/user/repo/something" ~expected:None;
    make_test ~archive:"https://github.com/user/repo/releases/download/v1.2.3/archive.tbz"
      ~expected:(Some (make "v1.2.3"));
    make_test ~archive:"https://github.com/user/repo/archive/v1.2.3.tbz" ~expected:(Some (make "v1.2.3"));
    make_test ~archive:"https://github.com/user/repo/archive/v1.2.3/archive.tbz"
      ~expected:(Some (make "v1.2.3"));
    make_test ~archive:"https://ocaml.janestreet.com/ocaml-core/4.07.1/files/package-v1.2.3.tbz"
      ~expected:(Some (make "v1.2.3"));
    make_test
      ~archive:"https://ocaml.janestreet.com/janestreet/repo/releases/download/v1.2.3/file.tbz"
      ~expected:(Some (make "v1.2.3"));
    make_test ~archive:"https://ocaml.janestreet.com/janestreet/repo/archive/v1.2.3.tbz"
      ~expected:(Some (make "v1.2.3"));
    make_test ~archive:"https://ocaml.janestreet.com/janestreet/malformed" ~expected:None;
    make_test ~archive:"https://gitlab.camlcity.org/some/path/file-v1.2.3.tbz"
      ~expected:(Some (make "v1.2.3"));
    make_test ~archive:"https://download.camlcity.org/some/path/file-v1.2.3.tbz"
      ~expected:(Some (make "v1.2.3"));
    make_test ~archive:"https://ocamlgraph.lri.fr/some/path/file-1.2.3.tbz"
      ~expected:(Some (make "v1.2.3"));
    make_test ~archive:"https://erratique.ch/some/path/file-1.2.3.tbz" ~expected:(Some (make "v1.2.3"));
    make_test ~archive:"https://other.domain.com/some/path/file-v1.2.3.tbz"
      ~expected:(Some (make "v1.2.3"))
  ]
