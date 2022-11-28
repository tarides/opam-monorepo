let test_repo_name =
  let make_test ~dev_repo ~expected () =
    let test_name = Printf.sprintf "repo_name: %s" dev_repo in
    let test_fun () =
      let actual = Duniverse_lib.Dev_repo.(repo_name (from_string dev_repo)) in
      Alcotest.(check (result string Testable.r_msg)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~dev_repo:"git://github.com/ocamllabs/opam-monorepo"
      ~expected:(Ok "opam-monorepo") ();
    make_test ~dev_repo:"git://github.com/ocamllabs/opam-monorepo.git"
      ~expected:(Ok "opam-monorepo") ();
    make_test ~dev_repo:"git+https://github.com/ocamllabs/opam-monorepo.git"
      ~expected:(Ok "opam-monorepo") ();
    make_test ~dev_repo:"git+https://github.com/ocamllabs/opam-monorepo/"
      ~expected:(Ok "opam-monorepo") ();
    make_test ~dev_repo:"git+https://github.com/ocamllabs/opam-monorepo.1/"
      ~expected:(Ok "opam-monorepo") ();
  ]
  |> List.append
       (let invalid_repos =
          [
            "";
            ".";
            "/";
            "./.";
            "...";
            "///";
            "https://github.com";
            "https://github.com/";
          ]
        in
        let make_test_error dev_repo =
          make_test ~dev_repo
            ~expected:
              (Rresult.R.error_msgf
                 "unexpected empty string while computing name for dev_repo: \
                  \"%s\""
                 dev_repo)
            ()
        in
        List.map make_test_error invalid_repos)

let suite = ("Dev_repo", List.concat [ test_repo_name ])
