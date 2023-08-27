let universe =
  List.map OpamFile.OPAM.read_from_string
    [
      {|
opam-version: "2.0"
name: "ocaml-base-compiler"
version: "3.14"
|};
      {|
opam-version: "2.0"
name: "p1"
version: "1"
|};
      {|
opam-version: "2.0"
name: "p1"
version: "2"
|};
      {|
opam-version: "2.0"
name: "p2"
version: "1"
|};
    ]

let root =
  OpamFile.OPAM.read_from_string
    {|
opam-version: "2.0"
name: "root"
version: "0"
depends: ["p1" "p2"]
|}

let calculate universe root expected =
  let preferred_versions = OpamPackage.Name.Map.empty in
  let local_opam_files = OpamPackage.Name.Map.empty in
  let target_packages =
    OpamPackage.Name.Set.singleton (OpamFile.OPAM.name root)
  in
  let opam_provided = OpamPackage.Name.Set.empty in
  let pin_depends = OpamPackage.Name.Map.empty in
  let opam_env = Stdext.String.Map.empty in
  let pkgs = root :: universe in
  let solver = Duniverse_lib.Opam_solve.mock_solver in
  match
    Duniverse_lib.Opam_solve.calculate ~build_only:false ~allow_jbuilder:false
      ~require_cross_compile:false ~preferred_versions ~local_opam_files
      ~target_packages ~opam_provided ~pin_depends solver (opam_env, pkgs)
  with
  | Ok es ->
      let es =
        List.map
          (fun es ->
            let pkg =
              es.Duniverse_lib.Opam.Dependency_entry.package_summary.package
            in
            ( OpamPackage.(Name.to_string (name pkg)),
              OpamPackage.(Version.to_string (version pkg)) ))
          es
      in
      Alcotest.(check (slist (pair string string) compare)) __LOC__ expected es
  | Error (`Diagnostics d) ->
      let (`Msg e) =
        Duniverse_lib.Opam_solve.diagnostics_message ~verbose:false solver d
      in
      Alcotest.failf "error: diagnostic %s" e
  | Error (`Msg e) -> Alcotest.fail e

let simple () =
  calculate universe root
    [ ("ocaml-base-compiler", "3.14"); ("p1", "2"); ("p2", "1"); ("root", "0") ]

let suite = ("solve", [ Alcotest.test_case "simple" `Quick simple ])
