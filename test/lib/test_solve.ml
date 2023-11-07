let calculate universe ?(pins = []) root expected =
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
      ~target_packages ~opam_provided ~pin_depends solver (opam_env, pkgs, pins)
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
  in
  let root =
    OpamFile.OPAM.read_from_string
      {|
opam-version: "2.0"
name: "root"
version: "0"
depends: ["p1" "p2"]
|}
  in
  calculate universe root
    [ ("ocaml-base-compiler", "3.14"); ("p1", "2"); ("p2", "1"); ("root", "0") ]

let conflicts () =
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
conflicts: ["p2" {= "1"}]
|};
        {|
opam-version: "2.0"
name: "p2"
version: "1"
|};
      ]
  in
  let root =
    OpamFile.OPAM.read_from_string
      {|
opam-version: "2.0"
name: "root"
version: "0"
depends: ["p1" "p2"]
|}
  in
  calculate universe root
    [ ("ocaml-base-compiler", "3.14"); ("p1", "1"); ("p2", "1"); ("root", "0") ]

let conflict_class () =
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
conflict-class: ["x"]
|};
        {|
opam-version: "2.0"
name: "p1"
version: "2"
conflict-class: ["y"]
|};
        {|
opam-version: "2.0"
name: "p2"
version: "1"
conflict-class: ["y"]
|};
      ]
  in
  let root =
    OpamFile.OPAM.read_from_string
      {|
opam-version: "2.0"
name: "root"
version: "0"
depends: ["p1" "p2"]
|}
  in
  calculate universe root
    [ ("ocaml-base-compiler", "3.14"); ("p1", "1"); ("p2", "1"); ("root", "0") ]

let universe_with_url =
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
dev-repo: "x"
url {
  src: "https://p.com/p.tbz"
  checksum: "sha256=0000000000000000000000000000000000000000000000000000000000000001"
}
|};
      {|
opam-version: "2.0"
name: "p1"
version: "2"
dev-repo: "x"
url {
  src: "https://p.com/p.tbz"
  checksum: "sha256=0000000000000000000000000000000000000000000000000000000000000002"
}
|};
      {|
opam-version: "2.0"
name: "p2"
version: "1"
dev-repo: "x"
url {
  src: "https://p.com/p.tbz"
  checksum: "sha256=0000000000000000000000000000000000000000000000000000000000000001"
}
|};
      {|
opam-version: "2.0"
name: "p2"
version: "2"
dev-repo: "x"
url {
  src: "https://p.com/p.tbz"
  checksum: "sha256=0000000000000000000000000000000000000000000000000000000000000002"
}
|};
    ]

let conflict_url () =
  let universe = universe_with_url in
  let root =
    OpamFile.OPAM.read_from_string
      {|
opam-version: "2.0"
name: "root"
version: "0"
depends: ["p1" {= "1"} "p2"]
|}
  in
  calculate universe root
    [ ("ocaml-base-compiler", "3.14"); ("p1", "1"); ("p2", "1"); ("root", "0") ]

let no_conflict_with_pin () =
  let p2_dev =
    OpamFile.OPAM.read_from_string
      {|
opam-version: "2.0"
name: "p2"
version: "0"
dev-repo: "x"
url {
  src: "git+https://x#hash"
}
|}
  in
  let universe = p2_dev :: universe_with_url in
  let root =
    OpamFile.OPAM.read_from_string
      {|
opam-version: "2.0"
name: "root"
version: "0"
depends: ["p1" "p2"]
|}
  in
  calculate
    ~pins:[ OpamPackage.of_string "p2.0" ]
    universe root
    [ ("ocaml-base-compiler", "3.14"); ("p1", "2"); ("p2", "0"); ("root", "0") ]

let suite =
  ( "solve",
    [
      Alcotest.test_case "simple" `Quick simple;
      Alcotest.test_case "conflicts" `Quick conflicts;
      Alcotest.test_case "conflict_class" `Quick conflict_class;
      Alcotest.test_case "conflict_url" `Quick conflict_url;
      Alcotest.test_case "no_conflict_with_pin" `Quick no_conflict_with_pin;
    ] )
