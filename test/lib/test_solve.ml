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

let opam_file stanzas =
  let opam_stanzas = {|opam-version: "2.0"|} :: stanzas in
  let pp_stanzas = Fmt.list ~sep:(Fmt.any "\n") Fmt.string in
  let opam_string = Format.asprintf "%a" pp_stanzas opam_stanzas in
  OpamFile.OPAM.read_from_string opam_string

let quoted = Printf.sprintf "%s: %S"
let name = quoted "name"
let version = quoted "version"
let dev_repo = quoted "dev-repo"
let build = quoted "build"

let url ~src ?checksum () =
  match checksum with
  | Some checksum ->
      Printf.sprintf {|url {
  src: %S
  checksum: "sha256=%064d"
}|} src
        checksum
  | None -> Printf.sprintf {|url {
  src: %S
}|} src

let pp_list = Fmt.list ~sep:Fmt.sp Fmt.Dump.string
let depends pkgs = Format.asprintf "depends: [%a]" pp_list pkgs
let conflict_class cls = Format.asprintf "conflict-class: [%a]" pp_list cls

let ocaml_base_compiler, ocaml_base_expected =
  let n = "ocaml-base-compiler" in
  let v = "3.14" in
  (opam_file [ name n; version v ], (n, v))

let simple () =
  let universe =
    [
      ocaml_base_compiler;
      opam_file [ name "p1"; version "1" ];
      opam_file [ name "p1"; version "2" ];
      opam_file [ name "p2"; version "1" ];
    ]
  in
  let root = opam_file [ name "root"; version "0"; depends [ "p1"; "p2" ] ] in
  calculate universe root
    [ ocaml_base_expected; ("p1", "2"); ("p2", "1"); ("root", "0") ]

let conflicts () =
  let universe =
    [
      ocaml_base_compiler;
      opam_file [ name "p1"; version "1" ];
      opam_file [ name "p1"; version "2"; {|conflicts: ["p2" {= "1"}]|} ];
      opam_file [ name "p2"; version "1" ];
    ]
  in
  let root = opam_file [ name "root"; version "0"; depends [ "p1"; "p2" ] ] in
  calculate universe root
    [ ocaml_base_expected; ("p1", "1"); ("p2", "1"); ("root", "0") ]

let conflict_class () =
  let universe =
    [
      ocaml_base_compiler;
      opam_file [ name "p1"; version "1"; conflict_class [ "x" ] ];
      opam_file [ name "p1"; version "2"; conflict_class [ "y" ] ];
      opam_file [ name "p2"; version "1"; conflict_class [ "y" ] ];
    ]
  in
  let root = opam_file [ name "root"; version "0"; depends [ "p1"; "p2" ] ] in
  calculate universe root
    [ ocaml_base_expected; ("p1", "1"); ("p2", "1"); ("root", "0") ]

let universe_with_url ~virtual_packages =
  let extra =
    if virtual_packages then [] else [ build "[ [] ]"; depends [ "dune" ] ]
  in
  [
    ocaml_base_compiler;
    opam_file
      ([
         name "p1";
         version "1";
         dev_repo "x";
         url ~src:"https://p.com/p.tbz" ~checksum:1 ();
       ]
      @ extra);
    opam_file
      ([
         name "p1";
         version "2";
         dev_repo "x";
         url ~src:"https://p.com/p.tbz" ~checksum:2 ();
       ]
      @ extra);
    opam_file
      ([
         name "p2";
         version "1";
         dev_repo "x";
         url ~src:"https://p.com/p.tbz" ~checksum:1 ();
       ]
      @ extra);
    opam_file
      ([
         name "p2";
         version "2";
         dev_repo "x";
         url ~src:"https://p.com/p.tbz" ~checksum:2 ();
       ]
      @ extra);
    opam_file [ name "dune"; version "0" ];
  ]

let conflict_url () =
  let universe = universe_with_url ~virtual_packages:false in
  let root =
    opam_file [ name "root"; version "0"; {|depends: ["p1" {= "1"} "p2"] |} ]
  in
  calculate universe root
    [
      ocaml_base_expected; ("dune", "0"); ("p1", "1"); ("p2", "1"); ("root", "0");
    ]

let no_conflict_when_virtual () =
  let universe = universe_with_url ~virtual_packages:true in
  let root =
    opam_file [ name "root"; version "0"; {|depends: ["p1" {= "1"} "p2"] |} ]
  in
  calculate universe root
    [ ocaml_base_expected; ("p1", "1"); ("p2", "2"); ("root", "0") ]

let no_conflict_with_pin () =
  let p2_dev =
    opam_file
      [ name "p2"; version "0"; dev_repo "x"; url ~src:"git+https://x#hash" () ]
  in
  let universe = p2_dev :: universe_with_url ~virtual_packages:false in
  let root = opam_file [ name "root"; version "0"; depends [ "p1"; "p2" ] ] in
  calculate
    ~pins:[ OpamPackage.of_string "p2.0" ]
    universe root
    [
      ocaml_base_expected; ("dune", "0"); ("p1", "2"); ("p2", "0"); ("root", "0");
    ]

let suite =
  ( "solve",
    [
      Alcotest.test_case "simple" `Quick simple;
      Alcotest.test_case "conflicts" `Quick conflicts;
      Alcotest.test_case "conflict_class" `Quick conflict_class;
      Alcotest.test_case "conflict_url" `Quick conflict_url;
      Alcotest.test_case "no_conflict_when_virtual" `Quick
        no_conflict_when_virtual;
      Alcotest.test_case "no_conflict_with_pin" `Quick no_conflict_with_pin;
    ] )
