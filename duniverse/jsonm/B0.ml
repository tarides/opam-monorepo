open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let unix = B0_ocaml.libname "unix"
let uutf = B0_ocaml.libname "uutf"
let jsonm = B0_ocaml.libname "jsonm"

(* Libraries *)

let jsonm_lib =
  let srcs = Fpath.[`Dir (v "src") ] in
  let requires = [uutf] in
  B0_ocaml.lib jsonm ~doc:"The jsonm library" ~srcs ~requires

(* Tests *)

let jsontrip =
  let srcs = Fpath.[`File (v "test/jsontrip.ml")] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ unix; uutf; jsonm ] in
  B0_ocaml.exe "jsontrip" ~srcs ~meta ~requires

let test =
  let srcs = Fpath.[`File (v "test/test.ml")] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ uutf; jsonm ] in
  B0_ocaml.exe "test" ~srcs ~meta ~requires

let jtree =
  let srcs = Fpath.[`File (v "test/jtree.ml")] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ jsonm ] in
  B0_ocaml.exe "jtree" ~srcs ~meta ~requires

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> tag B0_opam.tag
    |> add authors ["The jsonm programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/jsonm"
    |> add online_doc "https://erratique.ch/software/jsonm/doc/"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/jsonm.git"
    |> add issues "https://github.com/dbuenzli/jsonm/issues"
    |> add description_tags
      ["json"; "codec"; "org:erratique"]
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.05.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "uutf", {|> "1.0.0" |};
      ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
  in
  B0_pack.v "default" ~doc:"jsonm package" ~meta ~locked:true @@
  B0_unit.list ()
