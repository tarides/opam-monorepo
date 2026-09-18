open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let b0_std = B0_ocaml.libname "b0.std"
let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"
let cmdliner = B0_ocaml.libname "cmdliner"
let unix = B0_ocaml.libname "unix"

let fmt = B0_ocaml.libname "fmt"
let fmt_cli = B0_ocaml.libname "fmt.cli"
let fmt_tty = B0_ocaml.libname "fmt.tty"
let fmt_top = B0_ocaml.libname "fmt.top"

(* Libraries *)

let fmt_lib =
  B0_ocaml.lib fmt ~srcs:[`Dir ~/"src"]

let fmt_cli =
  let srcs = [`Dir ~/"src/cli"] in
  B0_ocaml.lib fmt_cli ~srcs ~requires:[cmdliner; fmt] ~exports:[fmt]

let fmt_tty =
  let srcs = [`Dir ~/"src/tty"] in
  B0_ocaml.lib fmt_tty ~srcs ~requires:[unix; fmt] ~exports:[fmt]

let fmt_top =
  let srcs = [`Dir ~/"src/top"; `X ~/"src/top/fmt_tty_top_init.ml"] in
  B0_ocaml.lib fmt_top ~srcs ~requires:[compiler_libs_toplevel]

(* Tests *)

let test ?(requires = []) = B0_ocaml.test ~requires:(fmt :: requires)

let test_fmt = test ~/"test/test_fmt.ml" ~requires:[b0_std]
let styled_perf_bug =
  test ~/"test/styled_perf_bug.ml" ~requires:[unix] ~run:false

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The fmt programmers"]
    |> ~~ B0_meta.maintainers
       ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/fmt"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/fmt/doc/"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/fmt.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/fmt/issues"
    |> ~~ B0_meta.description_tags
      ["string"; "format"; "pretty-print"; "org:erratique"]
    |> B0_meta.tag B0_opam.tag
    |> ~~ B0_opam.depopts ["base-unix", ""; "cmdliner", ""]
    |> ~~ B0_opam.conflicts
      [ "cmdliner", {|< "1.3.0"|}]
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.1.0"|};
      ]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-base-unix" "%{base-unix:installed}%"
          "--with-cmdliner" "%{cmdliner:installed}%"]]|}
  in
  B0_pack.make "default" ~doc:"fmt package" ~meta ~locked:true @@
  B0_unit.list ()
