open B0_kit.V000

(* OCaml library names *)

let b0_std = B0_ocaml.libname "b0.std"

let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"
let mtime = B0_ocaml.libname "mtime"
let mtime_clock = B0_ocaml.libname "mtime.clock"
let unix = B0_ocaml.libname "unix"
let threads = B0_ocaml.libname "threads.posix"
let cmdliner = B0_ocaml.libname "cmdliner"
let fmt = B0_ocaml.libname "fmt"
let fmt_tty = B0_ocaml.libname "fmt.tty"
let fmt_cli = B0_ocaml.libname "fmt.cli"
let lwt = B0_ocaml.libname "lwt"
let lwt_unix = B0_ocaml.libname "lwt.unix"
let js_of_ocaml_compiler_runtime =
  B0_ocaml.libname "js_of_ocaml-compiler.runtime"

let logs = B0_ocaml.libname "logs"
let logs_fmt = B0_ocaml.libname "logs.fmt"
let logs_browser = B0_ocaml.libname "logs.browser"
let logs_cli = B0_ocaml.libname "logs.cli"
let logs_lwt = B0_ocaml.libname "logs.lwt"
let logs_threaded = B0_ocaml.libname "logs.threaded"
let logs_top = B0_ocaml.libname "logs.top"

(* Libraries *)

let logs_lib =
  B0_ocaml.lib logs ~srcs:[`Dir ~/"src"] ~requires:[]

let logs_fmt_lib =
  let srcs = [`Dir ~/"src/fmt"] in
  B0_ocaml.lib logs_fmt ~srcs ~requires:[logs; fmt] ~exports:[logs]

let logs_browser_lib =
  let srcs = [`Dir ~/"src/browser"] in
  let requires = [logs; js_of_ocaml_compiler_runtime] in
  B0_ocaml.lib logs_browser ~srcs ~requires ~exports:[logs]

let logs_threaded_lib =
  let srcs = [`Dir ~/"src/threaded"] in
  B0_ocaml.lib logs_threaded ~srcs ~requires:[logs; threads] ~exports:[logs]

let logs_cli_lib =
  let srcs = [`Dir ~/"src/cli"] in
  B0_ocaml.lib logs_cli ~srcs ~requires:[logs; cmdliner] ~exports:[logs]

let logs_lwt_lib =
  let srcs = [`Dir ~/"src/lwt"] in
  B0_ocaml.lib logs_lwt ~srcs ~requires:[logs; lwt] ~exports:[logs]

let logs_top_lib =
  let srcs = [`Dir ~/"src/top"] in
  B0_ocaml.lib logs_top ~srcs ~requires:[logs; compiler_libs_toplevel]

(* Tests *)

let test ?(requires = []) = B0_ocaml.test ~requires:(logs :: requires)

let test_fmt =
  let requires = [fmt_tty; logs_fmt] in
  test ~/"test/test_fmt.ml" ~requires ~run:false (* exits with 1. *)

let test_formatter =
  test ~/"test/test_formatter.ml" ~run:false (* exits with 1. *)

let test_tool =
  let requires = [logs_fmt; logs_cli; fmt_cli; fmt_tty; cmdliner] in
  test ~/"test/tool.ml" ~requires ~run:false (* exits with 1 *)

let test_tags =
  test ~/"test/tags.ml" ~requires:[mtime; mtime_clock]

let test_multi =
  test ~/"test/test_multi.ml" ~requires:[logs; logs_fmt; fmt_tty]

let test_threaded =
  test ~/"test/test_threaded.ml" ~requires:[logs_fmt; logs_threaded; threads]

let test_mutext_safe =
  test ~/"test/test_mutex_safe.ml" ~requires:[logs_fmt; logs_threaded; threads]

let test_lwt =
  let requires = [b0_std; logs_fmt; logs_lwt; fmt; fmt_tty; lwt; lwt_unix] in
  test ~/"test/test_lwt.ml" ~requires

let test_count = test ~/"test/test_count.ml" ~requires:[b0_std]

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The logs programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/logs"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/logs/doc"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/logs.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/logs/issues"
    |> ~~ B0_meta.description_tags ["log"; "system"; "org:erratique"; ]
    |> B0_meta.tag B0_opam.tag
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-js_of_ocaml-compiler" "%{js_of_ocaml-compiler:installed}%"
          "--with-fmt" "%{fmt:installed}%"
          "--with-cmdliner" "%{cmdliner:installed}%"
          "--with-lwt" "%{lwt:installed}%"
          "--with-base-threads" "%{base-threads:installed}%"]]|}
    |> ~~ B0_opam.depopts
      ["cmdliner", "";
       "js_of_ocaml-compiler", "";
       "fmt", "";
       "lwt", "";
       "base-threads", ""]
    |> B0_meta.add B0_opam.conflicts [
      "cmdliner", {|< "1.3.0"|};
      "js_of_ocaml-compiler", {|< "5.5.0"|};
      "fmt", {|< "0.9.0"|}; ]
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.1.0"|};
        "mtime", {|with-test|};]
  in
  B0_pack.make "default" ~doc:"logs package" ~meta ~locked:true @@
  B0_unit.list ()
