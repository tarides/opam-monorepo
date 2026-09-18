open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let unix = B0_ocaml.libname "unix"
let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"
let rresult = B0_ocaml.libname "rresult"
let rresult_top = B0_ocaml.libname "rresult.top"
let astring = B0_ocaml.libname "astring"
let astring_top = B0_ocaml.libname "astring.top"
let fpath = B0_ocaml.libname "fpath"
let fpath_top = B0_ocaml.libname "fpath.top"
let fmt = B0_ocaml.libname "fmt"
let fmt_top = B0_ocaml.libname "fmt.tty"
let fmt_tty = B0_ocaml.libname "fmt.tty"
let logs = B0_ocaml.libname "logs"
let logs_fmt = B0_ocaml.libname "logs.fmt"
let logs_top = B0_ocaml.libname "logs.top"
let mtime = B0_ocaml.libname "mtime"
let mtime_clock = B0_ocaml.libname "mtime.clock"

let bos = B0_ocaml.libname "bos"
let bos_setup = B0_ocaml.libname "bos.setup"
let bos_top = B0_ocaml.libname "bos.top"

(* Libraries *)

let bos_lib =
  let srcs = [ `Dir ~/"src" ] in
  let requires = [astring; fpath; fmt; unix; logs] in
  B0_ocaml.lib bos ~doc:"The bos library" ~srcs ~requires

let bos_setup_lib =
  let srcs = [`Dir ~/"src/setup"] in
  let requires = [rresult; fmt_tty; logs_fmt; astring; fpath; logs; fmt; bos] in
  B0_ocaml.lib bos_setup ~doc:"The bos.setup library" ~srcs ~requires

let bos_top_lib =
  let srcs = [`Dir ~/"src/top"; `X ~/"src/top/bos_top_init.ml"] in
  let requires =
    [ rresult_top; astring_top; fpath_top; fmt_top; logs_top;
      bos; compiler_libs_toplevel]
  in
  B0_ocaml.lib bos_top ~doc:"The bos.top library" ~srcs ~requires

(* Tests *)

let test =
  let srcs =
    [ `File ~/"test/testing.mli";
      `File ~/"test/testing.ml";
      `File ~/"test/test_cmd.ml";
      `File ~/"test/test_os_cmd.ml";
      `File ~/"test/test_pat.ml"; ]
  in
  let requires = [ rresult; astring; fpath; logs_fmt; bos] in
  B0_ocaml.test ~/"test/test.ml" ~doc:"Test suite" ~srcs ~requires

let test_arg =
  let doc = "Test argument parsing" in
  let requires = [ astring; fmt; fpath; logs; logs_fmt; bos ] in
  B0_ocaml.test ~/"test/test_arg.ml" ~doc ~requires ~run:false

let test_arg_pos =
  let doc = "Test argument parsing" in
  let requires = [ fmt; logs; logs_fmt; bos ] in
  B0_ocaml.test ~/"test/test_arg_pos.ml" ~doc ~requires ~run:false

let watch =
  let srcs = Filepath.[`File (v "test/watch.ml")] in
  let requires =
    [ unix; logs_fmt; fmt_tty; mtime; mtime_clock; rresult; fpath; bos;
      bos_setup ]
  in
  let doc = "Watch files for changes." in
  B0_ocaml.test ~/"test/watch.ml" ~doc ~srcs ~requires ~run:false

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The bos programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/bos"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/bos/doc"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/bos.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/bos/issues"
    |> ~~ B0_meta.description_tags
      ["os"; "system"; "cli"; "command"; "file"; "path"; "log"; "unix";
       "org:erratique"]
    |> B0_meta.tag B0_opam.tag
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.1.0"|};
        "base-unix", "";
        "rresult", {|>= "0.7.0"|};
        "astring", "";
        "fpath", "";
        "fmt", {|>= "0.8.10"|};
        "logs", "";
        "mtime", {|with-test|};
      ]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
  in
  B0_pack.make "default" ~doc:"bos package" ~meta ~locked:true @@
  B0_unit.list ()
