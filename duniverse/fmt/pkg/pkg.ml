#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let unix = Conf.with_pkg "base-unix"
let cmdliner = Conf.with_pkg "cmdliner"

let () =
  Pkg.describe "fmt" @@ fun c ->
  let unix = Conf.value c unix in
  let cmdliner = Conf.value c cmdliner in
  Ok [ Pkg.mllib "src/fmt.mllib";
       Pkg.mllib ~cond:unix ~dst_dir:"tty" "src/tty/fmt_tty.mllib";
       Pkg.mllib ~cond:cmdliner ~dst_dir:"cli" "src/cli/fmt_cli.mllib";
       Pkg.mllib ~api:[] ~dst_dir:"top" "src/top/fmt_top.mllib";
       Pkg.lib ~dst:"top/fmt_tty_top_init.ml" "src/top/fmt_tty_top_init.ml";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld" ]
