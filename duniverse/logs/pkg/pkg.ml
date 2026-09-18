#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let jsoo = Conf.with_pkg "js_of_ocaml-compiler"
let cmdliner = Conf.with_pkg "cmdliner"
let fmt = Conf.with_pkg "fmt"
let lwt = Conf.with_pkg "lwt"
let threads = Conf.with_pkg "base-threads"
let () =
  Pkg.describe "logs" @@ fun c ->
  let jsoo = Conf.value c jsoo in
  let cmdliner = Conf.value c cmdliner in
  let fmt = Conf.value c fmt in
  let lwt = Conf.value c lwt in
  let threads = Conf.value c threads in
  Ok [ Pkg.mllib "src/logs.mllib";
       Pkg.mllib ~cond:fmt "src/fmt/logs_fmt.mllib" ~dst_dir:"fmt";
       Pkg.mllib ~cond:jsoo "src/browser/logs_browser.mllib" ~dst_dir:"browser";
       Pkg.mllib ~cond:cmdliner "src/cli/logs_cli.mllib" ~dst_dir:"cli";
       Pkg.mllib ~cond:lwt "src/lwt/logs_lwt.mllib" ~dst_dir:"lwt";
       Pkg.mllib ~cond:fmt ~api:[] "src/top/logs_top.mllib" ~dst_dir:"top";
       Pkg.mllib ~cond:threads
         "src/threaded/logs_threaded.mllib" ~dst_dir:"threaded";
       Pkg.lib "src/top/logs_top_init.ml";
       Pkg.lib "src/top/logs_top_init.ml" ~dst:"top/logs_top_init_ml";
       Pkg.lib "src/fmt/logs_fmt_top_init.ml" ~dst:"fmt/logs_fmt_top_init.ml";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld"]
