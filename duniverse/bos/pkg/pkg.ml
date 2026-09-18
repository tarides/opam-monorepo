#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "bos" @@ fun c ->
  Ok [ Pkg.mllib ~api:["Bos"] "src/bos.mllib";
       Pkg.mllib "src/setup/bos_setup.mllib" ~dst_dir:"setup";
       Pkg.mllib ~api:[] "src/top/bos_top.mllib" ~dst_dir:"top";
       Pkg.lib "src/top/bos_top_init.ml";
       Pkg.lib "src/top/bos_top_init.ml" ~dst:"top/bos_top_init_ml" ]
