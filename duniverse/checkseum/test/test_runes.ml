#!/usr/bin/env ocaml

#use "topfind"

#require "astring"

#require "fpath"

#require "bos"

open Rresult

let is_opt x = String.length x > 1 && x.[0] = '-'

let parse_opt_arg x =
  let l = String.length x in
  if x.[1] <> '-'
  then
    if l = 2
    then (x, None)
    else (String.sub x 0 2, Some (String.sub x 2 (l - 2)))
  else
    try
      let i = String.index x '=' in
      (String.sub x 0 i, Some (String.sub x (i + 1) (l - i - 1)))
    with Not_found -> (x, None)

type arg =
  | Path of Fpath.t
  | Library of [ `Abs of Fpath.t | `Rel of Fpath.t | `Name of string ]

let parse_lL_value name value =
  match name with
  | "-L" -> (
      match Fpath.of_string value with
      | Ok v when Fpath.is_dir_path v && Sys.is_directory value -> R.ok (Path v)
      | Ok v when Sys.is_directory value -> R.ok (Path (Fpath.to_dir_path v))
      | Ok v -> R.error_msgf "Directory <%a> does not exist" Fpath.pp v
      | Error err -> Error err)
  | "-l" -> (
      match Astring.String.cut ~sep:":" value with
      | Some ("", path) -> (
          match Fpath.of_string path with
          | Ok v when Fpath.is_abs v && Sys.file_exists path ->
              Ok (Library (`Abs v))
          | Ok v when Fpath.is_rel v -> Ok (Library (`Rel v))
          | Ok v -> R.error_msgf "Library <%a> does not exist" Fpath.pp v
          | Error err -> Error err)
      | Some (_, _) -> R.error_msgf "Invalid <namespec> %S" value
      | None ->
      match Fpath.of_string value with
      | Ok v when Fpath.is_file_path v && Fpath.filename v = value ->
          Ok (Library (`Name value))
      | Ok v -> R.error_msgf "Invalid library name <%a>" Fpath.pp v
      | Error err -> Error err)
  | _ -> Fmt.failwith "Invalid argument name %S" name

let parse_lL_args args =
  let rec go lL_args = function
    | [] | "--" :: _ -> R.ok (List.rev lL_args)
    | x :: args -> (
        if not (is_opt x)
        then go lL_args args
        else
          let name, value = parse_opt_arg x in
          match name with
          | "-L" | "-l" -> (
              match value with
              | Some value ->
                  parse_lL_value name value >>= fun v -> go (v :: lL_args) args
              | None ->
              match args with
              | [] -> R.error_msgf "%s must have a value." name
              | value :: args ->
                  if is_opt value
                  then R.error_msgf "%s must have a value." name
                  else
                    parse_lL_value name value >>= fun v ->
                    go (v :: lL_args) args)
          | _ -> go lL_args args) in
  go [] args

let is_path = function Path _ -> true | Library _ -> false
let prj_path = function Path x -> x | _ -> assert false
let prj_libraries = function Library x -> x | _ -> assert false

let libraries_exist args =
  let paths, libraries = List.partition is_path args in
  let paths = List.map prj_path paths in
  let libraries = List.map prj_libraries libraries in
  let rec go = function
    | [] -> R.ok ()
    | `Rel library :: libraries ->
        let rec check = function
          | [] -> R.error_msgf "Library <:%a> does not exist." Fpath.pp library
          | p0 :: ps -> (
              let path = Fpath.(p0 // library) in
              Bos.OS.Path.exists path >>= function
              | true -> go libraries
              | false -> check ps) in
        check paths
    | `Name library :: libraries ->
        let lib = Fmt.strf "lib%s.a" library in
        let rec check = function
          | [] -> R.error_msgf "Library lib%s.a does not exist." library
          | p0 :: ps -> (
              let path = Fpath.(p0 / lib) in
              Bos.OS.Path.exists path >>= function
              | true -> go libraries
              | false -> check ps) in
        check paths
    | `Abs path :: libraries -> (
        Bos.OS.Path.exists path >>= function
        | true -> go libraries
        | false -> R.error_msgf "Library <%a> does not exist." Fpath.pp path)
  in
  go libraries

let exists lib =
  let open Bos in
  let command = Cmd.(v "ocamlfind" % "query" % lib) in
  OS.Cmd.run_out command |> OS.Cmd.out_null >>= function
  | (), (_, `Exited 0) -> R.ok true
  | _ -> R.ok false

let query target lib =
  let open Bos in
  let format = Fmt.strf "-L%%d %%(%s_linkopts)" target in
  let command = Cmd.(v "ocamlfind" % "query" % "-format" % format % lib) in
  OS.Cmd.run_out command
  |> OS.Cmd.out_lines
  >>= (function
        | output, (_, `Exited 0) -> R.ok output
        | _ -> R.error_msgf "<ocamlfind> does not properly exit.")
  >>| String.concat " "
  >>| Astring.String.cuts ~sep:" " ~empty:false

let run () =
  (exists "mirage-xen-posix" >>= function
   | true -> query "xen" "checkseum" >>= parse_lL_args >>= libraries_exist
   | false -> R.ok ())
  >>= fun () ->
  (exists "ocaml-freestanding" >>= function
   | true ->
       query "freestanding" "checkseum" >>= parse_lL_args >>= libraries_exist
   | false -> R.ok ())
  >>= fun () -> R.ok ()

let exit_success = 0
let exit_failure = 1

let () =
  match run () with
  | Ok () -> exit exit_success
  | Error (`Msg err) ->
      Fmt.epr "%s\n%!" err ;
      exit exit_failure
