open Duniverse_lib
open Rresult.R.Infix
open Cmdliner

let run (`Repo repo) () =
  let duniverse_file = Fpath.(repo // Config.duniverse_file) in
  Duniverse.load ~file:duniverse_file >>= fun config ->
  print_endline (String.concat " " config.Duniverse.config.ocaml_compilers);
  Ok ()

let info =
  let doc = "print OCaml compilers that are supported for this duniverse" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "This command prints the OCaml compilers that are supported by this\n\
        \ duniverse. The results can be piped to your continuous integration\n\
         system to install the compiler needed for a successful dune build.";
    ]
  in
  Term.info "print-ocaml-compilers" ~doc ~exits ~man

let term = Term.(term_result (const run $ Common.Arg.repo $ Common.Arg.setup_logs ()))

let cmd = (term, info)
