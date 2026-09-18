open Configurator.V1

let get_warning_flags t =
  match ocaml_config_var t "ccomp_type" with
  | Some "msvc" -> ["/Wall"; "/W3"]
  | _ -> ["-Wall"; "-Wextra"; "-Wpedantic"]

let () =
  main ~name:"discover" (fun t ->
    let wflags = get_warning_flags t in
    Flags.write_sexp "cflags.sexp" wflags)
