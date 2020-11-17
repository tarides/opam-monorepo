open Import

module Lang = struct
  type version = int * int

  let compare_version (major, minor) (major', minor') =
    match Int.compare major major' with 0 -> Int.compare minor minor' | _ as ord -> ord

  let pp_version fmt (major, minor) = Format.fprintf fmt "%d.%d" major minor

  let parse_version s =
    let err () = Error (`Msg (Printf.sprintf "Invalid dune lang version: %s" s)) in
    match String.split_on_char ~sep:'.' s with
    | [ major; minor ] -> (
        match (int_of_string_opt major, int_of_string_opt minor) with
        | Some major, Some minor -> Ok (major, minor)
        | _ -> err () )
    | _ -> err ()

  let parse_stanza s =
    let content =
      let open Option.O in
      String.drop_prefix ~prefix:"(" s >>= String.drop_suffix ~suffix:")" >>| fun content ->
      String.split_on_char ~sep:' ' content
    in
    match content with
    | Some [ "lang"; "dune"; version ] -> parse_version version
    | _ -> Error (`Msg (Printf.sprintf "Invalid lang stanza: %s" s))

  let is_stanza s = String.is_prefix ~prefix:"(lang " s

  let duniverse_minimum_version = (1, 11)

  let stanza version = Format.asprintf "(lang dune %a)" pp_version version
end

module Raw = struct
  let as_sexps path =
    try Ok (Sexplib.Sexp.load_sexps (Fpath.to_string path)) with
    | Sexplib.Sexp.Parse_error pe ->
        Error (`Msg (Format.asprintf "Failed to parse dune file %a: %s" Fpath.pp path pe.err_msg))
    | Failure _ ->
        Error (`Msg (Format.asprintf "Failed to parse dune file %a: Invalid sexp" Fpath.pp path))

  let comment s = Printf.sprintf "; %s" s

  let vendored_dirs glob = Printf.sprintf "(vendored_dirs %s)" glob

  let duniverse_dune_content =
    [
      comment "This file is generated by duniverse.";
      comment "Be aware that it is likely to be overwritten by your next duniverse pull invocation.";
      "";
      vendored_dirs "*";
    ]

  let duniverse_minimum_lang = Lang.stanza Lang.duniverse_minimum_version
end

module Project = struct
  module OV = Ocaml_version

  let rec name sexps =
    match (sexps : Sexplib0.Sexp.t list) with
    | [] -> Error (`Msg "Missing a name field in the dune-project file")
    | List [ Atom "name"; Atom name ] :: _ -> Ok name
    | _ :: tl -> name tl

  let load_dune_project () = Raw.as_sexps (Fpath.v "dune-project")

  let check_opam_generation =
    let open Sexplib.Sexp in
    List.exists ~f:(function
      | List [ Atom "generate_opam_files"; Atom "true" ] -> true
      | _ -> false)

  let compare_ov a b op =
    let b = OV.of_string_exn b in
    match (OV.compare a b, op) with
    | -1, ("<=" | "<" | "<>") -> true
    | 0, ("<=" | "=" | ">=") -> true
    | 0, "<>" -> false
    | 1, (">=" | ">" | "<>") -> true
    | _ -> false

  let eval_ocaml_bcomp sxp ov =
    let open Sexplib.Sexp in
    let rec eval = function
      | List (Atom "and" :: tl) -> List.fold_left ~f:(fun a b -> eval b && a) ~init:true tl
      | List (Atom "or" :: tl) -> List.fold_left ~f:(fun a b -> eval b || a) ~init:false tl
      | List [ Atom "not"; a ] -> not (eval a)
      | List [ Atom ((">=" | ">" | "=" | "<" | "<=" | "<>") as op); Atom a ] -> compare_ov ov a op
      | List _ -> failwith "unexpected list atoms, only ne/and/or/not supported"
      | Atom _ -> true
    in
    eval sxp

  let supported_ocaml_compilers () =
    let open Result.O in
    let open Sexplib.Sexp in
    load_dune_project () >>= fun sxp ->
    (* Check that opam file generation is on, and warn otherwise *)
    match check_opam_generation sxp with
    | false ->
        Error
          (`Msg
            "Duniverse requires you to have (generate_opam_files true) in your dune-project, so \
             that it can infer the supported OCaml versions from your package constraints. Please \
             see https://dune.readthedocs.io/en/stable/opam.html#generating-opam-files")
    | true ->
        let constr =
          List.filter_map
            ~f:(function
              | List (Atom "package" :: prsxp) -> (
                  List.filter_map
                    ~f:(function List (Atom "depends" :: depsxp) -> Some depsxp | _ -> None)
                    prsxp
                  |> function
                  | [] -> None
                  | depsxp :: _ -> (
                      List.filter_map
                        ~f:(function
                          | Atom "ocaml" -> Some []
                          | List (Atom "ocaml" :: ocsxp) -> Some ocsxp
                          | _ -> None)
                        depsxp
                      |> function
                      | [] -> None
                      | res :: _ -> Some res ) )
              | _ -> None)
            sxp
          |> List.flatten
        in
        let constr = List (Atom "and" :: constr) in
        let r = List.filter ~f:(eval_ocaml_bcomp constr) OV.Releases.all in
        Ok r
end
