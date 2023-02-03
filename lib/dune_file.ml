open! Import

module Lang = struct
  type version = int * int

  let pp_version fmt (major, minor) = Format.fprintf fmt "%d.%d" major minor
  let version_to_string version = Format.asprintf "%a" pp_version version

  let stanza_regexp =
    let open Re in
    compile
      (seq
         [
           bos;
           char '(';
           rep blank;
           str "lang";
           rep1 blank;
           str "dune";
           rep1 blank;
           group (rep1 digit);
           char '.';
           group (rep1 digit);
           rep blank;
           char ')';
           rep blank;
           opt (char '\r');
           eol;
         ])

  let from_match group =
    let major_str = Re.Group.get group 1 in
    let minor_str = Re.Group.get group 2 in
    match (int_of_string_opt major_str, int_of_string_opt minor_str) with
    | Some major, Some minor -> Ok (major, minor)
    | _ ->
        Rresult.R.error_msgf
          "Invalid dune-project file: invalid lang version %s.%s" major_str
          minor_str

  let from_content content =
    match Re.all stanza_regexp content with
    | [ group ] -> from_match group
    | _ ->
        (* We match on [bos], meaning the only possible case here is the empty
           list *)
        Rresult.R.error_msg
          "Invalid dune-project file: It does not start with a valid lang \
           stanza"

  let update_stanza ~version group =
    let new_ver = version_to_string version in
    let old_ver =
      Printf.sprintf "%s.%s" (Re.Group.get group 1) (Re.Group.get group 2)
    in
    let stanza = Re.Group.get group 0 in
    Base.String.substr_replace_first ~pattern:old_ver ~with_:new_ver stanza

  let update ~version content =
    Re.replace ~all:false stanza_regexp ~f:(update_stanza ~version) content

  let compare_version (major, minor) (major', minor') =
    match Int.compare major major' with
    | 0 -> Int.compare minor minor'
    | _ as ord -> ord

  let duniverse_minimum_version = (1, 11)
end

module Packages = struct
  open Sexplib0.Sexp
  module Set = Set.Make (String)
  module Map = Map.Make (String)

  type t = Digest.t

  let init disambiguation = Digest.string disambiguation
  let random_valid_identifier t = Digest.to_hex t

  let random_public_name v original =
    let suffix = random_valid_identifier v in
    (* needs to start with `old.` to be part of the same package *)
    Fmt.str "%s.%s" original suffix

  let random_library_name v original =
    let suffix = random_valid_identifier v in
    (* needs not to have a dot *)
    Fmt.str "%s_%s" original suffix

  let find_by_name name stanzas =
    let matches =
      List.filter_map stanzas ~f:(function
        | List [ Atom candidate; Atom value ] -> (
            match String.equal candidate name with
            | true -> Some value
            | false -> None)
        | _ -> None)
    in
    match matches with [] -> None | [ x ] -> Some x | _ -> None

  type 'a rename_result = {
    changed : bool;
    stanzas : 'a;
    renames : string Map.t;
  }

  (* determine whether the package should be kept or not, handles [pkg.name] and [pkg] *)
  let should_keep ~keep name =
    let test_against =
      match Base.String.lsplit2 name ~on:'.' with
      | Some (package, _name) -> package
      | None -> name
    in
    Set.mem test_against keep

  let rename_library t ~keep renames stanzas =
    let public_name = find_by_name "public_name" stanzas in
    let name = find_by_name "name" stanzas in
    match public_name with
    | None -> { changed = false; stanzas; renames }
    | Some public_name -> (
        match should_keep ~keep public_name with
        | true -> { changed = false; stanzas; renames }
        | false ->
            let stanzas =
              List.filter stanzas ~f:(function
                | List (Atom "public_name" :: _) -> false
                | _ -> true)
            in
            let new_public_name = random_public_name t public_name in
            let stanzas =
              List [ Atom "public_name"; Atom new_public_name ] :: stanzas
            in

            let stanzas, renames =
              match name with
              | Some _private_name ->
                  let renames =
                    Map.add ~key:public_name ~data:new_public_name renames
                  in
                  (stanzas, renames)
              | None ->
                  (* we need to add a valid "name" field if there is none *)
                  let new_name = random_library_name t public_name in
                  let stanzas =
                    List [ Atom "name"; Atom new_name ] :: stanzas
                  in
                  let renames =
                    Map.add ~key:public_name ~data:new_name renames
                  in
                  (stanzas, renames)
            in
            { changed = true; stanzas; renames })

  let rename_one t ~keep renames = function
    | List (Atom "library" :: stanzas) ->
        let { changed; stanzas; renames } =
          rename_library t ~keep renames stanzas
        in
        { changed; stanzas = List (Atom "library" :: stanzas); renames }
    | stanzas -> { changed = false; stanzas; renames }

  let update_lib_reference renames = function
    | Atom old_name as original -> (
        match Map.find_opt old_name renames with
        | Some new_name ->
            let stanzas = Atom new_name in
            { changed = true; stanzas; renames }
        | None -> { changed = false; stanzas = original; renames })
    | otherwise -> { changed = false; stanzas = otherwise; renames }

  let is_tuareg = function
    | List (Atom "*" :: Atom "-*-" :: Atom "tuareg" :: Atom "-*-" :: _) -> true
    | _ -> false

  let rec update_reference renames = function
    | Atom _ as stanzas -> { changed = false; stanzas; renames }
    | List ((Atom "libraries" as stanza) :: libs) ->
        let changed, libs =
          List.fold_left
            ~f:(fun (changed_before, acc) lib ->
              let { changed; stanzas; renames = _ } =
                update_lib_reference renames lib
              in
              (changed_before || changed, stanzas :: acc))
            ~init:(false, []) libs
        in
        let libs = List.rev libs in
        let stanzas = List (stanza :: libs) in
        { changed; stanzas; renames }
    | List sexps ->
        let changed, sexps =
          List.fold_left
            ~f:(fun (changed_before, acc) sexp ->
              let { changed; stanzas; renames = _ } =
                update_reference renames sexp
              in
              (changed_before || changed, stanzas :: acc))
            ~init:(false, []) sexps
        in
        let sexps = List.rev sexps in
        { changed; stanzas = List sexps; renames }

  let update_references renames sexps =
    let changed = false in
    match sexps with
    | [] -> { changed; stanzas = []; renames }
    | first :: _ as stanzas when is_tuareg first ->
        { changed; stanzas; renames }
    | sexps ->
        let changed, sexps =
          List.fold_left
            ~f:(fun (changed_before, acc) sexp ->
              let { changed; stanzas; renames = _ } =
                update_reference renames sexp
              in
              (changed_before || changed, stanzas :: acc))
            ~init:(false, []) sexps
        in
        let sexps = List.rev sexps in
        { changed; stanzas = sexps; renames }

  let rename t ~keep renames sexps =
    let keep = Set.of_list keep in
    let changed = false in
    match sexps with
    | first :: _ as stanzas when is_tuareg first ->
        (* if the first sexp is the tuareg stanza, then it is an ocaml file,
           do not modify *)
        { changed; stanzas; renames }
    | sexps ->
        let { changed; stanzas; renames } =
          List.fold_left
            ~f:(fun { changed; stanzas = acc; renames } sexp ->
              let { changed = recursively_changed; stanzas = v; renames } =
                rename_one t ~keep renames sexp
              in
              let changed = changed || recursively_changed in
              { changed; stanzas = v :: acc; renames })
            ~init:{ changed; stanzas = []; renames }
            sexps
        in
        let stanzas = List.rev stanzas in
        { changed; stanzas; renames }
end

module Raw = struct
  let as_sexps path =
    try Ok (Sexplib.Sexp.load_sexps (Fpath.to_string path)) with
    | Sexplib.Sexp.Parse_error pe ->
        Error
          (`Msg
            (Format.asprintf "Failed to parse dune file %a: %s" Fpath.pp path
               pe.err_msg))
    | Failure _ ->
        Error
          (`Msg
            (Format.asprintf "Failed to parse dune file %a: Invalid sexp"
               Fpath.pp path))

  let comment s = Printf.sprintf "; %s" s
  let vendored_dirs glob = Printf.sprintf "(vendored_dirs %s)" glob

  let duniverse_dune_content =
    [
      comment "This file is generated by opam-monorepo.";
      comment
        "Be aware that it is likely to be overwritten by your next opam \
         monorepo pull invocation.";
      "";
      vendored_dirs "*";
    ]
end

module Project = struct
  let rec name sexps =
    match (sexps : Sexplib0.Sexp.t list) with
    | [] -> Error (`Msg "Missing a name field in the dune-project file")
    | List [ Atom "name"; Atom name ] :: _ -> Ok name
    | _ :: tl -> name tl
end
