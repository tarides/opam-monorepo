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

  type redirected_name = { package_name : string; subpackage : string option }

  let public_name_of_redirect { package_name; subpackage } =
    match subpackage with
    | None -> package_name
    | Some subpackage -> Fmt.str "%s.%s" package_name subpackage

  let random_public_name v original =
    let disamb = random_valid_identifier v in
    match Base.String.lsplit2 original ~on:'.' with
    | Some (package, subpackage) ->
        let package_name = Fmt.str "%s_%s" package disamb in
        { package_name; subpackage = Some subpackage }
    | None ->
        let package_name = Fmt.str "%s_%s" original disamb in
        { package_name; subpackage = None }

  let package_name name =
    match Base.String.lsplit2 name ~on:'.' with
    | Some (package, _) -> package
    | None -> name

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

  type new_name = {
    public_name : string;
    private_name : string option;
    dune_project : string;
  }

  type 'a change = { changed : bool; data : 'a }
  type 'a rename = { stanzas : 'a; renames : new_name Map.t }

  let modified ~changed ~stanzas ~renames =
    { changed; data = { stanzas; renames } }

  let changed = modified ~changed:true
  let unchanged = modified ~changed:false

  (* determine whether the package should be kept or not, handles [pkg.name] and [pkg] *)
  let should_keep ~keep name =
    let test_against =
      match Base.String.lsplit2 name ~on:'.' with
      | Some (package, _name) -> package
      | None -> name
    in
    Set.mem test_against keep

  let rename_library t ~dune_project ~keep renames stanzas =
    let public_name = find_by_name "public_name" stanzas in
    let name = find_by_name "name" stanzas in
    match public_name with
    | None -> unchanged ~stanzas ~renames
    | Some public_name -> (
        match should_keep ~keep public_name with
        | true -> unchanged ~stanzas ~renames
        | false ->
            let stanzas =
              List.filter stanzas ~f:(function
                | List (Atom "public_name" :: _) -> false
                | _ -> true)
            in
            let new_public_name = random_public_name t public_name in
            let stanzas =
              List
                [
                  Atom "public_name";
                  Atom (public_name_of_redirect new_public_name);
                ]
              :: stanzas
            in
            (* add a redirect from base name to new name *)
            let renames =
              Map.add ~key:(package_name public_name)
                ~data:
                  {
                    public_name = new_public_name.package_name;
                    private_name = None;
                    dune_project;
                  }
                renames
            in

            let stanzas, renames =
              match name with
              | Some _ as private_name ->
                  let data =
                    {
                      public_name = public_name_of_redirect new_public_name;
                      private_name;
                      dune_project;
                    }
                  in
                  let renames = Map.add ~key:public_name ~data renames in
                  (stanzas, renames)
              | None as private_name ->
                  (* we need to add a valid "name" field and it has to match the
                     previous public name to make sure there is no extra level
                     of wrapping happening if the library comes with its own
                     entry point module. *)
                  let stanzas =
                    List [ Atom "name"; Atom public_name ] :: stanzas
                  in
                  (* private_name is None, because it means that the old
                     reference can't have referred to the private name as it
                     did not exist before *)
                  let data =
                    {
                      public_name = public_name_of_redirect new_public_name;
                      private_name;
                      dune_project;
                    }
                  in
                  let renames = Map.add ~key:public_name ~data renames in
                  (stanzas, renames)
            in
            changed ~stanzas ~renames)

  let rename_one t ~dune_project ~keep renames = function
    | List (Atom "library" :: stanzas) ->
        let { changed; data = { stanzas; renames } } =
          rename_library t ~dune_project ~keep renames stanzas
        in
        {
          changed;
          data = { stanzas = List (Atom "library" :: stanzas); renames };
        }
    | stanzas -> { changed = false; data = { stanzas; renames } }

  let library_name ~dune_project renames old_name =
    let open Option.O in
    let* { public_name; private_name; dune_project = origin_dune_project } =
      Map.find_opt old_name renames
    in
    match private_name with
    | Some name when old_name = name && dune_project = origin_dune_project ->
        None
    | None | Some _ -> Some public_name

  let update_lib_reference ~dune_project renames = function
    | Atom old_name as data -> (
        match library_name ~dune_project renames old_name with
        | None -> { changed = false; data }
        | Some public_name ->
            let data = Atom public_name in
            { changed = true; data })
    | List [ Atom "re_export"; Atom old_name ] as data -> (
        match library_name ~dune_project renames old_name with
        | None -> { changed = false; data }
        | Some public_name ->
            let data = List [ Atom "re_export"; Atom public_name ] in
            { changed = true; data })
    | otherwise -> { changed = false; data = otherwise }

  (* the file is an ML file if the first stanza starts with the emacs tuareg mode
     comment *)
  let is_tuareg = function
    | List (Atom "*" :: Atom "-*-" :: Atom "tuareg" :: Atom "-*-" :: _) :: _ ->
        true
    | _ -> false

  let rec update_reference ~dune_project renames = function
    | Atom _ as data -> { changed = false; data }
    | List (Atom "package" :: Atom old_name :: _) as data -> (
        match Map.find_opt old_name renames with
        | None -> { changed = false; data }
        | Some { public_name; private_name = _; dune_project = _ } ->
            { changed = true; data = List [ Atom "package"; Atom public_name ] }
        )
    | List ((Atom "libraries" as stanza) :: libs) ->
        let changed, libs =
          List.fold_left
            ~f:(fun (changed_before, acc) lib ->
              let { changed; data } =
                update_lib_reference ~dune_project renames lib
              in
              (changed_before || changed, data :: acc))
            ~init:(false, []) libs
        in
        let libs = List.rev libs in
        let data = List (stanza :: libs) in
        { changed; data }
    | List sexps ->
        let changed, sexps =
          List.fold_left
            ~f:(fun (changed_before, acc) sexp ->
              let { changed; data } =
                update_reference ~dune_project renames sexp
              in
              (changed_before || changed, data :: acc))
            ~init:(false, []) sexps
        in
        let sexps = List.rev sexps in
        { changed; data = List sexps }

  let update_references ~dune_project renames data =
    let changed = false in
    match is_tuareg data with
    | true -> { changed; data }
    | false ->
        let changed, data =
          List.fold_left
            ~f:(fun (changed_before, acc) sexp ->
              let { changed; data } =
                update_reference ~dune_project renames sexp
              in
              (changed_before || changed, data :: acc))
            ~init:(false, []) data
        in
        let data = List.rev data in
        { changed; data }

  let rename t ~dune_project ~keep renames stanzas =
    let keep = Set.of_list keep in
    match is_tuareg stanzas with
    | true ->
        (* if the first sexp is the tuareg stanza, then it is an ocaml file,
           do not modify *)
        unchanged ~stanzas ~renames
    | false ->
        let changed, stanzas, renames =
          List.fold_left
            ~f:(fun (changed, stanzas, renames) sexp ->
              let {
                changed = recursively_changed;
                data = { stanzas = v; renames };
              } =
                rename_one t ~dune_project ~keep renames sexp
              in
              let changed = changed || recursively_changed in
              (changed, v :: stanzas, renames))
            ~init:(false, [], renames) stanzas
        in
        let stanzas = List.rev stanzas in
        modified ~changed ~stanzas ~renames

  let renamed_opam_file renames path =
    let directory, opam_file_name = Fpath.split_base path in
    let package_name = Fpath.rem_ext opam_file_name |> Fpath.to_string in
    match Map.find_opt package_name renames with
    | None -> path
    | Some { public_name; private_name = _; dune_project = _ } ->
        let new_file_name = public_name |> Fpath.v |> Fpath.add_ext "opam" in
        Fpath.(directory // new_file_name)

  let update_dune_project_stanzas renames sexp =
    match sexp with
    | List (Atom "package" :: sexps) ->
        let changed, sexps =
          List.fold_left
            ~f:(fun (changed, acc) sexp ->
              match sexp with
              | List [ Atom "name"; Atom old_name ] as data -> (
                  match Map.find_opt old_name renames with
                  | None -> (changed, data :: acc)
                  | Some { public_name; private_name = _; dune_project = _ } ->
                      (true, List [ Atom "name"; Atom public_name ] :: acc))
              | data -> (changed, data :: acc))
            ~init:(false, []) sexps
        in
        let sexps = List.rev sexps in
        let data = List (Atom "package" :: sexps) in
        { changed; data }
    | data -> { changed = false; data }

  let update_dune_project_references renames sexps =
    let changed, sexps =
      List.fold_left
        ~f:(fun (changed_before, acc) sexp ->
          let { changed; data } = update_dune_project_stanzas renames sexp in
          (changed_before || changed, data :: acc))
        ~init:(false, []) sexps
    in
    let sexps = List.rev sexps in
    { changed; data = sexps }
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
