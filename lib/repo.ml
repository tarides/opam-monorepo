open Import

type t = Fpath.t

let folder_blacklist = [ "_build"; "_opam"; Fpath.to_string Config.vendor_dir ]

let local_packages ~recurse ?filter t =
  let open Result.O in
  Bos.OS.Dir.exists t >>= fun exists ->
  if not exists then Ok String.Map.empty
  else
    let traverse =
      if recurse then
        `Sat
          (fun p ->
            Ok
              (not
                 (List.mem
                    (Fpath.to_string (Fpath.base p))
                    ~set:folder_blacklist)))
      else `Sat (fun p -> Ok (Fpath.equal p t))
    in
    let accept_name =
      match filter with
      | None -> fun _ -> true
      | Some list ->
          fun p ->
            let pkg = p |> Fpath.rem_ext |> Fpath.basename in
            List.exists ~f:(fun { Types.Opam.name; _ } -> name = pkg) list
    in
    Bos.OS.Path.fold
      ~elements:(`Sat (fun p -> Ok (Fpath.has_ext ".opam" p && accept_name p)))
      ~traverse
      (fun path acc -> Fpath.(basename (rem_ext path), t // path) :: acc)
      [] [ t ]
    >>= fun lst ->
    String.Map.of_list lst
    |> Result.map_error ~f:(fun (_, a, b) ->
           `Msg
             (Printf.sprintf
                "Conflicting definitions for the same package have been found:\n\
                 - %s\n\
                 - %s" (Fpath.to_string a) (Fpath.to_string b)))

let all_local_packages_names repo =
  let open Result.O in
  Bos.OS.Dir.exists repo >>= fun exists ->
  if not exists then Ok OpamPackage.Name.Set.empty
  else
    Bos.OS.Path.fold
      ~elements:(`Sat (fun p -> Ok (Fpath.has_ext ".opam" p)))
      (fun path acc ->
        OpamPackage.Name.Set.add
          (OpamPackage.Name.of_string Fpath.(basename (rem_ext path)))
          acc)
      OpamPackage.Name.Set.empty [ repo ]

let dune_project t = Fpath.(t / "dune-project")

let project_name t =
  let open Result.O in
  let dune_project = dune_project t in
  Dune_file.Raw.as_sexps dune_project >>= Dune_file.Project.name

let lockfile ~name t = Fpath.(t / (name ^ ".opam.locked"))

let lockfile ?local_packages:lp t =
  let open Result.O in
  let local_packages =
    match lp with
    | Some lp -> Ok lp
    | None ->
        local_packages ~recurse:false t >>| fun lp ->
        List.map
          ~f:(fun (name, _) -> { Types.Opam.name; version = None })
          (String.Map.bindings lp)
  in
  local_packages >>= fun pkgs ->
  match pkgs with
  | [ { name; _ } ] -> Ok (lockfile ~name t)
  | _ -> project_name t >>= fun name -> Ok (lockfile ~name t)
