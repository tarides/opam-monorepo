open Astring
open Rresult

let compute_deps ~opam_entries =
  Dune_cmd.log_invalid_packages opam_entries;
  let get_default_branch remote = Exec.git_default_branch ~remote () in
  Duniverse.Deps.from_opam_entries ~get_default_branch opam_entries

let resolve_ref deps =
  let resolve_ref ~upstream ~ref = Exec.git_resolve ~remote:upstream ~ref in
  Duniverse.Deps.resolve ~resolve_ref deps

let path pin =
  Fpath.(Config.pins_dir / (pin.Types.Opam.pin ^ ".opam"))

let read_from_config duniverse_file =
  Bos.OS.File.exists duniverse_file >>= fun exists ->
  if not exists then Ok [] else
  Duniverse.load ~file:duniverse_file >>= fun duniverse ->
  Ok duniverse.config.pins

let read_from_opam opam_file =
  let open OpamParserTypes in
  let pin_of_opam_pin_depend value =
    match value with
    | List (_, [String (_, pkg_nv); String (_, url)]) ->
      let url = Uri.of_string url in
      let tag = Uri.fragment url in
      let url = Some Uri.(with_fragment url None |> to_string) in
      begin
        match String.cut ~sep:"." pkg_nv with
        | Some (pin, ("" | "dev")) -> Some { Types.Opam.pin; tag; url }
        | Some (pin, version) when tag = None -> Some { pin; tag = Some version; url }
        | Some (pin, version) -> Some { pin; tag = Some version; url }
        | None -> None
      end
    | _ -> None
  in
  OpamParser.file (Fpath.to_string opam_file) |> fun { file_contents; _ } ->
  file_contents
  |> List.filter_map (function
      | Variable (_, "pin-depends", List (_, pin_depends)) ->
        Some (List.filter_map pin_of_opam_pin_depend pin_depends)
      | _ -> None)
  |> List.flatten
  |> List.map (fun pin -> (pin.Types.Opam.pin, (opam_file, pin)))
  |> String.Map.of_list


let merge ~opam_files ~config_pins =
  let merge_or_fail_on_dup =
    String.Map.union (fun name (file1, _pin1) (file2, _pin2) ->
      Fmt.failwith "@[<2>Found duplicate pin entries for `%s`:@;- %a@;- %a@]"
        name Fpath.pp file1 Fpath.pp file2)
  in
  let opam_pin_depends =
    String.Map.fold
      (fun _ opam_file acc -> merge_or_fail_on_dup (read_from_opam opam_file) acc)
      opam_files String.Map.empty
    |> String.Map.map snd
  in
  let add_config_pin opam_pins config_pin =
    String.Map.update config_pin.Types.Opam.pin
      begin function
        | None -> Some config_pin
        | opam_pin -> opam_pin
      end opam_pins
  in
  let all = List.fold_left add_config_pin opam_pin_depends config_pins in
  String.Map.fold (fun _ pin acc -> pin :: acc) all []

let read ~opam_files ~config =
  read_from_config config >>= fun config_pins ->
  try Ok (merge ~opam_files ~config_pins)
  with Failure msg -> R.error_msg msg

let to_package (pin : Types.Opam.pin) : Types.Opam.package =
  let name = pin.pin in
  let version = None in
  {name; version}

let to_opam_entry (pin : Types.Opam.pin) : Types.Opam.entry =
  let name = pin.pin in
  let package = to_package pin in
  let dev_repo = Opam_cmd.classify_from_dev_repo ~name pin.url in
  { package; dev_repo; tag = pin.tag; is_dune = true }

let copy_opam_files ~pinned_paths deps =
  Bos.OS.Dir.create Config.pins_dir >>= fun _created ->
  Stdune.Result.List.iter deps
    ~f:(fun {Duniverse.Deps.Source.dir; provided_packages; _} ->
      Stdune.Result.List.iter provided_packages
        ~f:(fun {Duniverse.Deps.Opam.name; _} ->
          if String.Map.mem name pinned_paths then Ok () else
          let opam = name ^ ".opam" in
          let src = Fpath.(Config.vendor_dir / dir / opam |> to_string) in
          let dst = Fpath.(Config.pins_dir / opam |> to_string) in
          let cmd = Bos.Cmd.(v "cp" % src % dst) in (* FIXME: does this work on win? *)
          Bos.OS.Cmd.run cmd))

let remove_stale_pins ~pinned_paths pins =
  if pins = [] then Bos.OS.Dir.delete ~recurse:true Config.pins_dir else
  let stale =
    String.Map.filter
      (fun name _ -> not (List.exists (fun pin -> pin.Types.Opam.pin = name) pins))
      pinned_paths in
  String.Map.fold (fun _ p _ -> Bos.OS.File.delete ~must_exist:true p) stale (Ok ())

let pull ~pull_mode ~repo ~pinned_paths pins =
  let opam_entries = List.map to_opam_entry pins in
  compute_deps ~opam_entries >>= resolve_ref >>= fun {Duniverse.Deps.duniverse; _} ->
  let duniverse_to_pull =
    List.filter (fun {Duniverse.Deps.Source.provided_packages; _} ->
        List.for_all (fun {Duniverse.Deps.Opam.name; _} -> not (String.Map.mem name pinned_paths))
          provided_packages)
      duniverse in
  Cloner.get_cache () >>= fun cache ->
  Pull.duniverse ~cache ~pull_mode ~repo duniverse_to_pull >>= fun () ->
  Ok duniverse

let init ~repo ~pull_mode ~pins =
  Opam_cmd.find_local_opam_packages Config.pins_dir >>= fun pinned_paths ->
  remove_stale_pins ~pinned_paths pins >>= fun () ->
  pull ~pull_mode ~repo ~pinned_paths pins >>= fun src_deps ->
  copy_opam_files ~pinned_paths src_deps >>= fun () ->
  Ok src_deps

