open Import

(* Check that [output_dir] is strictly a descendant of [duniverse_dir] *)
let is_in_universe_dir ~duniverse_dir ~output_dir =
  Fpath.(is_prefix (normalize duniverse_dir) (normalize output_dir))
  && not (String.equal (Fpath.filename output_dir) "")

(* Delete version control metadata and vendor subdirectory *)
let do_trim_clone output_dir =
  let open Result.O in
  let* () =
    Bos.OS.Dir.delete ~must_exist:false ~recurse:true
      Fpath.(output_dir / ".git")
  in
  Bos.OS.Dir.delete ~recurse:true Fpath.(output_dir // Config.vendor_dir)

let parse_sexps path =
  Bos.OS.File.with_ic path
    (fun ic () ->
      match Sexplib.Sexp.input_sexps ic with
      | sexps -> Some sexps
      | exception _ -> None)
    ()

let preprocess_dune dfp ~keep ~dune_project ~renames dune_file =
  let open Result.O in
  let* sexps = parse_sexps dune_file in
  match sexps with
  | None -> Ok None
  | Some sexps -> (
      let Dune_file.Packages.{ changed; data = { stanzas; renames } } =
        Dune_file.Packages.rename dfp ~dune_project ~keep renames sexps
      in
      match changed with
      | false -> Ok None
      | true -> Ok (Some (stanzas, renames)))

(* can't use Sexplib pretty printer here because Sexplib tries too hard
   to escape values like \ and thus generates dune files that dune can't
   read *)
let rec pp_sexp ppf = function
  | Sexplib0.Sexp.List items ->
      let pp_items = Fmt.list ~sep:(Fmt.any " ") pp_sexp in
      Fmt.pf ppf "(%a)" pp_items items
  | Atom "" -> Fmt.string ppf {|""|}
  | Atom s -> (
      match
        String.contains s ' ' || String.contains s '(' || String.contains s ')'
      with
      | false -> Fmt.string ppf s
      | true ->
          let s = String.escaped s in
          Fmt.pf ppf {|"%s"|} s)

let write_sexps path sexps =
  let open Result.O in
  let* write_result =
    Bos.OS.File.with_oc path
      (fun oc sexps ->
        let ppf = Format.formatter_of_out_channel oc in
        List.iter ~f:(fun sexp -> Fmt.pf ppf "%a\n" pp_sexp sexp) sexps;
        Ok (Fmt.flush ppf ()))
      sexps
  in
  write_result

let name_of_path path =
  match parse_sexps path with
  | Error msg ->
      Logs.err (fun l ->
          l "Error loading file %a: %a" Fpath.pp path Rresult.R.pp_msg msg);
      None
  | Ok None -> None
  | Ok (Some sexps) -> (
      match Dune_file.Project.name sexps with
      | Ok name -> Some name
      | Error msg ->
          Logs.debug (fun l ->
              l "Determining project name of `%a` failed: %a" Fpath.pp path
                Rresult.R.pp_msg msg);
          None)

let rec dune_project_of_dune path =
  let parent_dir = Fpath.parent path in
  match Fpath.equal path parent_dir with
  | true -> None
  | false -> (
      let maybe_dune_project = Fpath.(path / "dune-project") in
      match Bos.OS.Path.exists maybe_dune_project with
      | Ok true -> (
          match name_of_path maybe_dune_project with
          | None ->
              let basename = Fpath.basename path in
              Some basename
          | Some _ as project -> project)
      | Ok false -> dune_project_of_dune parent_dir
      | Error msg ->
          Logs.err (fun l ->
              l "Error determining existance of %a: %a" Fpath.pp
                maybe_dune_project Rresult.R.pp_msg msg);
          None)

let if_no_dune_project = Option.value ~default:""

let rename_opam_files renames directory =
  let is_opam_file path =
    let extension = Fpath.get_ext path in
    Ok (String.equal extension ".opam")
  in
  Bos.OS.Path.fold ~elements:(`Sat is_opam_file)
    (fun path () ->
      let new_path = Dune_file.Packages.renamed_opam_file renames path in
      match Fpath.equal path new_path with
      | true -> ()
      | false -> (
          match Bos.OS.Path.move path new_path with
          | Ok () ->
              Logs.debug (fun l ->
                  l "Renamed OPAM file: %a -> %a\n" Fpath.pp path Fpath.pp
                    new_path);
              ()
          | Error msg ->
              Logs.err (fun l ->
                  l "Error moving OPAM file %a: %a" Fpath.pp path
                    Rresult.R.pp_msg msg)))
    () [ directory ]

let rewrite_dune_project renames directory =
  let open Result.O in
  let dune_project_file = Fpath.(directory / "dune-project") in
  let* exists = Bos.OS.File.exists dune_project_file in
  match exists with
  | false -> Ok ()
  | true -> (
      let* parsed = parse_sexps dune_project_file in
      match parsed with
      | None -> Ok ()
      | Some sexps -> (
          let Dune_file.Packages.{ changed; data } =
            Dune_file.Packages.update_dune_project_references renames sexps
          in
          match changed with
          | false -> Ok ()
          | true -> write_sexps dune_project_file data))

let postprocess_project ~keep ~disambiguation directory =
  let open Result.O in
  let is_dune_file path =
    let filename = Fpath.filename path in
    let matches =
      Base.List.mem ~equal:String.equal [ "dune"; "dune.inc" ] filename
    in
    Ok matches
  in
  let elements = `Sat is_dune_file in
  let dfp = Dune_file.Packages.init disambiguation in
  let renames = Dune_file.Packages.Map.empty in
  (* determine mappings first *)
  let* renames =
    Bos.OS.Path.fold ~elements
      (fun path renames ->
        let dune_project = path |> dune_project_of_dune |> if_no_dune_project in
        match preprocess_dune dfp ~dune_project ~keep ~renames path with
        | Ok (Some (sexps, renames)) -> (
            match write_sexps path sexps with
            | Ok () -> renames
            | Error msg ->
                Logs.err (fun l ->
                    l "Error while writing file %a: %a" Fpath.pp path
                      Rresult.R.pp_msg msg);
                renames)
        | Ok None -> renames
        | Error msg ->
            Logs.err (fun l ->
                l "Error while preprocessing: %a " Rresult.R.pp_msg msg);
            renames)
      renames [ directory ]
  in
  (* iterate over all dune files and rewrite where necessary *)
  let* rewritten =
    Bos.OS.Path.fold ~elements
      (fun path acc ->
        let* sexps = parse_sexps path in
        match sexps with
        | None -> acc
        | Some sexps -> (
            let dune_project =
              path |> dune_project_of_dune |> if_no_dune_project
            in
            let Dune_file.Packages.{ changed; data } =
              Dune_file.Packages.update_references ~dune_project renames sexps
            in
            match changed with
            | false -> acc
            | true ->
                Logs.debug (fun l ->
                    l "Rewriting %a to make names unique" Fpath.pp path);
                write_sexps path data))
      (Ok ()) [ directory ]
  in
  let* () = rewritten in
  let* () = rename_opam_files renames directory in
  let* () = rewrite_dune_project renames directory in
  Ok ()

let pull ?(trim_clone = false) ~global_state ~duniverse_dir src_dep =
  let open Result.O in
  let open Duniverse.Repo in
  let { dir; url; hashes; provided_packages = _; dune_packages } = src_dep in
  let output_dir = Fpath.(duniverse_dir / dir) in
  if is_in_universe_dir ~duniverse_dir ~output_dir then
    let url = Url.to_opam_url url in
    let open OpamProcess.Job.Op in
    Opam.pull_tree ~url ~hashes ~dir:output_dir global_state @@| fun result ->
    let* () = result in
    let* () = if trim_clone then do_trim_clone output_dir else Ok () in
    let* () =
      postprocess_project ~keep:dune_packages ~disambiguation:dir output_dir
    in
    Ok ()
  else
    let error =
      Rresult.R.error_msgf
        "Refusing to pull %s into directory %s as it is not inside the \
         directory %s"
        (Url.to_string url)
        (Fpath.to_string output_dir)
        (Fpath.to_string duniverse_dir)
    in
    Done error

let pull_source_dependencies ?trim_clone ~global_state ~duniverse_dir src_deps =
  let open Result.O in
  let jobs = !OpamStateConfig.r.dl_jobs in
  let* _ =
    OpamParallel.map ~jobs
      ~command:(pull ?trim_clone ~global_state ~duniverse_dir)
      src_deps
    |> Base.Result.all
  in
  let total = List.length src_deps in
  let pp_count = Pp.Styled.good Fmt.int in
  Logs.app (fun l ->
      l "Successfully pulled %a/%a repositories" pp_count total pp_count total);
  Ok ()

let mark_duniverse_content_as_vendored ~duniverse_dir =
  let open Result.O in
  let dune_file = Fpath.(duniverse_dir / "dune") in
  let content = Dune_file.Raw.duniverse_dune_content in
  Logs.debug (fun l ->
      l "Writing %a:\n %s" Pp.Styled.path dune_file
        (String.concat ~sep:"\n" content));
  let* () = Persist.write_lines_hum dune_file content in
  Logs.debug (fun l -> l "Successfully wrote %a" Pp.Styled.path dune_file);
  Ok ()

let pre_pull_clean_up ~full ~preserve_symlinks ~duniverse_dir duniverse =
  let open Result.O in
  match full with
  | true ->
      let* () =
        Bos.OS.Dir.delete ~must_exist:false ~recurse:true duniverse_dir
      in
      Ok []
  | false ->
      let* preserved =
        Result.List.map duniverse ~f:(fun { Duniverse.Repo.dir; _ } ->
            let directory = Fpath.(duniverse_dir / dir) in
            let* stat = Bos.OS.Path.symlink_stat directory in
            match (preserve_symlinks, stat.st_kind) with
            | true, S_LNK -> Ok (Some dir)
            | _, _ ->
                let* () =
                  Bos.OS.Dir.delete ~must_exist:false ~recurse:true directory
                in
                Ok None)
      in
      preserved |> List.filter_map ~f:Fun.id |> Result.ok

let duniverse_documentation =
  {|# duniverse

This folder contains vendored source code of the dependencies of the project,
created by the [opam-monorepo](https://github.com/ocamllabs/opam-monorepo)
tool. You can find the packages and versions that are included in this folder
in the `.opam.locked` files.

To update the packages do not modify the files and directories by hand, instead
use `opam-monorepo` to keep the lockfiles and directory contents accurate and
in sync:

```sh
opam monorepo lock 
opam monorepo pull
```

If you happen to include the `duniverse/` folder in your Git repository make
sure to commit all files:

```sh
git add -A duniverse/
```

For more information check out the homepage and manual of `opam-monorepo`.
|}

let write_duniverse_dir_documentation ~duniverse_dir =
  let open Result.O in
  let file_name = Fpath.v "README.md" in
  let readme_file = Fpath.(duniverse_dir // file_name) in
  let* written =
    Bos.OS.File.with_output readme_file
      (fun output () ->
        let content = Bytes.of_string duniverse_documentation in
        output (Some (content, 0, Bytes.length content)) |> Result.ok)
      ()
  in
  written

let filter_preserved ~preserved duniverse =
  List.filter
    ~f:(fun { Duniverse.Repo.dir; _ } -> not @@ List.mem dir ~set:preserved)
    duniverse

let duniverse ~full ~preserve_symlinks ~root ~global_state ~trim_clone duniverse
    =
  if Base.List.is_empty duniverse then Ok ()
  else
    let open Result.O in
    let duniverse_dir = Fpath.(root // Config.vendor_dir) in
    let* preserved =
      pre_pull_clean_up ~full ~preserve_symlinks ~duniverse_dir duniverse
    in
    let duniverse = filter_preserved ~preserved duniverse in
    let* _created = Bos.OS.Dir.create duniverse_dir in
    let* () = mark_duniverse_content_as_vendored ~duniverse_dir in
    let* () = write_duniverse_dir_documentation ~duniverse_dir in
    pull_source_dependencies ~global_state ~trim_clone ~duniverse_dir duniverse
