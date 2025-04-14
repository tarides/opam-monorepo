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

let pull ?(trim_clone = false) ~global_state ~duniverse_dir src_dep =
  let open Result.O in
  let open Duniverse.Repo in
  let { dir; url; hashes; _ } = src_dep in
  let output_dir = Fpath.(duniverse_dir / dir) in
  if is_in_universe_dir ~duniverse_dir ~output_dir then
    let url = Url.to_opam_url url in
    let open OpamProcess.Job.Op in
    Opam.pull_tree ~url ~hashes ~dir:output_dir global_state @@| fun result ->
    let* () = result in
    if trim_clone then do_trim_clone output_dir else Ok ()
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
    |> List.fold_left
      ~f:(fun acc r ->
         match acc, r with
         | Error _ as e, _ | Ok _, (Error _ as e) -> e
         | Ok acc, Ok r -> Ok (r :: acc))
      ~init:(Ok [])
    |> List.rev
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
  if duniverse = [] then Ok ()
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
