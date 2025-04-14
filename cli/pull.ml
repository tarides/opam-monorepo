open Import

let min_dune_ver = D.Dune_file.Lang.duniverse_minimum_version

let should_update_lang ~yes () =
  Prompt.confirm
    ~question:(fun l -> l "Should I update your dune-project?")
    ~yes

let log_version_update ~dune_project_path =
  Common.Logs.app (fun l ->
      l "Setting dune language version to %a in %a" D.Dune_file.Lang.pp_version
        min_dune_ver D.Pp.Styled.path dune_project_path)

let suggest_updating_version ~yes ~version ~dune_project_path ~content =
  let pp_current = D.Pp.Styled.bad D.Dune_file.Lang.pp_version in
  let pp_required = D.Pp.Styled.good D.Dune_file.Lang.pp_version in
  Common.Logs.app (fun l ->
      l "You are using version %a of the dune language" pp_current version);
  Common.Logs.app (fun l ->
      l "Duniverse requires version %a or above" pp_required min_dune_ver);
  if should_update_lang ~yes () then (
    let updated = D.Dune_file.Lang.update ~version:min_dune_ver content in
    log_version_update ~dune_project_path;
    Bos.OS.File.write dune_project_path updated)
  else Ok ()

let check_dune_lang_version ~yes ~root =
  let open Result.O in
  let dune_project_path = D.Project.dune_project root in
  Logs.debug (fun l ->
      l "Looking for dune-project file in %a" D.Pp.Styled.path dune_project_path);
  let* found_dune_project = Bos.OS.File.exists dune_project_path in
  if found_dune_project then
    let* content = Bos.OS.File.read dune_project_path in
    match D.Dune_file.Lang.from_content content with
    | Error (`Msg msg) ->
        Logs.warn (fun l -> l "%s" msg);
        Ok ()
    | Ok version -> (
        match
          D.Dune_file.Lang.(compare_version version duniverse_minimum_version)
        with
        | n when n >= 0 -> Ok ()
        | _ ->
            suggest_updating_version ~yes ~version ~dune_project_path ~content)
  else (
    Logs.debug (fun l -> l "No dune-project found");
    Ok ())

let run (`Yes yes) (`Root root) (`Lockfile explicit_lockfile)
    (`Keep_git_dir keep_git_dir) (`Keep_symlinked_dir keep_symlinked_dir)
    (`Duniverse_repos duniverse_repos) () =
  let open Result.O in
  let* lockfile = Common.find_lockfile ~explicit_lockfile root in
  let* duniverse = D.Lockfile.to_duniverse lockfile in
  match duniverse with
  | [] ->
      Common.Logs.app (fun l ->
          l "No dependencies to pull, there's nothing to be done here!");
      Ok ()
  | duniverse ->
      let full =
        match (duniverse_repos, keep_symlinked_dir) with
        | None, false -> true
        | _ -> false
      in
      let* duniverse =
        Common.filter_duniverse ~to_consider:duniverse_repos duniverse
      in
      let* () = check_dune_lang_version ~yes ~root in
      OpamGlobalState.with_ `Lock_none @@ fun global_state ->
      let* locked_ocaml_version =
        D.Lockfile.ocaml_version lockfile
        |> Option.to_result ~none:(`Msg "OCaml compiler not in lockfile")
      in
      let* pulled =
        D.Pull.duniverse ~global_state ~root ~full
          ~preserve_symlinks:keep_symlinked_dir ~trim_clone:(not keep_git_dir)
          duniverse
      in
      let* switch_ocaml_version =
        let+ version = D.Exec.ocaml_version () in
        OpamPackage.Version.of_string (Ocaml_version.to_string version)
      in
      (match
         D.Opam.version_is_at_least locked_ocaml_version switch_ocaml_version
       with
      | true -> ()
      | false ->
          Logs.warn (fun l ->
              l
                "Pulling duniverse succeeded but the version of the OCaml \
                 compiler does not match the lockfile: %a in lockfile, yet %a \
                 in switch.\n\
                 You might want to change the compiler version of your switch \
                 accordingly:\n\
                 opam install %a.%a --update-invariant" D.Opam.Pp.version
                locked_ocaml_version D.Opam.Pp.version switch_ocaml_version
                D.Opam.Pp.package_name D.Config.compiler_package_name
                D.Opam.Pp.version locked_ocaml_version));
      Ok pulled

let info =
  let open Cmdliner in
  let doc = "fetch the dependencies sources as specified by the lockfile" in
  let exits = Common.exit_codes in
  let man =
    [
      `S Manpage.s_description;
      `P
        "This command fetches the sources of the dependencies according to the \
         lockfile calculated with $(b,opam monorepo lock), stores them in the \
         $(b,duniverse/) directory in the repository and set it up so they are \
         treated as vendored code by dune.";
      `P
        "The previous content of the $(b,duniverse/) folder is deleted upon \
         calling this command, unless a subset of repositories to pull is \
         explicitly passed on the command line.";
    ]
  in
  Cmd.info "pull" ~doc ~exits ~man

let term =
  Common.Term.result_to_exit
    Cmdliner.Term.(
      const run $ Common.Arg.yes $ Common.Arg.root $ Common.Arg.lockfile
      $ Common.Arg.keep_git_dir $ Common.Arg.keep_symlinked_dir
      $ Common.Arg.duniverse_repos $ Common.Arg.setup_logs ())

let cmd = Cmdliner.Cmd.v info term
