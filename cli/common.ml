open Import

module Arg = struct
  let named f = Cmdliner.Term.(app (const f))
  let fpath = Cmdliner.Arg.conv ~docv:"PATH" (Fpath.of_string, Fpath.pp)

  let root =
    let doc =
      "Use this directory as the project root instead of the current working \
       directory."
    in
    named
      (fun x -> `Root x)
      Cmdliner.Arg.(
        value
        & opt fpath (Fpath.v (Sys.getcwd ()))
        & info [ "r"; "root" ] ~docv:"DIR" ~doc)

  let lockfile =
    let doc =
      "Path to the lockfile to use or generate. Defaults \
       $(b,<project-name>.opam.locked)"
    in
    named
      (fun x -> `Lockfile x)
      Cmdliner.Arg.(
        value & opt (some fpath) None & info [ "l"; "lockfile" ] ~doc)

  let yes =
    let doc = "Do not prompt for confirmation and always assume yes" in
    named
      (fun x -> `Yes x)
      Cmdliner.Arg.(value & flag & info [ "y"; "yes" ] ~doc)

  let non_empty_list_opt =
    Cmdliner.Term.const (function [] -> None | l -> Some l)

  let keep_git_dir =
    let doc =
      "Keep the `.git' directory if the pulled vendored source is a git \
       repository."
    in
    named
      (fun x -> `Keep_git_dir x)
      Cmdliner.Arg.(value & flag & info [ "keep-git-dir" ] ~doc)

  let keep_symlinked_dir =
    let doc =
      "Preserve directories that are symlinked. This can be useful when \
       working on the dependencies of projects."
    in
    named
      (fun x -> `Keep_symlinked_dir x)
      Cmdliner.Arg.(value & flag & info [ "keep-symlinked-dir" ] ~doc)

  let duniverse_repos =
    let open Cmdliner in
    let docv = "REPOSITORIES" in
    let doc =
      "The list of $(docv) from your duniverse to process. If none is \
       provided, all will be processed."
    in
    named
      (fun x -> `Duniverse_repos x)
      Term.(
        non_empty_list_opt
        $ Arg.(value & pos_all string [] & info ~doc ~docv []))

  let thread_safe_reporter reporter =
    let lock = Mutex.create () in
    let { Logs.report } = reporter in
    let oui src level ~over k msgf =
      Mutex.lock lock;
      let x = report src level ~over k msgf in
      Mutex.unlock lock;
      x
    in
    Logs.{ report = oui }

  let setup_logs () =
    Printexc.record_backtrace true;
    let setup_log style_renderer level =
      Fmt_tty.setup_std_outputs ?style_renderer ();
      Logs.set_level level;
      Logs.set_reporter (thread_safe_reporter (Logs_fmt.reporter ()))
    in
    let global_option_section = "COMMON OPTIONS" in
    let open Cmdliner.Term in
    const setup_log
    $ Fmt_cli.style_renderer ~docs:global_option_section ()
    $ Logs_cli.level ~docs:global_option_section ()

  let version =
    match Build_info.V1.version () with
    | None -> "n/a"
    | Some v -> Build_info.V1.Version.to_string v
end

let regular_error_exit =
  Cmdliner.Cmd.Exit.info ~doc:"on regular opam-monorepo errors" 1

let exit_codes = regular_error_exit :: Cmdliner.Cmd.Exit.defaults

module Term = struct
  let result_to_exit term =
    let to_exit result =
      match result with
      | Ok () -> 0
      | Error (`Msg msg) ->
          Logs.err (fun l -> l "%s" msg);
          Cmdliner.Cmd.Exit.info_code regular_error_exit
    in
    Cmdliner.Term.(const to_exit $ term)
end

module Logs = struct
  let app ?src f =
    Logs.app ?src (fun l ->
        f (fun ?header ?tags fmt ->
            l ?header ?tags ("%a" ^^ fmt) D.Pp.Styled.header ()))
end

(** Filters the duniverse according to the CLI provided list of repos *)
let filter_duniverse ~to_consider (duniverse : D.Duniverse.t) =
  match to_consider with
  | None -> Ok duniverse
  | Some to_consider -> (
      let repos_map =
        String.Map.of_list_map_exn duniverse ~f:(fun src -> (src.dir, src))
      in
      let unmatched, found =
        List.partition_map to_consider ~f:(fun asked ->
            match String.Map.find repos_map asked with
            | None -> Either.Left asked
            | Some found -> Either.Right found)
      in
      match unmatched with
      | [] -> Ok found
      | _ ->
          let sep fmt () = Fmt.pf fmt " " in
          Rresult.R.error_msgf
            "The following repos are not in your duniverse: %a"
            Fmt.(list ~sep string)
            unmatched)

let find_lockfile_aux ~explicit_lockfile repo =
  let open Result.O in
  match explicit_lockfile with
  | Some file -> Ok file
  | None -> (
      let* local_lockfiles = D.Project.local_lockfiles repo in
      match local_lockfiles with
      | [] ->
          Rresult.R.error_msg
            "No lockfile: try running `opam monorepo lock` first"
      | [ file ] -> Ok file
      | lockfile_paths ->
          let sep = Fmt.(const char ' ') in
          Rresult.R.error_msgf
            "Found several lockfiles: %a\n\
             Please pick one explicitly using the %a option"
            (Fmt.list ~sep D.Pp.Styled.path)
            lockfile_paths
            Fmt.(styled `Bold string)
            "--lockfile")

let find_lockfile ~explicit_lockfile ?(quiet = false) root =
  let open Result.O in
  find_lockfile_aux ~explicit_lockfile root >>= fun file ->
  if not quiet then
    Logs.app (fun l -> l "Using lockfile %a" D.Pp.Styled.path file);
  D.Lockfile.load ~opam_monorepo_cwd:root ~file
