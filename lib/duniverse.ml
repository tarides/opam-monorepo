open Import

type unresolved = Git.Ref.t
type resolved = Git.Ref.resolved

module Repo = struct
  module Url = struct
    type 'ref t = Git of { repo : string; ref : 'ref } | Other of string

    let equal equal_ref t t' =
      match (t, t') with
      | Git { repo; ref }, Git { repo = repo'; ref = ref' } ->
          String.equal repo repo' && equal_ref ref ref'
      | Other s, Other s' -> String.equal s s'
      | _ -> false

    let compare compare_ref t t' =
      match (t, t') with
      | Git _, Other _ -> Base.Ordering.to_int Base.Ordering.Less
      | Other _, Git _ -> Base.Ordering.to_int Base.Ordering.Greater
      | Git { repo; ref }, Git { repo = repo'; ref = ref' } -> (
          let c1 = String.compare repo repo' in
          match Base.Ordering.of_int c1 with
          | Base.Ordering.Less | Greater -> c1
          | Equal -> compare_ref ref ref')
      | Other s, Other s' -> String.compare s s'

    let pp pp_ref fmt t =
      let open Pp_combinators.Ocaml in
      match t with
      | Git { repo; ref } ->
          Format.fprintf fmt
            "@[<hov 2>Git@ @[<hov 2>{ repo = %a;@ ref = %a }@]@]" string repo
            pp_ref ref
      | Other s -> Format.fprintf fmt "@[<hov 2>Other@ %a@]" string s

    let opam_url_from_string s =
      OpamUrl.parse ~from_file:true ~handle_suffix:false s

    let to_string : resolved t -> string = function
      | Other s -> s
      | Git { repo; ref = { Git.Ref.commit; _ } } ->
          Printf.sprintf "%s#%s" repo commit

    let to_opam_url t = opam_url_from_string (to_string t)

    let from_opam_url opam_url =
      match Opam.Url.from_opam opam_url with
      | Opam.Url.Other s -> Ok (Other s)
      | Opam.Url.Git { repo; ref = Some commit } ->
          Ok (Git { repo; ref = { Git.Ref.t = commit; commit } })
      | _ -> Error (`Msg "Git URL must be resolved to a commit hash")
  end

  module Package = struct
    module Dev_repo = struct
      type t = string

      let equal a b =
        let a = a |> Uri.of_string |> Uri_utils.Normalized.of_uri in
        let b = b |> Uri.of_string |> Uri_utils.Normalized.of_uri in
        Uri_utils.Normalized.equal a b
    end

    type t = {
      opam : OpamPackage.t;
      dev_repo : Dev_repo.t;
      url : unresolved Url.t;
      hashes : OpamHash.t list;
      pinned : bool;
    }

    let equal t t' =
      OpamPackage.equal t.opam t'.opam
      && Dev_repo.equal t.dev_repo t'.dev_repo
      && Url.equal Git.Ref.equal t.url t'.url

    let pp fmt { opam; dev_repo; url; hashes; pinned } =
      let open Pp_combinators.Ocaml in
      Format.fprintf fmt
        "@[<hov 2>{ opam = %a;@ dev_repo = %a;@ url = %a;@ hashes = %a;@ \
         pinned = %b; }@]"
        Opam.Pp.raw_package opam string dev_repo (Url.pp Git.Ref.pp) url
        (list Opam.Pp.hash) hashes pinned

    let from_package_summary ~get_default_branch ps =
      let open Opam.Package_summary in
      let open Result.O in
      let url ourl =
        match (ourl : Opam.Url.t) with
        | Other s -> Ok (Url.Other s)
        | Git { repo; ref = Some ref } -> Ok (Url.Git { repo; ref })
        | Git { repo; ref = None } ->
            let* ref = get_default_branch repo in
            Ok (Url.Git { repo; ref })
      in
      match is_safe_package ps with
      | true -> Ok None
      | false -> (
          match ps with
          | {
           url_src = Some url_src;
           package;
           dev_repo = Some dev_repo;
           hashes;
           pinned;
           _;
          } ->
              let* url = url url_src in
              Ok (Some { opam = package; dev_repo; url; hashes; pinned })
          | { dev_repo = None; package; _ } ->
              Logs.warn (fun l ->
                  l
                    "Package %a has no dev-repo specified, but it needs a \
                     dev-repo to be successfully included in the duniverse."
                    Opam.Pp.package package);
              Ok None
          | _ -> Ok None)

    let is_pinned { pinned; _ } = pinned
  end

  type 'ref t = {
    dir : string;
    url : 'ref Url.t;
    hashes : OpamHash.t list;
    provided_packages : OpamPackage.t list;
  }

  module Unresolved_url_map = Map.Make (struct
    type t = unresolved Url.t

    let compare = Url.compare Git.Ref.compare
  end)

  let dir_name_from_dev_repo dev_repo =
    Dev_repo.repo_name dev_repo
    |> Base.Result.map ~f:(function "dune" -> "dune_" | name -> name)

  let log_url_selection ~dev_repo ~packages pinned_packages =
    let url_to_string : unresolved Url.t -> string = function
      | Git { repo; ref } -> Printf.sprintf "%s#%s" repo ref
      | Other s -> s
    in
    let pp_package fmt { Package.opam; url; _ } =
      Fmt.pf fmt "%a: %s" Opam.Pp.package opam (url_to_string url)
    in
    let pp_packages = Fmt.(list ~sep:(any "\n") pp_package) in
    Logs.warn (fun l ->
        l
          "The following packages come from the same repository %s but are \
           associated with different URLs:\n\
           %a\n\
           The URL for the pinned package(s) was selected: %a"
          (Dev_repo.to_string dev_repo)
          pp_packages packages pp_packages pinned_packages)

  let from_packages ~dev_repo (packages : Package.t list) =
    let open Result.O in
    let provided_packages = List.map packages ~f:(fun p -> p.Package.opam) in
    let* dir = dir_name_from_dev_repo dev_repo in
    let urls =
      let add acc p =
        Unresolved_url_map.set acc p.Package.url p.Package.hashes
      in
      List.fold_left packages ~init:Unresolved_url_map.empty ~f:add
      |> Unresolved_url_map.bindings
    in
    match urls with
    | [ (url, hashes) ] -> Ok { dir; url; hashes; provided_packages }
    | _ -> (
        match List.filter ~f:Package.is_pinned packages with
        | [] ->
            let pp_hash = Fmt.of_to_string OpamHash.to_string in
            (* this should not happen because we passed extra constraints
                 to the opam solver to avoid this situation *)
            Fmt.failwith
              "The following packages have the same `dev-repo' but are using  \
               different versions of the archive tarballs:\n\
               %a\n\
               This should not happen, please report the issue to \
               https://github.com/tarides/opam-monorepo.\n\
               %!"
              Fmt.Dump.(list (pair (Url.pp string) (list pp_hash)))
              urls
        | first_pin :: _ as pins ->
            log_url_selection ~dev_repo ~packages pins;
            let url = first_pin.url in
            let hashes = first_pin.hashes in
            Ok { dir; url; hashes; provided_packages })

  let equal equal_ref t t' =
    let { dir; url; hashes; provided_packages } = t in
    let {
      dir = dir';
      url = url';
      hashes = hashes';
      provided_packages = provided_packages';
    } =
      t'
    in
    String.equal dir dir'
    && Url.equal equal_ref url url'
    && Base.List.equal Opam.Hash.equal hashes hashes'
    && Base.List.equal OpamPackage.equal provided_packages provided_packages'

  let pp pp_ref fmt { dir; url; hashes; provided_packages } =
    let open Pp_combinators.Ocaml in
    Format.fprintf fmt
      "@[<hov 2>{ dir = %a;@ url = %a;@ hashes = %a;@ provided_packages = %a \
       }@]"
      string dir (Url.pp pp_ref) url (list Opam.Pp.hash) hashes
      (list Opam.Pp.raw_package) provided_packages

  let resolve ~resolve_ref ({ url; _ } as t) =
    let open Result.O in
    match (url : unresolved Url.t) with
    | Git { repo; ref } ->
        let* resolved_ref = resolve_ref ~repo ~ref in
        let resolved_url = Url.Git { repo; ref = resolved_ref } in
        Ok { t with url = resolved_url }
    | Other s -> Ok { t with url = Other s }
end

type t = resolved Repo.t list

let equal t t' = Base.List.equal (Repo.equal Git.Ref.equal_resolved) t t'

let pp fmt t =
  let open Pp_combinators.Ocaml in
  (list (Repo.pp Git.Ref.pp_resolved)) fmt t

let dev_repo_map_from_packages packages =
  List.fold_left packages ~init:Dev_repo.Map.empty ~f:(fun acc pkg ->
      let key = Dev_repo.from_string pkg.Repo.Package.dev_repo in
      Dev_repo.Map.update acc key ~f:(function
        | Some pkgs -> Some (pkg :: pkgs)
        | None -> Some [ pkg ]))

(* converts a map from dev-repos to lists of packages to a list of repos after
   checking for errors *)
let dev_repo_package_map_to_repos dev_repo_package_map =
  let open Result.O in
  let dev_repo_to_repo_result_map =
    Dev_repo.Map.mapi dev_repo_package_map ~f:(fun dev_repo pkgs ->
        Repo.from_packages ~dev_repo pkgs)
  in
  (* Handle any errors relating to individual repos but maintain the
     association between dev-repo and repo for further error checking. *)
  let* repo_by_dev_repo =
    Dev_repo.Map.bindings dev_repo_to_repo_result_map
    |> List.map ~f:(fun (dev_repo, repo_result) ->
           Result.map (fun repo -> (dev_repo, repo)) repo_result)
    |> Base.Result.all
  in
  (* Detect the case where multiple different dev-repos are associated with the
     same duniverse directory. *)
  let* () =
    let dev_repos_by_dir =
      List.fold_left repo_by_dev_repo ~init:String.Map.empty
        ~f:(fun acc (dev_repo, (repo : _ Repo.t)) ->
          String.Map.update acc repo.dir ~f:(function
            | None -> Some [ dev_repo ]
            | Some dev_repos -> Some (dev_repo :: dev_repos)))
      |> String.Map.bindings
    in
    match
      List.find_opt dev_repos_by_dir ~f:(fun (_, dev_repos) ->
          List.length dev_repos > 1)
    with
    | None -> Ok ()
    | Some (dir, dev_repos) ->
        let dir_path = Fpath.(Config.vendor_dir / dir) in
        let message_first_line =
          Fmt.str "Multiple dev-repos would be vendored into the directory: %a"
            Fpath.pp dir_path
        in
        let dev_repos_pp =
          Fmt.list
            ~sep:Fmt.(const char '\n')
            (fun ppf dev_repo -> Fmt.pf ppf "- %a" Dev_repo.pp dev_repo)
        in
        Rresult.R.error_msgf "%s\nDev-repos:\n%a" message_first_line
          dev_repos_pp dev_repos
  in
  Ok (List.map ~f:snd repo_by_dev_repo)

let from_dependency_entries ~get_default_branch dependencies =
  let open Result.O in
  let summaries =
    List.filter_map
      ~f:(fun Opam.Dependency_entry.{ package_summary; vendored } ->
        match vendored with true -> Some package_summary | false -> None)
      dependencies
  in
  let results =
    List.map
      ~f:(Repo.Package.from_package_summary ~get_default_branch)
      summaries
  in
  let* pkg_opts = Base.Result.all results in
  let pkgs = Base.List.filter_opt pkg_opts in
  let dev_repo_map = dev_repo_map_from_packages pkgs in
  dev_repo_package_map_to_repos dev_repo_map

let resolve ~resolve_ref t =
  Parallel.map ~f:(Repo.resolve ~resolve_ref) t |> Base.Result.all
