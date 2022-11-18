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
      | Git _, Other _ -> Ordering.to_int Lt
      | Other _, Git _ -> Ordering.to_int Gt
      | Git { repo; ref }, Git { repo = repo'; ref = ref' } -> (
          let c1 = String.compare repo repo' in
          match Ordering.of_int c1 with
          | Lt | Gt -> c1
          | Eq -> compare_ref ref ref')
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
    type t = {
      opam : OpamPackage.t;
      dev_repo : string;
      url : unresolved Url.t;
      hashes : OpamHash.t list;
      pinned : bool;
    }

    let equal t t' =
      OpamPackage.equal t.opam t'.opam
      && String.equal t.dev_repo t'.dev_repo
      && Url.equal Git.Ref.equal t.url t'.url
      && Bool.equal t.pinned t'.pinned

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
      match ps with
      | _ when is_base_package ps -> Ok None
      | { url_src = None; _ } | { dev_repo = None; _ } -> Ok None
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

    let is_pinned { pinned; _ } = pinned
  end

  type 'ref t = {
    dir : string;
    url : 'ref Url.t;
    hashes : OpamHash.t list;
    provided_packages : OpamPackage.t list;
  }

  let log_url_selection ~dev_repo ~packages ~reason =
    let url_to_string : unresolved Url.t -> string = function
      | Git { repo; ref } -> Printf.sprintf "%s#%s" repo ref
      | Other s -> s
    in
    let pp_package fmt { Package.opam = { name; version }; url; _ } =
      Format.fprintf fmt "%a.%a: %s" Opam.Pp.package_name name Opam.Pp.version
        version (url_to_string url)
    in
    let sep fmt () = Format.fprintf fmt "\n" in
    let pp_reason fmt = function
      | `Highest_version package ->
          Fmt.pf fmt
            "The url for the highest versioned package was selected: %a"
            pp_package package
      | `Pinned packages ->
          Fmt.pf fmt "The url for the pinned package(s) was selected: %a"
            Fmt.(list ~sep pp_package)
            packages
    in
    Logs.warn (fun l ->
        l
          "The following packages come from the same repository %s but are \
           associated with different URLs:\n\
           %a\n\
           %a"
          (Dev_repo.to_string dev_repo)
          (Fmt.list ~sep pp_package) packages pp_reason reason)

  module Unresolved_url_map = Map.Make (struct
    type t = unresolved Url.t

    let compare = Url.compare Git.Ref.compare
  end)

  let dir_name_from_dev_repo dev_repo =
    match Dev_repo.repo_name dev_repo with "dune" -> "dune_" | name -> name

  let from_packages ~dev_repo (packages : Package.t list) =
    let provided_packages = List.map packages ~f:(fun p -> p.Package.opam) in
    let dir = dir_name_from_dev_repo dev_repo in
    let urls =
      let add acc p =
        Unresolved_url_map.set acc p.Package.url p.Package.hashes
      in
      List.fold_left packages ~init:Unresolved_url_map.empty ~f:add
      |> Unresolved_url_map.bindings
    in
    match urls with
    | [ (url, hashes) ] -> { dir; url; hashes; provided_packages }
    | _ -> (
        (* If packages from the same repo were resolved to different URLs, we need to pick
           a single one. Here we decided to go with the one associated with the package
           that has the higher version. We need a better long term solution as this won't
           play nicely with pins for instance.
           The best solution here would be to use source trimming, so we can pull each individual
           package to its own directory and strip out all the unrelated source code but we would
           need dune to provide that feature. *)
        match List.filter ~f:Package.is_pinned packages with
        | [] ->
            let highest_version_package =
              List.max_exn packages ~compare:(fun p p' ->
                  OpamPackage.Version.compare p.Package.opam.version
                    p'.opam.version)
            in
            log_url_selection ~dev_repo ~packages
              ~reason:(`Highest_version highest_version_package);
            let url = highest_version_package.url in
            let hashes = highest_version_package.hashes in
            { dir; url; hashes; provided_packages }
        | pinned :: pinneds ->
            if
              not
                (List.for_all pinneds ~f:(fun p ->
                     String.equal pinned.Package.dev_repo p.Package.dev_repo
                     && (* not necessary? *)
                     Url.equal Git.Ref.equal pinned.url p.url))
            then failwith "multiple pinned packages for same dir";
            log_url_selection ~dev_repo ~packages
              ~reason:(`Pinned (pinned :: pinneds));
            let url = pinned.url in
            let hashes = pinned.hashes in
            { dir; url; hashes; provided_packages })

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
    && List.equal Opam.Hash.equal hashes hashes'
    && List.equal OpamPackage.equal provided_packages provided_packages'

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

let equal t t' = List.equal (Repo.equal Git.Ref.equal_resolved) t t'

let pp fmt t =
  let open Pp_combinators.Ocaml in
  (list (Repo.pp Git.Ref.pp_resolved)) fmt t

let dev_repo_map_from_packages packages =
  List.fold_left packages ~init:Dev_repo.Map.empty ~f:(fun acc pkg ->
      let key = Dev_repo.from_string pkg.Repo.Package.dev_repo in
      Dev_repo.Map.update acc key ~f:(function
        | Some pkgs -> Some (pkg :: pkgs)
        | None -> Some [ pkg ]))

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
  let* pkg_opts = Result.List.all results in
  let pkgs = List.filter_opt pkg_opts in
  let dev_repo_map = dev_repo_map_from_packages pkgs in
  let repos =
    Dev_repo.Map.fold dev_repo_map ~init:[]
      ~f:(fun ~key:dev_repo ~data:pkgs acc ->
        Repo.from_packages ~dev_repo pkgs :: acc)
  in
  Ok repos

let resolve ~resolve_ref t =
  Parallel.map ~f:(Repo.resolve ~resolve_ref) t |> Result.List.all
