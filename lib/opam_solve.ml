open Import

module type BASE_CONTEXT = sig
  include Opam_0install.S.CONTEXT

  type input

  val is_pinned : t -> OpamTypes.name -> bool

  val create :
    ?test:OpamPackage.Name.Set.t ->
    constraints:OpamFormula.version_constraint OpamTypes.name_map ->
    input ->
    t
end

module type OPAM_MONOREPO_CONTEXT = sig
  type input
  type base_rejection
  type r = Non_dune | No_cross_compile | Base_rejection of base_rejection

  include Opam_0install.S.CONTEXT with type rejection = r

  val is_pinned : t -> OpamTypes.name -> bool

  val create :
    ?install_test_deps_for:OpamPackage.Name.Set.t ->
    ?opam_provided:OpamPackage.Name.Set.t ->
    ?require_dune:bool ->
    allow_jbuilder:bool ->
    require_cross_compile:bool ->
    preferred_versions:OpamTypes.version OpamPackage.Name.Map.t ->
    fixed_packages:
      (OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
    constraints:OpamFormula.version_constraint OpamTypes.name_map ->
    input ->
    t

  val opam_file : t -> OpamPackage.t -> (OpamFile.OPAM.t, Rresult.R.msg) result
  (** Convenience function to return the opam file associated to a pkg
    in the given context.
    Takes into account local packages an pin-depends. *)
end

module Opam_monorepo_context (Base_context : BASE_CONTEXT) :
  OPAM_MONOREPO_CONTEXT
    with type base_rejection = Base_context.rejection
     and type input = Base_context.input = struct
  type base_rejection = Base_context.rejection
  type input = Base_context.input

  type t = {
    base_context : Base_context.t;
    fixed_packages :
      (OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t;
    allow_jbuilder : bool;
    require_dune : bool;
    opam_provided : OpamPackage.Name.Set.t;
    require_cross_compile : bool;
    preferred_versions : OpamTypes.version OpamPackage.Name.Map.t;
  }

  let is_pinned { base_context; fixed_packages; _ } name =
    Base_context.is_pinned base_context name
    || OpamPackage.Name.Map.mem name fixed_packages

  type r =
    | Non_dune
    | No_cross_compile
    | Base_rejection of Base_context.rejection

  type rejection = r

  let pp_rejection fmt = function
    | Non_dune -> Fmt.pf fmt "Doesn't build with dune"
    | No_cross_compile -> Fmt.pf fmt "Does not cross compile"
    | Base_rejection r -> Base_context.pp_rejection fmt r

  let create ?install_test_deps_for
      ?(opam_provided = OpamPackage.Name.Set.empty) ?(require_dune = true)
      ~allow_jbuilder ~require_cross_compile ~preferred_versions ~fixed_packages
      ~constraints input =
    let base_context =
      Base_context.create ?test:install_test_deps_for ~constraints input
    in
    {
      base_context;
      fixed_packages;
      allow_jbuilder;
      require_dune;
      opam_provided;
      require_cross_compile;
      preferred_versions;
    }

  let validate_candidate
      { allow_jbuilder; require_cross_compile; require_dune; _ } ~name ~version
      opam_file =
    (* this function gets called way too often.. memoize? *)
    let pkg = OpamPackage.create name version in
    let depends = OpamFile.OPAM.depends opam_file in
    let depopts = OpamFile.OPAM.depopts opam_file in
    let uses_dune =
      Opam.depends_on_dune ~allow_jbuilder depends
      || Opam.depends_on_dune ~allow_jbuilder depopts
    in
    (* is_safe_package doesn't care about ~pinned, so we use a dummy
       value here. *)
    let summary = Opam.Package_summary.from_opam pkg ~pinned:false opam_file in
    let is_valid_dune_wise =
      Opam.Package_summary.is_safe_package summary
      || (not require_dune) || uses_dune
    in
    match is_valid_dune_wise with
    | false -> Error Non_dune
    | true
      when (not require_cross_compile) || Opam.has_cross_compile_tag opam_file
      ->
        Ok opam_file
    | true -> Error No_cross_compile

  let rec remove_opam_provided_from_formula opam_provided filtered_formula :
      OpamTypes.filtered_formula =
    match filtered_formula with
    | OpamFormula.Atom (name, _) as atom -> (
        match OpamPackage.Name.Set.mem name opam_provided with
        | true ->
            Logs.debug (fun l ->
                l "Removed %a from formula as it is opam-provided"
                  Opam.Pp.package_name name);
            OpamFormula.Empty
        | false -> atom)
    | OpamFormula.And (left, right) -> (
        let left = remove_opam_provided_from_formula opam_provided left in
        let right = remove_opam_provided_from_formula opam_provided right in
        match (left, right) with
        | Empty, right -> right
        | left, Empty -> left
        | left, right -> OpamFormula.And (left, right))
    | OpamFormula.Or (left, right) -> (
        let left = remove_opam_provided_from_formula opam_provided left in
        let right = remove_opam_provided_from_formula opam_provided right in
        match (left, right) with
        | Empty, right -> right
        | left, Empty -> left
        | left, right -> OpamFormula.Or (left, right))
    | otherwise -> otherwise

  let remove_opam_provided_from_dependencies opam_provided opam_file =
    let depends = OpamFile.OPAM.depends opam_file in
    let depends = remove_opam_provided_from_formula opam_provided depends in
    OpamFile.OPAM.with_depends depends opam_file

  let filter_candidates t name versions =
    List.map
      ~f:(fun (version, result) ->
        match result with
        | Error r -> (version, Error (Base_rejection r))
        | Ok opam_file ->
            let res = validate_candidate t ~name ~version opam_file in
            (version, res))
      versions

  let remove_opam_provided ~opam_provided versions =
    match OpamPackage.Name.Set.is_empty opam_provided with
    | true -> versions
    | false ->
        versions
        |> List.map ~f:(fun (version, result) ->
               match result with
               | Error _ as e -> (version, e)
               | Ok opam_file ->
                   let opam_file =
                     remove_opam_provided_from_dependencies opam_provided
                       opam_file
                   in
                   (version, Ok opam_file))

  let demote_candidates_to_avoid versions =
    let regular, avoid, broken =
      List.fold_left versions ~init:([], [], [])
        ~f:(fun (regular, avoid, broken) ((_, opam_res) as e) ->
          match opam_res with
          | Ok opam ->
              if Opam.avoid_version opam then (regular, e :: avoid, broken)
              else (e :: regular, avoid, broken)
          | Error _ -> (regular, avoid, e :: broken))
    in
    List.rev_append regular (List.rev_append avoid broken)

  let promote_version version candidates =
    match version with
    | None -> candidates
    | Some preferred_version ->
        let rec move_version_first acc l =
          match l with
          | [] -> candidates
          | ((version, _opam) as candidate) :: tl ->
              if OpamPackage.Version.equal version preferred_version then
                candidate :: List.rev_append acc tl
              else move_version_first (candidate :: acc) tl
        in
        move_version_first [] candidates

  let candidate_cross_compile (_version, opam_res) : bool =
    match opam_res with
    | Error _ -> false
    | Ok opam_file -> Opam.has_cross_compile_tag opam_file

  (* this is a hack - as we don't have a global view of the package
     universe, we build the dev_repo table lazily, whenever the 0install
     solver request more information about a package (by calling
     [Solver.candidates]. *)
  let dev_repos = Dev_repo.Tbl.create 13

  let hashes pkg =
    match OpamFile.OPAM.url pkg with
    | None -> []
    | Some url -> OpamFile.URL.checksum url

  (* build the list of conflicts by removing packages with the same hash *)
  let conflicts_with_same_dev_repo_but_a_different_hash =
    let memo = Hashtbl.create 13 in
    fun (name, version, hashes) pkgs ->
      match Hashtbl.find_opt memo (name, version) with
      | Some r -> r
      | None ->
          let r =
            match hashes with
            | [] -> (* pinned package or virtual package *) []
            | _ ->
                let same_hash h = List.exists ~f:(fun h' -> h = h') hashes in
                let conflict (n, _, hs) =
                  (* remove self conflicts *)
                  n <> name
                  && (* remove packages with the same dev-repo and the same hash *)
                  not (List.exists ~f:same_hash hs)
                in
                List.filter ~f:conflict pkgs
          in
          Hashtbl.add memo (name, version) r;
          r

  let with_conflict t pkg =
    if is_pinned t (OpamFile.OPAM.name pkg) then
      (* skip conflicts if a package is in pin-depends, listed on the
         CLI or pinned in the local switch. *)
      pkg
    else
      let name = OpamFile.OPAM.name pkg in
      let version = OpamFile.OPAM.version pkg in
      let hashes = hashes pkg in
      let entry = (name, version, hashes) in
      match OpamFile.OPAM.dev_repo pkg with
      | None -> pkg
      | Some dev_repo ->
          let dev_repo = Dev_repo.from_string (OpamUrl.to_string dev_repo) in
          let in_conflicts =
            match Dev_repo.Tbl.find_all dev_repos dev_repo with
            | [] ->
                Dev_repo.Tbl.add dev_repos dev_repo entry;
                []
            | conflicts ->
                if not (List.mem entry ~set:conflicts) then
                  Dev_repo.Tbl.add dev_repos dev_repo entry;
                (* remove packages from the same repo *)
                conflicts_with_same_dev_repo_but_a_different_hash entry
                  conflicts
          in
          let conflicts =
            in_conflicts
            |> List.map ~f:(fun (name, version, _) ->
                   let version =
                     let open OpamTypes in
                     let v = OpamPackage.Version.to_string version in
                     OpamFormula.Atom (Constraint (`Eq, FString v))
                   in
                   OpamFormula.Atom (name, version))
            |> OpamFormula.ors
          in
          OpamFile.OPAM.with_conflicts conflicts pkg

  let add_url_conflicts t pkgs =
    List.map
      ~f:(fun (v, pkg) ->
        match pkg with
        | Error _ -> (v, pkg)
        | Ok pkg -> (v, Ok (with_conflict t pkg)))
      pkgs

  let candidates t name =
    match OpamPackage.Name.Map.find_opt name t.fixed_packages with
    | Some (version, opam_file) ->
        let opam_file =
          remove_opam_provided_from_dependencies t.opam_provided opam_file
        in
        [ (version, Ok opam_file) ]
    | None ->
        let candidates = Base_context.candidates t.base_context name in
        let require_cross_compile =
          t.require_cross_compile
          && List.exists ~f:candidate_cross_compile candidates
        in
        let preferred_version =
          OpamPackage.Name.Map.find_opt name t.preferred_versions
        in
        filter_candidates { t with require_cross_compile } name candidates
        |> remove_opam_provided ~opam_provided:t.opam_provided
        |> demote_candidates_to_avoid
        |> promote_version preferred_version
        |> add_url_conflicts t

  let user_restrictions { base_context; _ } name =
    Base_context.user_restrictions base_context name

  let filter_deps { base_context; _ } pkg formula =
    Base_context.filter_deps base_context pkg formula

  let opam_file t pkg =
    let name = OpamPackage.name pkg in
    let candidates = candidates t name in
    let version = OpamPackage.version pkg in
    let res =
      List.find_map candidates ~f:(fun (v, opam_file) ->
          if OpamPackage.Version.equal v version then Some opam_file else None)
    in
    match res with
    | None -> Rresult.R.error_msgf "No such package %a" Opam.Pp.package pkg
    | Some (Ok opam_file) -> Ok opam_file
    | Some (Error rejection) ->
        Rresult.R.error_msgf "Package %a rejected: %a" Opam.Pp.package pkg
          pp_rejection rejection
end

exception Pinned_local_package

let relop_equal relop1 relop2 =
  match (relop1, relop2) with
  | `Eq, `Eq | `Neq, `Neq | `Geq, `Geq | `Gt, `Gt | `Leq, `Leq | `Lt, `Lt ->
      true
  | _, _ -> false

let version_constraint_equal (relop1, v1) (relop2, v2) =
  relop_equal relop1 relop2 && OpamPackage.Version.equal v1 v2

(* variant of [safe_add] that succeeds when the key/value pair to be added
   already exists, otherwise same semantics as [safe_add] *)
let name_map_duplicate_safe_add key value map =
  match OpamPackage.Name.Map.find_opt key map with
  | None -> OpamPackage.Name.Map.add key value map
  | Some existing -> (
      match version_constraint_equal existing value with
      | true -> map
      | false ->
          failwith
            (Printf.sprintf "duplicate differing entry %s"
               (OpamPackage.Name.to_string key)))

let constraints ~required_packages ~ocaml_version =
  let no_constraints = OpamPackage.Name.Map.empty in
  let constraints =
    OpamPackage.Set.fold
      (fun package constraints ->
        let key = OpamPackage.name package in
        let value = (`Eq, OpamPackage.version package) in
        OpamPackage.Name.Map.safe_add key value constraints)
      required_packages no_constraints
  in
  match ocaml_version with
  | None -> constraints
  | Some version ->
      let key = OpamPackage.Name.of_string "ocaml" in
      let value = (`Eq, OpamPackage.Version.of_string version) in
      name_map_duplicate_safe_add key value constraints

let mk_request ~allow_compiler_variants packages =
  let package_names = OpamPackage.Name.Set.elements packages in
  if allow_compiler_variants then package_names
  else
    (* We add ocaml-base-compiler to the solver request to prevent it
       from selecting a version of OCaml that hasn't been officially
       released yet but that exists in opam with variants such as
       ocaml-variants.x+trunk *)
    let base_compiler = OpamPackage.Name.of_string "ocaml-base-compiler" in
    base_compiler :: package_names

let depend_on_compiler_variants local_packages =
  OpamPackage.Name.Map.exists
    (fun _name (_version, opam_file) ->
      let depends = OpamFile.OPAM.depends opam_file in
      Opam.depends_on_compiler_variants depends)
    local_packages

let fixed_packages ~local_packages ~pin_depends =
  try
    Ok
      (OpamPackage.Name.Map.union
         (fun _local _pin -> raise Pinned_local_package)
         local_packages pin_depends)
  with Pinned_local_package ->
    Rresult.R.error_msg
      "You have a locally defined package in a pin-depends field of another \
       locally defined package"

module type OPAM_MONOREPO_SOLVER = sig
  type input
  type diagnostics

  val calculate :
    build_only:bool ->
    allow_jbuilder:bool ->
    require_cross_compile:bool ->
    preferred_versions:OpamTypes.version OpamPackage.Name.Map.t ->
    local_opam_files:
      (OpamTypes.version * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
    target_packages:OpamPackage.Name.Set.t ->
    opam_provided:OpamPackage.Name.Set.t ->
    pin_depends:(OpamTypes.version * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
    ?ocaml_version:string ->
    input ->
    ( Opam.Dependency_entry.t list,
      [> `Diagnostics of diagnostics | `Msg of string ] )
    result

  val diagnostics_message : verbose:bool -> diagnostics -> [> `Msg of string ]
  val not_buildable_with_dune : diagnostics -> OpamPackage.Name.t list

  val unavailable_versions_due_to_constraints :
    diagnostics -> (OpamPackage.Name.t * OpamFormula.version_formula) list
end

module Make_solver (Context : OPAM_MONOREPO_CONTEXT) :
  OPAM_MONOREPO_SOLVER with type input = Context.input = struct
  type input = Context.input

  module Solver = Opam_0install.Solver.Make (Context)

  let build_context ~build_only ~allow_jbuilder ~require_cross_compile
      ~preferred_versions ?opam_provided ?require_dune
      ?(vendored_packages = OpamPackage.Set.empty) ~ocaml_version
      ~local_packages ~pin_depends ~target_packages input =
    let open Result.O in
    let install_test_deps_for =
      if build_only then OpamPackage.Name.Set.empty else target_packages
    in
    let constraints =
      constraints ~required_packages:vendored_packages ~ocaml_version
    in
    let+ fixed_packages = fixed_packages ~local_packages ~pin_depends in
    Context.create ~install_test_deps_for ~allow_jbuilder ~require_cross_compile
      ~preferred_versions ?opam_provided ?require_dune ~constraints
      ~fixed_packages input

  type raw_calculation = { package : OpamPackage.t; vendored : bool }

  let pp_raw_calculation fmt { package; vendored = _ } =
    Opam.Pp.package fmt package

  let calculate_raw_with_opam_provided ~local_packages ~opam_provided ~request
      context vendored_packages =
    match Solver.solve context request with
    | Error e ->
        Logs.err (fun l ->
            l "Solving opam-provided dependencies could not find a solution");
        Error (`Diagnostics e)
    | Ok selections ->
        let vendored_package_names =
          vendored_packages |> OpamPackage.Set.elements
          |> List.map ~f:OpamPackage.name
          |> OpamPackage.Name.Set.of_list
        in
        selections |> Solver.packages_of_result
        |> List.filter_map ~f:(fun package ->
               let name = OpamPackage.name package in
               let in_local_packages =
                 OpamPackage.Name.Map.mem name local_packages
               in
               let in_vendored_packages =
                 OpamPackage.Name.Set.mem name vendored_package_names
               in
               let in_opam_provided =
                 OpamPackage.Name.Set.mem name opam_provided
               in
               Logs.debug (fun l ->
                   l "Package %a: local %b, vendored %b, opam-provided %b"
                     Opam.Pp.package_name name in_local_packages
                     in_vendored_packages in_opam_provided);
               match in_local_packages with
               | true -> None
               | false ->
                   let vendored =
                     (not in_opam_provided) && in_vendored_packages
                   in
                   Some { package; vendored })
        |> Result.ok

  let calculate_raw_without_opam_provided ~local_packages packages =
    packages |> OpamPackage.Set.elements
    |> List.filter_map ~f:(fun package ->
           let name = OpamPackage.name package in
           let in_local_packages =
             OpamPackage.Name.Map.mem name local_packages
           in
           match in_local_packages with
           | true -> None
           | false -> Some { package; vendored = true })
    |> Result.ok

  let calculate_raw ~local_packages ~target_packages context =
    let allow_compiler_variants = depend_on_compiler_variants local_packages in
    let request = mk_request ~allow_compiler_variants target_packages in
    match Solver.solve context request with
    | Error e -> Error (`Diagnostics e)
    | Ok selections -> Ok (request, selections)

  type diagnostics = Solver.diagnostics

  let diagnostics_message ~verbose diagnostics =
    `Msg (Solver.diagnostics ~verbose diagnostics)

  module Pkg_map = Solver.Solver.Output.RoleMap

  let no_version_builds_with_dune component =
    match Solver.Diagnostics.Component.selected_impl component with
    | Some _ -> false
    | None -> (
        let rejects, _reason = Solver.Diagnostics.Component.rejects component in
        match rejects with
        | [] -> false
        | _ ->
            List.for_all
              ~f:(fun (_, reason) ->
                match reason with
                | `Model_rejection Context.Non_dune -> true
                | _ -> false)
              rejects)

  let not_buildable_with_dune diagnostics =
    let rolemap = Solver.diagnostics_rolemap diagnostics in
    Pkg_map.fold
      (fun pkg component acc ->
        match no_version_builds_with_dune component with
        | false -> acc
        | true -> pkg :: acc)
      rolemap []
    |> List.filter_map ~f:Solver.package_name

  let determine_version_restriction component =
    let open Option.O in
    let restrictions =
      component |> Solver.Diagnostics.Component.notes
      |> List.map ~f:(function
           | Solver.Diagnostics.Note.UserRequested restriction ->
               [ restriction ]
           | Restricts (_other_role, _impl, restrictions) -> restrictions
           | _ -> [])
      |> List.flatten
    in
    let* version_restrictions =
      match restrictions with
      | [] -> None
      | restrictions ->
          restrictions
          |> List.map ~f:(fun restriction ->
                 let _, version_restriction = Solver.formula restriction in
                 version_restriction)
          |> Option.some
    in
    match version_restrictions with
    | [] -> None
    | init :: rest ->
        List.fold_left
          ~f:(fun formula restriction -> OpamFormula.And (formula, restriction))
          ~init rest
        |> Option.some

  (* [true] if the rejected package would've satisified the version constraint if it
     wouldn't have been a [Model_rejection] *)
  let model_rejection_of_eligible_version version_restriction (model, reason) =
    match reason with
    | `Model_rejection _ -> (
        match Solver.version model with
        | None -> true
        | Some rejected_package ->
            let rejected_version = OpamPackage.version rejected_package in
            let would_be_eligible_otherwise =
              OpamFormula.check_version_formula version_restriction
                rejected_version
            in
            would_be_eligible_otherwise)
    | _ -> false

  let unavailable_versions_due_to_constraints diagnostics =
    let rolemap = Solver.diagnostics_rolemap diagnostics in
    Pkg_map.fold
      (fun pkg component unavailable ->
        match Solver.Diagnostics.Component.selected_impl component with
        | Some _ -> unavailable
        | None -> (
            (* short-circuit skip of fold *)
            let ( let* ) a f =
              match a with Some a -> f a | None -> unavailable
            in
            let* pkg_name = Solver.package_name pkg in
            let* version_restriction =
              determine_version_restriction component
            in
            let rejects, _reason =
              Solver.Diagnostics.Component.rejects component
            in
            (* check if any packages would have had matching versions if they weren't model-rejected *)
            let model_rejections_would_match_version =
              List.exists
                ~f:(model_rejection_of_eligible_version version_restriction)
                rejects
            in
            (* if it is unavailable, construct info on why *)
            match model_rejections_would_match_version with
            | false -> unavailable
            | true -> (pkg_name, version_restriction) :: unavailable))
      rolemap []

  let get_opam_info ~context { package; vendored } =
    match Context.opam_file context package with
    | Ok opam_file ->
        let pinned = Context.is_pinned context package.name in
        let package_summary =
          Opam.Package_summary.from_opam package opam_file ~pinned
        in
        Opam.Dependency_entry.{ package_summary; vendored }
    | Error (`Msg msg) ->
        (* If we're calling this function on a package, it means it has been
           returned as part of the solver solution and therefore should correspond
           to a valid candidate. *)
        Logs.debug (fun l ->
            l "Could not retrieve opam file for %a: %s" Opam.Pp.package package
              msg);
        assert false

  let calculate ~build_only ~allow_jbuilder ~require_cross_compile
      ~preferred_versions ~local_opam_files:local_packages ~target_packages
      ~opam_provided ~pin_depends ?ocaml_version input =
    let open Result.O in
    let* context =
      build_context ~build_only ~allow_jbuilder ~require_cross_compile
        ~preferred_versions ~ocaml_version ~pin_depends ~local_packages
        ~target_packages ~opam_provided input
    in
    let* request, selections =
      calculate_raw ~local_packages ~target_packages context
    in
    let vendored_packages =
      selections |> Solver.packages_of_result |> OpamPackage.Set.of_list
    in
    Logs.info (fun l ->
        l "Selected packages from dune-only run of solver: %a"
          (Opam.Pp.Package_set.pp ~sep:Fmt.comma)
          vendored_packages);
    let* deps, context =
      match OpamPackage.Name.Set.is_empty opam_provided with
      | true ->
          (* shortcut if we know everything will be vendored, no need to build
             a second context and run the solver again *)
          let* deps =
            calculate_raw_without_opam_provided ~local_packages
              vendored_packages
          in
          Ok (deps, context)
      | false ->
          let* opam_provided_context =
            build_context ~build_only ~allow_jbuilder
              ~require_cross_compile:false ~preferred_versions ~ocaml_version
              ~pin_depends ~vendored_packages ~local_packages ~target_packages
              ~require_dune:false input
          in
          let* deps =
            calculate_raw_with_opam_provided ~local_packages ~opam_provided
              ~request opam_provided_context vendored_packages
          in
          Ok (deps, opam_provided_context)
    in
    Logs.app (fun l ->
        l "%aFound %a opam dependencies for the target package%a."
          Pp.Styled.header ()
          Fmt.(styled `Green int)
          (List.length deps) Pp.plural_int
          (OpamPackage.Name.Set.cardinal target_packages));
    Logs.info (fun l ->
        l "The dependencies are: %a"
          Fmt.(list ~sep:(any ",@ ") pp_raw_calculation)
          deps);
    Logs.app (fun l ->
        l "%aQuerying opam database for their metadata and Dune compatibility."
          Pp.Styled.header ());

    Ok (List.map ~f:(get_opam_info ~context) deps)
end

type explicit_repos = string list
type opam_env = OpamVariable.variable_contents String.Map.t
type switch = OpamStateTypes.unlocked OpamStateTypes.switch_state

module Multi_dir_context :
  BASE_CONTEXT with type input = opam_env * explicit_repos = struct
  include Opam_0install.Dir_context

  type input = opam_env * explicit_repos
  (** A list of repo URLs *)

  type nonrec t = t list

  let is_pinned _ _ = false

  (** Create a Dir_context with multiple repos. The list is ordered by priority.
      First repo in the list as higher priority. If two repos provide the same version
      of a package, the one from the highest priority repo will be used, the other
      discared by [candidates]. *)
  let create ?test ~constraints (env, paths) =
    let env varname = String.Map.find_opt varname env in
    match paths with
    | [] ->
        invalid_arg
          "Multi_dir_context should be instanciated with at least one repo"
    | paths -> List.map ~f:(create ?test ~constraints ~env) paths

  let merge_candidates ~name acc candidates =
    List.fold_left ~init:acc candidates ~f:(fun acc (version, opam_file_res) ->
        try OpamPackage.Version.Map.safe_add version opam_file_res acc
        with Failure _ ->
          Logs.info (fun l ->
              l
                "Several of the configured repos define %a.%a. Note that for \
                 now opam-monorepo does not support defining priorities \
                 between repos and picked one arbitrarily."
                Opam.Pp.package_name name Opam.Pp.version version);
          acc)

  (** Candidates must be returned in decreasing preference order *)
  let candidates t name =
    let map =
      List.fold_left t ~init:OpamPackage.Version.Map.empty
        ~f:(fun acc dir_context ->
          let candidates = candidates dir_context name in
          merge_candidates ~name acc candidates)
    in
    (* The bindings of the version map will be sorted by increasing version.
       We reverse it so it's in decreasing version order for the solver to pick
       the highest satisfying version. *)
    List.rev (OpamPackage.Version.Map.bindings map)

  let user_restrictions t pkg = user_restrictions (List.hd t) pkg
  let filter_deps t pkg f = filter_deps (List.hd t) pkg f
end

module Local_opam_context : BASE_CONTEXT with type input = switch = struct
  include Opam_0install.Switch_context

  type t = {
    context : Opam_0install.Switch_context.t;
    state : OpamStateTypes.unlocked OpamStateTypes.switch_state;
  }

  type input = OpamStateTypes.unlocked OpamStateTypes.switch_state

  let is_pinned t pkg = OpamSwitchState.is_pinned t.state pkg
  let candidates t = candidates t.context
  let user_restrictions t = user_restrictions t.context
  let filter_deps t = filter_deps t.context

  let create ?test ~constraints state =
    let context = create ?test ~constraints state in
    { context; state }
end

module Mock_context :
  BASE_CONTEXT
    with type input = opam_env * OpamFile.OPAM.t list * OpamPackage.t list =
struct
  type rejection = UserConstraint of OpamFormula.atom

  let pp_rejection f = function
    | UserConstraint x ->
        Fmt.pf f "Rejected by user-specified constraint %s"
          (OpamFormula.string_of_atom x)

  type t = {
    env : string -> OpamVariable.variable_contents option;
    pkgs : OpamFile.OPAM.t list;
    pins : OpamPackage.Set.t;
    constraints : OpamFormula.version_constraint OpamTypes.name_map;
    test : OpamPackage.Name.Set.t;
  }

  let user_restrictions t name =
    OpamPackage.Name.Map.find_opt name t.constraints

  let env t pkg v =
    if List.mem v ~set:OpamPackageVar.predefined_depends_variables then None
    else
      match OpamVariable.Full.to_string v with
      | "version" ->
          Some
            (OpamTypes.S
               (OpamPackage.Version.to_string (OpamPackage.version pkg)))
      | x -> t.env x

  let filter_deps t pkg f =
    let test = OpamPackage.Name.Set.mem (OpamPackage.name pkg) t.test in
    f
    |> OpamFilter.partial_filter_formula (env t pkg)
    |> OpamFilter.filter_deps ~build:true ~post:true ~test ~doc:false ~dev:false
         ~default:false

  let compare_version x y =
    OpamPackage.Version.compare (OpamFile.OPAM.version y)
      (OpamFile.OPAM.version x)

  let candidates t name =
    let user_constraints = user_restrictions t name in
    match
      List.find_all ~f:(fun pkg -> OpamFile.OPAM.name pkg = name) t.pkgs
    with
    | [] ->
        OpamConsole.log "opam-0install" "Package %S not found!"
          (OpamPackage.Name.to_string name);
        []
    | versions ->
        List.sort ~cmp:compare_version versions
        |> List.map ~f:(fun pkg ->
               let v = OpamFile.OPAM.version pkg in
               match user_constraints with
               | Some test
                 when not
                        (OpamFormula.check_version_formula
                           (OpamFormula.Atom test) v) ->
                   (v, Error (UserConstraint (name, Some test)))
               | _ -> (v, Ok pkg))

  type input = opam_env * OpamFile.OPAM.t list * OpamPackage.t list

  let is_pinned t name = OpamPackage.has_name t.pins name

  let create ?(test = OpamPackage.Name.Set.empty) ~constraints (env, pkgs, pins)
      =
    let env varname = String.Map.find_opt varname env in
    let pins = OpamPackage.Set.of_list pins in
    let pkg_of_opam pkg =
      let name = OpamFile.OPAM.name pkg in
      let version = OpamFile.OPAM.version pkg in
      OpamPackage.create name version
    in
    (* remove pinned packages from the universe -- as that's what's the
       opam solver is doing. *)
    let pkgs =
      List.filter pkgs ~f:(fun opam ->
          let pkg = pkg_of_opam opam in
          let keep =
            match
              OpamPackage.package_of_name_opt pins (OpamPackage.name pkg)
            with
            | None -> true
            | Some pin -> OpamPackage.version pkg = OpamPackage.version pin
          in
          Logs.debug (fun l -> l "keep %a = %b" Opam.Pp.package pkg keep);
          keep)
    in
    { pkgs; constraints; test; env; pins }
end

(* The code below aims to provide a unified interface over the two solver
   modules *)

type ('a, 'b) t =
  (module OPAM_MONOREPO_SOLVER with type input = 'a and type diagnostics = 'b)

module Local_opam_config_context = Opam_monorepo_context (Local_opam_context)
module Explicit_repos_context = Opam_monorepo_context (Multi_dir_context)
module Local_opam_config_solver = Make_solver (Local_opam_config_context)
module Explicit_repos_solver = Make_solver (Explicit_repos_context)
module Mock_solver = Make_solver (Opam_monorepo_context (Mock_context))

type switch_diagnostics = Local_opam_config_solver.diagnostics
type explicit_repos_diagnostics = Explicit_repos_solver.diagnostics
type mock_diagnostics = Mock_solver.diagnostics

let local_opam_config_solver : (switch, switch_diagnostics) t =
  (module Local_opam_config_solver)

let explicit_repos_solver :
    (opam_env * explicit_repos, explicit_repos_diagnostics) t =
  (module Explicit_repos_solver)

let mock_solver : _ t = (module Mock_solver)

let calculate :
    type context diagnostics.
    build_only:bool ->
    allow_jbuilder:bool ->
    require_cross_compile:bool ->
    preferred_versions:OpamTypes.version OpamPackage.Name.Map.t ->
    local_opam_files:
      (OpamTypes.version * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
    target_packages:OpamPackage.Name.Set.t ->
    opam_provided:OpamPackage.Name.Set.t ->
    pin_depends:(OpamTypes.version * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
    ?ocaml_version:string ->
    (context, diagnostics) t ->
    context ->
    ( Opam.Dependency_entry.t list,
      [> `Diagnostics of diagnostics | `Msg of string ] )
    result =
 fun ~build_only ~allow_jbuilder ~require_cross_compile ~preferred_versions
     ~local_opam_files ~target_packages ~opam_provided ~pin_depends
     ?ocaml_version t input ->
  let (module Solver : OPAM_MONOREPO_SOLVER
        with type diagnostics = diagnostics
         and type input = context) =
    t
  in
  Solver.calculate ~build_only ~allow_jbuilder ~require_cross_compile
    ~preferred_versions ~local_opam_files ~target_packages ~opam_provided
    ~pin_depends ?ocaml_version input

let diagnostics_message :
    type context diagnostics.
    verbose:bool ->
    (context, diagnostics) t ->
    diagnostics ->
    [> `Msg of string ] =
 fun ~verbose t diagnostics ->
  let (module Solver : OPAM_MONOREPO_SOLVER
        with type diagnostics = diagnostics
         and type input = context) =
    t
  in
  Solver.diagnostics_message ~verbose diagnostics

let not_buildable_with_dune :
    type context diagnostics.
    (context, diagnostics) t -> diagnostics -> OpamPackage.Name.t list =
 fun t diagnostics ->
  let (module Solver : OPAM_MONOREPO_SOLVER
        with type diagnostics = diagnostics
         and type input = context) =
    t
  in
  Solver.not_buildable_with_dune diagnostics

let unavailable_versions_due_to_constraints :
    type context diagnostics.
    (context, diagnostics) t ->
    diagnostics ->
    (OpamPackage.Name.t * OpamFormula.version_formula) list =
 fun t diagnostics ->
  let (module Solver : OPAM_MONOREPO_SOLVER
        with type diagnostics = diagnostics
         and type input = context) =
    t
  in
  Solver.unavailable_versions_due_to_constraints diagnostics
