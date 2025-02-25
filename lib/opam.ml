open Import

let depends_on_any packages (formula : OpamTypes.filtered_formula) =
  let packages = List.map ~f:OpamPackage.Name.of_string packages in
  let is_one_of_packages name =
    List.exists packages ~f:(fun p -> OpamPackage.Name.compare p name = 0)
  in
  OpamFormula.fold_left
    (fun acc (name, _) -> acc || is_one_of_packages name)
    false formula

let ocaml_options =
  (* ocaml-options-vanilla is purposefully excluded from that list as it doesn't imply
     a dependency towards ocaml-variants on its own. *)
  [
    "ocaml-option-32bit";
    "ocaml-option-afl";
    "ocaml-option-bytecode-only";
    "ocaml-option-default-unsafe-string";
    "ocaml-option-flambda";
    "ocaml-option-fp";
    "ocaml-option-musl";
    "ocaml-option-nnp";
    "ocaml-option-nnpchecker";
    "ocaml-option-no-flat-float-array";
    "ocaml-option-spacetime";
    "ocaml-option-static";
    "ocaml-options-only-afl";
    "ocaml-options-only-flambda";
    "ocaml-options-only-flambda-fp";
    "ocaml-options-only-fp";
    "ocaml-options-only-nnp";
    "ocaml-options-only-nnpchecker";
    "ocaml-options-only-no-flat-float-array";
  ]

let depends_on_compiler_variants formula =
  depends_on_any ("ocaml-variants" :: ocaml_options) formula

let depends_on_dune ~allow_jbuilder (formula : OpamTypes.filtered_formula) =
  let packages =
    if allow_jbuilder then [ "dune"; "jbuilder" ] else [ "dune" ]
  in
  depends_on_any packages formula

let version_is_at_least locked_version =
  let version_constraint = (`Geq, locked_version) in
  let version_formula = OpamFormula.Atom version_constraint in
  OpamFormula.check_version_formula version_formula

let avoid_version opam =
  let flags = OpamFile.OPAM.flags opam in
  List.mem ~set:flags OpamTypes.Pkgflag_AvoidVersion

let has_cross_compile_tag opam =
  let tags = OpamFile.OPAM.tags opam in
  List.mem ~set:tags "cross-compile"

let pull_tree_with_cache' ~cache_urls ~cache_dir ~url ~hashes ~dir =
  let dir_str = Fpath.to_string dir in
  let label = dir_str in
  (* Opam requires a label for the pull, it's only used for logging *)
  let opam_dir = OpamFilename.Dir.of_string dir_str in
  let open OpamProcess.Job.Op in
  OpamRepository.pull_tree ~cache_urls ~cache_dir label opam_dir hashes [ url ]
  @@| function
  | Result _ | Up_to_date _ -> Ok ()
  | Not_available (_, long_msg) ->
      Error (`Msg (Printf.sprintf "Failed to pull %s: %s" label long_msg))

let pull_tree ~url ~hashes ~dir global_state =
  let cache_dir =
    OpamRepositoryPath.download_cache global_state.OpamStateTypes.root
  in
  let cache_urls = OpamFile.Config.dl_cache global_state.config in
  pull_tree_with_cache' ~cache_urls ~cache_dir ~url ~hashes ~dir

let pull_tree_with_cache ~cache_dir =
  let cache_dir = cache_dir |> Fpath.to_string |> OpamFilename.Dir.of_string in
  pull_tree_with_cache' ~cache_urls:[] ~cache_dir

module Url = struct
  type t = Git of { repo : string; ref : string option } | Other of string

  (* This includes archives, other VCS and rsync opam src URLs *)

  let equal t t' =
    match (t, t') with
    | Git { repo; ref }, Git { repo = repo'; ref = ref' } ->
        String.equal repo repo' && Option.equal String.equal ref ref'
    | Other s, Other s' -> String.equal s s'
    | _ -> false

  let pp fmt t =
    let open Pp_combinators.Ocaml in
    match t with
    | Git { repo; ref } ->
        Format.fprintf fmt "@[<hov 2>Git@ @[<hov 2>{ repo = %a;@ ref = %a }@]@]"
          string repo
          (option ~brackets:true string)
          ref
    | Other s -> Format.fprintf fmt "@[<hov 2>Other@ %a@]" string s

  let from_opam url =
    match url.OpamUrl.backend with
    | `git -> (
        let str_url = OpamUrl.to_string url in
        match Base.String.lsplit2 ~on:'#' str_url with
        | Some (repo, ref) -> Git { repo; ref = Some ref }
        | None -> Git { repo = str_url; ref = None })
    | _ -> Other (OpamUrl.to_string url)

  let from_opam_field url =
    let url = OpamFile.URL.url url in
    from_opam url

  let is_local_filesystem url =
    match url.OpamUrl.backend with
    | `rsync -> true
    | `http | `git | `darcs | `hg -> false
end

module Hash = struct
  let equal t t' = OpamHash.Set.(equal (singleton t) (singleton t'))

  let pp fmt t =
    let contents = OpamHash.contents t in
    let kind = OpamHash.(string_of_kind (kind t)) in
    Format.fprintf fmt "%s:%s" kind contents
end

module Depexts = struct
  let pp fmt t =
    let pp_filter fmt filter =
      Format.fprintf fmt "%s" (OpamFilter.to_string filter)
    in
    let pp_pkg_set fmt pkg_set =
      Format.fprintf fmt "%s" (OpamSysPkg.Set.to_string pkg_set)
    in
    Format.fprintf fmt "%a"
      Pp_combinators.Ocaml.(list (pair pp_pkg_set pp_filter))
      t
end

module Pp = struct
  module Package : Pp_combinators.Opam.Printable with type t = OpamPackage.t =
  struct
    type t = OpamPackage.t

    let pp = Fmt.using OpamPackage.to_string Fmt.string
  end

  module Package_name :
    Pp_combinators.Opam.Printable with type t = OpamPackage.Name.t = struct
    type t = OpamPackage.Name.t

    let pp = Fmt.using OpamPackage.Name.to_string Fmt.string
  end

  module Package_name_set =
    Pp_combinators.Opam.Make_Set (OpamPackage.Name.Set) (Package_name)

  module Package_set = Pp_combinators.Opam.Make_Set (OpamPackage.Set) (Package)

  let package = Package.pp
  let package_name = Package_name.pp
  let version = Fmt.using OpamPackage.Version.to_string Fmt.string

  let raw_package fmt pkg =
    Format.fprintf fmt "@[<hov 2>{ name = %a;@ version = %a }@]" package_name
      pkg.OpamPackage.name version pkg.version

  let hash = Hash.pp
  let url = Fmt.using OpamUrl.to_string Fmt.string
end

module Package_flag = struct
  type t = OpamTypes.package_flag

  let pp pps (v : t) =
    match v with
    | Pkgflag_LightUninstall -> Fmt.pf pps "light-uninstall"
    | Pkgflag_Verbose -> Fmt.pf pps "verbose"
    | Pkgflag_Plugin -> Fmt.pf pps "plugin"
    | Pkgflag_Compiler -> Fmt.pf pps "compiler"
    | Pkgflag_Conf -> Fmt.pf pps "conf"
    | Pkgflag_AvoidVersion -> Fmt.pf pps "avoid-version"
    | Pkgflag_Unknown unknown -> Fmt.pf pps "unknown(%s)" unknown
end

module Package_summary = struct
  type t = {
    package : OpamPackage.t;
    url_src : Url.t option;
    hashes : OpamHash.t list;
    dev_repo : string option;
    depexts : (OpamSysPkg.Set.t * OpamTypes.filter) list;
    pinned : bool;
    flags : Package_flag.t list;
    has_build_commands : bool;
    has_install_commands : bool;
  }

  let pp fmt
      {
        package;
        url_src;
        hashes;
        dev_repo;
        depexts;
        pinned;
        flags;
        has_build_commands;
        has_install_commands;
      } =
    let open Pp_combinators.Ocaml in
    Format.fprintf fmt
      "@[<hov 2>{ name = %a;@ version = %a;@ url_src = %a;@ hashes = %a;@ \
       dev_repo = %a;@ depexts = %a;@ pinned = %B;@ flags = %a;@ \
       has_build_commands = %B;@ has_install_commands = %B}@]"
      Pp.package_name package.name Pp.version package.version
      (option ~brackets:true Url.pp)
      url_src (list Hash.pp) hashes
      (option ~brackets:true string)
      dev_repo Depexts.pp depexts pinned (list Package_flag.pp) flags
      has_build_commands has_install_commands

  let from_opam package ~pinned opam_file =
    let url_field = OpamFile.OPAM.url opam_file in
    let url_src = Base.Option.map ~f:Url.from_opam_field url_field in
    let hashes =
      Base.Option.value_map ~default:[] ~f:OpamFile.URL.checksum url_field
    in
    let dev_repo =
      Base.Option.map ~f:OpamUrl.to_string (OpamFile.OPAM.dev_repo opam_file)
    in
    let depexts = OpamFile.OPAM.depexts opam_file in
    let flags = OpamFile.OPAM.flags opam_file in
    let has_commands = function [] -> false | _ -> true in
    let has_build_commands = opam_file |> OpamFile.OPAM.build |> has_commands in
    let has_install_commands =
      opam_file |> OpamFile.OPAM.install |> has_commands
    in
    {
      package;
      url_src;
      hashes;
      dev_repo;
      depexts;
      pinned;
      flags;
      has_build_commands;
      has_install_commands;
    }

  let has_flag flag { flags; _ } = List.mem flag ~set:flags
  let is_compiler v = has_flag OpamTypes.Pkgflag_Compiler v
  let is_conf v = has_flag OpamTypes.Pkgflag_Conf v

  let is_virtual = function
    | { url_src = None; _ } -> true
    | { dev_repo = None | Some ""; _ } as pkg when is_conf pkg -> true
    | { has_build_commands = false; has_install_commands = false; _ } -> true
    | _ -> false

  let is_compiler_package { package; _ } =
    OpamPackage.Name.Set.mem package.name Config.compiler_package_names

  let is_skippable_package { package; _ } =
    OpamPackage.Name.Set.mem package.name Config.skip_packages

  let is_safe_package v =
    is_compiler v || is_compiler_package v || is_skippable_package v
    || is_virtual v
end

module Dependency_entry = struct
  type t = { package_summary : Package_summary.t; vendored : bool }
end

let local_package_version opam_file ~explicit_version =
  match explicit_version with
  | Some v -> v
  | None ->
      let default = OpamPackage.Version.of_string "zdev" in
      Option.value (OpamFile.OPAM.version_opt opam_file) ~default

module Extra_field = struct
  type 'a t = {
    name : string;
    to_opam_value : 'a -> OpamParserTypes.FullPos.value;
    from_opam_value :
      OpamParserTypes.FullPos.value -> ('a, [ `Msg of string ]) result;
  }

  let make ~name ~to_opam_value ~from_opam_value =
    {
      name = Printf.sprintf "x-opam-monorepo-%s" name;
      to_opam_value;
      from_opam_value;
    }

  let name t = t.name
  let set t a opam = OpamFile.OPAM.add_extension opam t.name (t.to_opam_value a)

  let get t opam =
    let open Result.O in
    match OpamFile.OPAM.extended opam t.name t.from_opam_value with
    | None -> Ok None
    | Some r -> r >>| Option.some
end

module Pos = struct
  module Full_pos = OpamParserTypes.FullPos

  let default = { Full_pos.filename = "None"; start = (0, 0); stop = (0, 0) }
  let from_value { Full_pos.pos; _ } = pos
  let with_default pelem = { Full_pos.pos = default; pelem }

  let errorf ~pos fmt =
    let startl, startc = pos.Full_pos.start in
    let stopl, stopc = pos.stop in
    Format.ksprintf
      (fun msg -> Error (`Msg msg))
      ("Error in opam file %s, [%d:%d]-[%d:%d]: " ^^ fmt)
      pos.filename startl startc stopl stopc

  let value_errorf ~value fmt =
    let pos = from_value value in
    errorf ~pos fmt

  let unexpected_value_error ~expected value =
    value_errorf ~value "Expected %s, got: %s" expected
      (OpamPrinter.FullPos.value value)
end

module Value = struct
  module String = struct
    let from_value value =
      match (value : OpamParserTypes.FullPos.value) with
      | { pelem = String s; _ } -> Ok s
      | _ -> Pos.unexpected_value_error ~expected:"a string" value

    let to_value s = Pos.with_default (OpamParserTypes.FullPos.String s)
  end

  module List = struct
    let from_value elm_from_value value =
      match (value : OpamParserTypes.FullPos.value) with
      | { pelem = List { pelem; _ }; _ } ->
          Result.List.map ~f:elm_from_value pelem
      | _ -> Pos.unexpected_value_error ~expected:"a list" value

    let to_value elm_to_value l =
      let pelem =
        OpamParserTypes.FullPos.List
          (Pos.with_default (List.map ~f:elm_to_value l))
      in
      Pos.with_default pelem
  end
end
