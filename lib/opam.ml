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

let pull_tree ~url ~hashes ~dir global_state =
  let dir_str = Fpath.to_string dir in
  let cache_dir =
    OpamRepositoryPath.download_cache global_state.OpamStateTypes.root
  in
  let label = dir_str in
  (* Opam requires a label for the pull, it's only used for logging *)
  let opam_dir = OpamFilename.Dir.of_string dir_str in
  let open OpamProcess.Job.Op in
  OpamRepository.pull_tree ~cache_dir label opam_dir hashes [ url ] @@| function
  | Result _ | Up_to_date _ -> Ok ()
  | Not_available (_, long_msg) ->
      Error (`Msg (Printf.sprintf "Failed to pull %s: %s" label long_msg))

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
        match String.lsplit2 ~on:'#' str_url with
        | Some (repo, ref) -> Git { repo; ref = Some ref }
        | None -> Git { repo = str_url; ref = None })
    | _ -> Other (OpamUrl.to_string url)

  let from_opam_field url =
    let url = OpamFile.URL.url url in
    from_opam url
end

module Hash = struct
  let equal t t' = OpamHash.Set.(equal (singleton t) (singleton t'))

  let pp fmt t =
    let contents = OpamHash.contents t in
    let kind = OpamHash.(string_of_kind (kind t)) in
    Format.fprintf fmt "%s:%s" kind contents
end

module Depexts = struct
  let equal t t' =
    List.equal
      (fun (pkg_set, filter) (pkg_set', filter') ->
        OpamSysPkg.Set.equal pkg_set pkg_set' && filter = filter')
      t t'

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

module Package_summary = struct
  type t = {
    name : string;
    version : string;
    url_src : Url.t option;
    hashes : OpamHash.t list;
    dev_repo : string option;
    depexts : (OpamSysPkg.Set.t * OpamTypes.filter) list;
  }

  let equal { name; version; url_src; hashes; dev_repo; depexts } t' =
    String.equal name t'.name
    && String.equal version t'.version
    && Option.equal Url.equal url_src t'.url_src
    && List.equal Hash.equal hashes t'.hashes
    && Option.equal String.equal dev_repo t'.dev_repo
    && Depexts.equal depexts t'.depexts

  let pp fmt { name; version; url_src; hashes; dev_repo; depexts } =
    let open Pp_combinators.Ocaml in
    Format.fprintf fmt
      "@[<hov 2>{ name = %a;@ version = %a;@ url_src = %a;@ hashes = %a;@ \
       dev_repo = %a;@ depexts = %a }@]"
      string name string version
      (option ~brackets:true Url.pp)
      url_src (list Hash.pp) hashes
      (option ~brackets:true string)
      dev_repo Depexts.pp depexts

  let from_opam ~pkg opam_file =
    let name = OpamPackage.name_to_string pkg in
    let version = OpamPackage.version_to_string pkg in
    let url_field = OpamFile.OPAM.url opam_file in
    let url_src = Option.map ~f:Url.from_opam_field url_field in
    let hashes =
      Option.map_default ~default:[] ~f:OpamFile.URL.checksum url_field
    in
    let dev_repo =
      Option.map ~f:OpamUrl.to_string (OpamFile.OPAM.dev_repo opam_file)
    in
    let depexts = OpamFile.OPAM.depexts opam_file in
    { name; version; url_src; hashes; dev_repo; depexts }

  let is_virtual = function
    | { url_src = None; _ } -> true
    | { dev_repo = None | Some ""; _ } -> true
    | _ -> false

  let is_base_package = function
    | { name; _ } when List.mem name ~set:Config.base_packages -> true
    | _ -> false
end

module Pp = struct
  let package fmt p = Format.fprintf fmt "%s" (OpamPackage.to_string p)
  let hash = Hash.pp
  let url fmt url = Format.fprintf fmt "%s" (OpamUrl.to_string url)
end

let local_package_version opam_file ~explicit_version =
  match explicit_version with
  | Some v -> v
  | None ->
      let default = OpamPackage.Version.of_string "zdev" in
      Option.value (OpamFile.OPAM.version_opt opam_file) ~default
