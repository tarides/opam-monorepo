open Import

let path_ok path = path |> Fpath.to_string |> Result.ok
let flip f a b = f b a

let dump_result =
  let dump_msg ppf (`Msg s) = Fmt.pf ppf "`Msg %S" s in
  Fmt.Dump.result ~ok:Fmt.Dump.string ~error:dump_msg

let canonicalize uri =
  let open Result.O in
  let new_path =
    let* fpath = Fpath.of_string (Uri.path uri) in
    let fpath =
      match Fpath.has_ext ".git" fpath with
      | true -> fpath
      | false -> Fpath.add_ext ".git" fpath
    in
    path_ok fpath
  in
  let new_scheme =
    match Uri.scheme uri with
    | Some "https" -> Ok "git+https"
    | Some "git" -> Ok "git+https"
    | Some ("git+https" as git_https) -> Ok git_https
    | Some other_scheme ->
        Fmt.error_msg "Can't canonicalize unknown scheme %s" other_scheme
    | None -> Fmt.error_msg "No scheme provided in %a" Uri.pp uri
  in
  match (new_path, new_scheme) with
  | Ok new_path, Ok new_scheme ->
      uri
      |> (flip Uri.with_path) new_path
      |> (flip Uri.with_scheme) (Some new_scheme)
      |> (flip Uri.with_fragment) None
  | failed_path, failed_scheme ->
      Logs.warn (fun l ->
          l
            "Canonicalization of URL %a failed, passing unchanged \
             (canonicialized path: %a canonicalized scheme: %a)"
            Uri.pp uri dump_result failed_path dump_result failed_scheme);
      uri
