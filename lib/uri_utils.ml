open Import

let canonicalize uri =
  let open Result.O in
  let path = Uri.path uri in
  let new_path =
    let* fpath = Fpath.of_string path in
    match Fpath.has_ext ".git" fpath with
    | true -> Ok fpath
    | false -> Ok (Fpath.add_ext ".git" fpath)
  in
  match new_path with
  | Error _ -> uri
  | Ok new_path ->
      let new_path = Fpath.to_string new_path in
      Uri.with_path uri new_path
