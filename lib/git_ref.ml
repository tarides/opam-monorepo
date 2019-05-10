open Sexplib.Conv

type t = string [@@deriving eq, ord, sexp, show]

let master = "master"

let make s = s

let to_string t = t

let from_archive_url archive =
  let uri = Uri.of_string archive in
  let path = Astring.String.cuts ~empty:false ~sep:"/" (Uri.path uri) in
  let parse_err () =
    Logs.err (fun l -> l "Unable to classify archive %s" archive);
    None
  in
  let tag_of_file ?(prefix = "") f =
    match Duniverse_std.File.strip_ext f |> Astring.String.cut ~rev:true ~sep:"-" with
    | None -> parse_err ()
    | Some (_n, v) -> Some (prefix ^ v)
  in
  let tag_of_last_path ?prefix () = List.rev path |> List.hd |> tag_of_file ?prefix in
  match Uri.scheme uri with
  | Some "git+http" | Some "git+https" | Some "git+ssh" | Some "git" -> (
    match Astring.String.cuts ~empty:false ~sep:"#" archive with
    | [ _repo; tag ] -> Some tag
    | _ -> Some "master" )
  | Some "git+file" -> None
  | _ -> (
    match Uri.host uri with
    | Some "github.com" -> (
      match path with
      | [ _u; _r; "releases"; "download"; v; _archive ] -> Some v
      | [ _u; _r; "archive"; archive ] -> Some (Duniverse_std.File.strip_ext archive)
      | [ _u; _r; "archive"; tag; _ ] -> Some tag
      | _ -> if Uri.scheme uri = Some "git+https" then None else parse_err () )
    | Some "ocaml.janestreet.com" -> (
      match path with
      | [ "ocaml-core"; _ver; "files"; f ] -> tag_of_file f
      | [ "janestreet"; _r; "releases"; "download"; v; _f ] -> Some v
      | [ "janestreet"; _r; "archive"; f ] -> Some (Duniverse_std.File.strip_ext f)
      | _ -> parse_err () )
    | Some "gitlab.camlcity.org" | Some "download.camlcity.org" -> tag_of_last_path ()
    | Some "ocamlgraph.lri.fr" | Some "erratique.ch" -> tag_of_last_path ~prefix:"v" ()
    | _ ->
        Logs.info (fun l ->
            l "Attempting to guess tag for %s from the final part of the URL" archive );
        tag_of_last_path () )
