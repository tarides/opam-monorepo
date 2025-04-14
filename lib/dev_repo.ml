open Import

type t = string

let compare = String.compare
let from_string s = s
let to_string t = t
let pp = Fmt.string

let repo_name_regexp =
  let open Re in
  compile
    (seq [
        opt (char '/');
        rep (char '.');
        group (rep1 (compl [ char '.'; char '/' ]));
        opt (seq [
            char '.';
            rep (compl [ char '/' ])]);
        rep (char '/');
        stop
      ])

let repo_name t =
  let path = Uri.of_string t |> Uri.path in
  match Re.exec_opt repo_name_regexp path with
  | Some group ->
    Ok (Re.Group.get group 0)
  | None ->
    Rresult.R.error_msgf
      "unexpected empty string while computing name for dev_repo: \"%s\"" t

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Tbl = Hashtbl.Make (struct
  type nonrec t = t

  let hash = Hashtbl.hash
  let equal = String.equal
end)
