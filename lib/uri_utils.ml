open Import

module Normalized = struct
  type t = Github of { user : string; repo : string } | Other of Uri.t

  let of_uri uri =
    match Uri.host uri with
    | Some "github.com" -> (
        let path = Uri.path uri in
        match Option.map
                (fun idx ->
                   (String.sub path ~pos:0 ~len:idx,
                    String.sub path ~pos:(succ idx) ~len:(String.length path - idx - 1)))
                (String.index_opt path '/') with
        | None -> Other uri
        | Some (user, gitrepo) -> (
            match Option.map
                    (fun idx ->
                       (String.sub gitrepo ~pos:0 ~len:idx,
                        String.sub gitrepo ~pos:(succ idx) ~len:(String.length gitrepo - idx - 1)))
                    (String.rindex_opt gitrepo '.') with
            | None -> Github { user; repo = gitrepo }
            | Some (repo, "git") -> Github { user; repo }
            | Some _ -> Other uri))
    | Some _ | None -> Other uri

  let equal a b =
    match (a, b) with
    | Other a, Other b -> Uri.equal a b
    | Github { user; repo }, Github { user = user'; repo = repo' } ->
        String.equal user user' && String.equal repo repo'
    | _, _ -> false

  let pp ppf = function
    | Github { user; repo } -> Fmt.pf ppf "<Github user=%s repo=%s>" user repo
    | Other uri -> Fmt.pf ppf "<Other %a>" Uri.pp uri
end
