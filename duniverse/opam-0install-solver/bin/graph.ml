type config = {
  prune : OpamPackage.Name.Set.t;
  hide_versions : bool;
}

module Input = Solver.Input
module Output = Solver.Solver.Output
module Role = Solver.Input.Role
module Role_map = Output.RoleMap

let classify_role x =
  match Solver.package_name x with
  | Some pkg -> `Opam pkg
  | None -> `Virtual x

let dependencies config ~pkgs sels =
  let depends = Hashtbl.create 100 in
  let versions = ref OpamPackage.Name.Map.empty in
  let sels = Output.to_map sels in
  sels |> Role_map.iter (fun role sel ->
      let pkg = classify_role role in
      let impl = Output.unwrap sel in
      Solver.version impl |> Option.iter (fun pkg ->
          versions := OpamPackage.Name.Map.add (OpamPackage.name pkg) pkg !versions
        );
      let deps, _ = Solver.Input.requires role impl in
      deps |> List.iter (fun dep ->
          let dep = Input.dep_info dep in
          let dep_role = dep.dep_role in
          (* Only show a dependency if we selected something (for optional dependencies).
             Don't show anything for restrictions, even if we did select it. *)
          if Role_map.mem dep_role sels && dep.dep_importance <> `Restricts then (
            Hashtbl.add depends pkg (classify_role dep_role);
          )
        )
    );
  let rec expand role =
    Hashtbl.find_all depends role
    |> List.concat_map (function
        | `Opam dep -> [dep]
        | `Virtual _ as role -> expand role
      )
  in
  let rec visit_pkg acc pkg =
    if OpamPackage.Name.Map.mem pkg acc then acc
    else (
      let deps =
        expand (`Opam pkg)
        |> List.filter (fun pkg -> not (OpamPackage.Name.Set.mem pkg config.prune))
      in
      let acc = OpamPackage.Name.Map.add pkg deps acc in
      List.fold_left visit_pkg acc deps
    )
  in
  List.fold_left visit_pkg OpamPackage.Name.Map.empty pkgs, !versions

let output config ~pkgs f sels =
  let depends, versions = dependencies config ~pkgs sels in
  Fmt.pf f {|@[<v2>digraph opam {@,|};
  Fmt.pf f {|rankdir="LR";@,|};
  Fmt.pf f {|node [shape="box",fontcolor="#ffffff",color="#ef7a08",fillcolor="#ef7a08",style="filled"];@,|};
  Fmt.pf f {|edge [color="#888888"];@,|};
  let label pkg =
    if config.hide_versions then OpamPackage.Name.to_string pkg
    else (
      OpamPackage.to_string (OpamPackage.Name.Map.find pkg versions)
    )
  in
  depends |> OpamPackage.Name.Map.iter (fun pkg deps ->
      let l = label pkg in
      if deps = [] then (
        Fmt.pf f "@,%S;" l
      ) else (
        deps |> List.iter (fun dep ->
            Fmt.pf f "@,%S -> %S;" l (label dep);
          )
      )
    );
  Fmt.pf f "@]@,}"

open Cmdliner

let dot =
  let doc = "Output as a dot dependency graph." in
  Arg.(value @@ flag @@ info ["dot"] ~doc)

let prune =
  let doc = "Prune graph at the given packages." in
  Arg.(value @@ Arg.(opt_all (list string) []) @@ info ["dot-prune"] ~doc)

let hide_versions =
  let doc = "Only show package names, not versions." in
  Arg.(value @@ flag @@ info ["dot-hide-versions"] ~doc)

let cmdliner =
  let make dot prune hide_versions =
    let dot = dot || prune <> [] || hide_versions in
    let prune = prune |> List.concat |> List.map OpamPackage.Name.of_string |> OpamPackage.Name.Set.of_list in
    if dot then Some { prune; hide_versions }
    else None
  in
  Term.(const make $ dot $ prune $ hide_versions)
