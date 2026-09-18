type config
val output : config -> pkgs:OpamPackage.Name.t list -> Solver.Solver.Output.t Fmt.t
val cmdliner : config option Cmdliner.Term.t
