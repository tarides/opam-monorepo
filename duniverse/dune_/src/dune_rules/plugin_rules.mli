open! Stdune

val setup_rules :
  sctx:Super_context.t -> dir:Path.Build.t -> Dune_file.Plugin.t -> unit Memo.t

val install_rules :
     sctx:Super_context.t
  -> sites:Sites.t
  -> dir:Path.Build.t
  -> Dune_file.Plugin.t
  -> Install.Entry.Sourced.t list Memo.t
