open Import

(** Tie the knot between [Dune_engine] and [Dune_rules]. *)
val init :
     stats:Dune_stats.t option
  -> sandboxing_preference:Sandbox_mode.t list
  -> cache_config:Dune_cache.Config.t
  -> cache_debug_flags:Dune_engine.Cache_debug_flags.t
  -> unit

type build_system =
  { conf : Dune_load.conf
  ; contexts : Context.t list
  ; scontexts : Super_context.t Context_name.Map.t
  }

val get : unit -> build_system Memo.t

val find_context_exn : build_system -> name:Context_name.t -> Context.t

val find_scontext_exn : build_system -> name:Context_name.t -> Super_context.t
