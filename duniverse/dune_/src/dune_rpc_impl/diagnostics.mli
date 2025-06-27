(** conversion functions for build system -> rpc diagnostics *)

open Import

val diagnostic_event_of_error_event :
  Build_system.Error.Event.t -> Dune_rpc.Diagnostic.Event.t

val diagnostic_of_error : Build_system.Error.t -> Dune_rpc_private.Diagnostic.t
