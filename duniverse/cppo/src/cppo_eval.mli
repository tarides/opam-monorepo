(** The type signatures in this module are not yet for public consumption.

    Please don't rely on them in any way.*)

module S : Set.S with type elt = string
module M : Map.S with type key = string

type env

val builtin_env : env

val include_inputs
  : extensions:(string, Cppo_command.command_template) Hashtbl.t
  -> preserve_quotations:bool
  -> incdirs:string list
  -> show_exact_locations:bool
  -> show_no_locations:bool
  -> Buffer.t
  -> env
  -> (string * string * (unit -> Lexing.lexbuf) * (unit -> unit)) list -> env
