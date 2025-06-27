(** Parsing and interpretation of opam files *)

open Import

type value := OpamParserTypes.value

(** Type of opam files *)
type t = OpamParserTypes.opamfile

(** Extracts a field *)
val get_field : t -> string -> value option

(** Parse the contents of an opam file *)
val parse : Lexing.lexbuf -> t

(** Parse just a value *)
val parse_value : Lexing.lexbuf -> value

(** Replace all [pos] value by a triplet [(fname, line, absolute_offset)] *)
val absolutify_positions : file_contents:string -> t -> t

val nopos : OpamParserTypes.pos

val existing_variables : t -> String.Set.t

module Create : sig
  val string : string -> value

  val list : ('a -> value) -> 'a list -> value

  val string_list : string list -> value

  val normalise_field_order : (string * value) list -> (string * value) list

  val of_bindings : (string * value) list -> file:Path.t -> t
end
