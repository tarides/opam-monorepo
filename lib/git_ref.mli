type t [@@deriving eq, ord, sexp, show]

val master : t

val from_archive_url : string -> t option
(** Attempt to build a git ref from the given src.url opam field *)

val make : string -> t

val to_string : t -> string
