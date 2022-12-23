val has_git_extension : Uri.t -> bool
(** Returns [true] if the given URI's path component has the .git extension *)

val canonicalize : Uri.t -> Uri.t
(** Returns a canonical representation of the URI *)
