open Import

type t

val from_string : string -> t
val to_string : t -> string
val pp : t Fmt.t

val repo_name : t -> (string, Rresult.R.msg) result
(** Computes a name for the repo by applying the following method:
  1. Start with the path component of the repo's uri
  2. Remove any trailing "/" characters
  3. Take the substring to the right of the right-most "/" character to obtain
     the final part of the path
  4. Remove any leading "." characters
  5. Take the substring to the left of the left-most "." character

    E.g. [repo_name (from_string "https://github.com/ocamllabs/opam-monorepo.git")]
    returns ["opam-monorepo"].

    Returns an error if the result would be the empty string. *)

module Map : Map.S with type key = t
