(* SPDX-License-Identifier: MIT *)

(** ["DocumentRef-" idstring ":"]"LicenseRef-" idstring *)
type user_defined_license = {
  document_ref : string option;
  license_ref : string;
}

(** simple-expression *)
type simple_license =
  | LicenseID of string (** license-id *)
  | LicenseIDPlus of string (** license-id '+' (the '+' isn't contained in the string) *)
  | LicenseRef of user_defined_license (** A SPDX user defined license reference *)

(** ["DocumentRef-" idstring ":"]"AdditionRef-" idstring *)
type user_defined_addition = {
  document_ref : string option;
  addition_ref : string;
}

(** addition-expression *)
type addition =
  | Exception of string (** license-exception-id *)
  | AdditionRef of user_defined_addition (** A SPDX user defined addition reference *)

(** license-expression *)
type t =
  | Simple of simple_license (** simple-expression *)
  | WITH of simple_license * addition (** simple-expression "WITH" addition-expression *)
  | AND of t * t (** compound-expression "AND" compound-expression *)
  | OR of t * t (** compound-expression "OR" compound-expression *)

(** The errors returned by the parser *)
type error = [
  | `InvalidLicenseID of string
  | `InvalidExceptionID of string
  | `ParseError
]

val parse : string -> (t, [> error]) result
(** [parse str] parses [str] according to the syntax described in:
    https://spdx.github.io/spdx-spec/v3.0.1/annexes/spdx-license-expressions/ *)

val to_string : t -> string
(** [to_string license] returns a normalized string corresponding to [license]
    in a valid SPDX license expression format. *)

val valid_license_ids : string list
(** [valid_license_ids] gives the list of valid license IDs.
    The list does not contain deprecated licenses.
    See: https://spdx.org/licenses/ *)

val valid_exception_ids : string list
(** [valid_exception_ids] gives the list of valid exception IDs.
    The list does not contain deprecated exceptions.
    See: https://spdx.org/licenses/exceptions-index.html *)
