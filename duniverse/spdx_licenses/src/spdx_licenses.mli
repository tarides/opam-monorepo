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

(** license-expression *)
type t =
  | Simple of simple_license (** simple-expression *)
  | WITH of simple_license * string (** simple-expression "WITH" license-exception-id *)
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
    https://spdx.github.io/spdx-spec/appendix-IV-SPDX-license-expressions/ *)

val to_string : t -> string
(** [to_string license] returns a normalized string corresponding to [license]
    in a valid SPDX license expression format. *)

val valid_license_ids : string list
(** [valid_license_ids] gives the list of valid license IDs.
    The list does not contain deprecated licenses.
    See Appendix I.1: https://spdx.github.io/spdx-spec/appendix-I-SPDX-license-list/ *)

val valid_exception_ids : string list
(** [valid_exception_ids] gives the list of valid exception IDs.
    The list does not contain deprecated exceptions.
    See Appendix I.2: https://spdx.github.io/spdx-spec/appendix-I-SPDX-license-list/ *)
