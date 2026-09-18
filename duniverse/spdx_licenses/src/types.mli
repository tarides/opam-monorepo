(* SPDX-License-Identifier: MIT *)

type user_defined_license = {
  document_ref : string option;
  license_ref : string;
}

type simple_license =
  | LicenseID of string
  | LicenseIDPlus of string
  | LicenseRef of user_defined_license

type user_defined_addition = {
  document_ref : string option;
  addition_ref : string;
}

type addition =
  | Exception of string
  | AdditionRef of user_defined_addition

type t =
  | Simple of simple_license
  | WITH of simple_license * addition
  | AND of t * t
  | OR of t * t
