(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Syntax

module type API_SETTINGS = sig

  (**[incremental] determines whether the incremental API must be generated.
     This flag is [true] if and only if the table back-end has been selected. *)
  val incremental: bool

  (**[inspection] determines whether the inspection API must be generated. This
     API involves GADTs, and requires producing more tables, so this flag is off
     by default. This flag can be [true] only if the table back-end has been
     selected. In other words, [inspection] implies [incremental]. The
     inspection API is an extension of the incremental API. *)
  val inspection : bool

  (**[unparsing] determines whether the unparsing API must be generated. This
     API offers facilities for constructing and displaying concrete syntax
     trees. This flag can be [true] only if the table back-end has been
     selected. In other words, [unparsing] implies [incremental]. *)
  val unparsing : bool

end

module type BASE_SETTINGS = sig

  (**[base] is a base name, that is, a file name without an extension.
     This base name should be used to construct the names of the files
     that we generate. *)
  val base: string

end

module type CHECK_GRAMMAR_SETTINGS = sig

  (**[ignore_unused_token t] returns [true] if an ignored-token warning
     about the token [t] should be suppressed. *)
  val ignore_unused_token : terminal -> bool

  (**[require_aliases] determines whether a token without an alias should
     cause a warning. *)
  val require_aliases : bool

end

module type COMMENT_SETTINGS = sig

  (**[comment] determines whether comments should be included in the
     OCaml code that is generated. *)
  val comment: bool

end

module type EXN_SETTINGS = sig

  (**[exn_carries_state] determines whether the exception [Error] should carry
     a state number. This is the state where a syntax error is detected. This
     feature is supported only by the code back-end. *)
  val exn_carries_state: bool

  (**[exn_carries_top_nodes] determines whether the exception [Error] should
     carry a list of GSS top nodes. This feature is supported only by the GLR
     back-end. *)
  val exn_carries_top_nodes: bool

  (**[fixedexc] determines whether the exception [Error] should be declared
     equal to the exception [Parsing.Parse_error]. Setting this flag to [true]
     can be useful when full compatibility with ocamlyacc is desired. *)
  val fixedexc: bool

end

module type INFER_SETTINGS = sig

  (**[ocamlc] is the command that should be used to invoke the OCaml
     bytecode compiler. Its default value is the string ["ocamlc"]. *)
  val ocamlc: string

  (**[ocamldep] is the command that should be used to invoke the OCaml
     dependency generator. Its default value is the string ["ocamldep"]. *)
  val ocamldep: string

end

module type TOKEN_TYPE_SETTINGS = sig

  (**If [mode] is [`DefineTokenType] then the types [token] and ['a terminal]
     are defined as algebraic data types. If it is [`UseExternalTokenType "M"]
     then they are defined as abbreviations for pre-existing types [M.token]
     and ['a M.Interpreter.terminal]. *)
  val mode: [ `DefineTokenType | `UseExternalTokenType of string]

end
