(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This module defines two APIs for parsers, namely the traditional API
   and the revised API, and offers conversions between these APIs. *)

(**An ocamlyacc-style, or Menhir-style, parser requires access to
   the lexer, which must be parameterized with a lexing buffer, and
   to the lexing buffer itself, where it reads position information.

   This traditional API is convenient when used with ocamllex, but
   inelegant when used with other lexer generators. *)
type ('token, 'semantic_value) traditional =
    (Lexing.lexbuf -> 'token) -> Lexing.lexbuf -> 'semantic_value

(**The revised API is independent of any lexer generator. Here, the
   parser only requires access to the lexer, and the lexer takes no
   parameters. The tokens returned by the lexer may contain position
   information. *)
type ('token, 'semantic_value) revised =
    (unit -> 'token) -> 'semantic_value

(* --------------------------------------------------------------------------- *)

(**[traditional2revised] converts a traditional parser, produced by
   ocamlyacc or Menhir, into a revised parser.

   A token of the revised lexer is essentially a triple of a token
   of the traditional lexer (or raw token), a start position, and
   and end position. The three [get] functions are accessors.

   We do not require the type ['token] to actually be a triple type.
   This enables complex applications where it is a record type with
   more than three fields. It also enables simple applications where
   positions are of no interest, so ['token] is just ['raw_token]
   and [get_startp] and [get_endp] return dummy positions. *)
val traditional2revised:
  ('token -> 'raw_token) ->
  ('token -> Lexing.position) ->
  ('token -> Lexing.position) ->
  ('raw_token, 'semantic_value) traditional ->
  ('token, 'semantic_value) revised

(* --------------------------------------------------------------------------- *)

(**[revised2traditional] converts a revised parser to a traditional parser. *)
val revised2traditional:
  ('raw_token -> Lexing.position -> Lexing.position -> 'token) ->
  ('token, 'semantic_value) revised ->
  ('raw_token, 'semantic_value) traditional

(* --------------------------------------------------------------------------- *)

(**This submodule offers simplified versions of the above conversions.
   In this simplified API, concrete triples are used. *)
module Simplified : sig

  (**[traditional2revised] converts a traditional parser, produced by
     ocamlyacc or Menhir, into a revised parser. *)
  val traditional2revised:
    ('token, 'semantic_value) traditional ->
    ('token * Lexing.position * Lexing.position, 'semantic_value) revised

  (**[revised2traditional] converts a revised parser to a traditional parser. *)
  val revised2traditional:
    ('token * Lexing.position * Lexing.position, 'semantic_value) revised ->
    ('token, 'semantic_value) traditional

end
