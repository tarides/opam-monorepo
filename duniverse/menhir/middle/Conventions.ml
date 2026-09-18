(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open IL
open ILConstruction
open FrontAPI
open MiddleAPI

module Make (G : GRAMMAR) (X : sig
  include NOPREFIX_SETTINGS
  include REPRESENT_SETTINGS (* actually, just [represent_values] *)
  include TOKEN_TYPE_SETTINGS
end) = struct
open G

module TokenType =
  TokenType.Make(X)
module ILTokens =
  ILTokens.Make(G)(X)
open ILTokens

(* -------------------------------------------------------------------------- *)

(* Prefixes. *)

let prefix name =
  if X.noprefix then
    name
  else
    (* This prefix must begin with an underscore. This allows avoiding
       warnings about unused variables with OCaml 3.09 and later. *)
    "_menhir_" ^ name

let dataprefix name =
  if X.noprefix then
    name
  else
    (* This prefix must begin with a capital letter. *)
    "Menhir" ^ name

let tvprefix name =
  if X.noprefix then
    name
  else
    (* This prefix must begin with an ordinary letter (not an underscore). *)
    "ttv_" ^ name

(* -------------------------------------------------------------------------- *)

(* Naming conventions. *)

let ntvar nt =
  TypVar (Printf.sprintf "tv_%s" (Nonterminal.print true nt))

let semv =
  "_v"

let stack =
  prefix "stack"

let state =
  prefix "s"

let token =
  "_tok"

(* The following variables are used to hold start and end positions. Do not
   change these names! They are chosen to coincide with the $startpos and
   $endpos keywords, which the lexer rewrites to _startpos and _endpos, so
   that binding these variables before executing a semantic action is
   meaningful. *)

(* These names should agree with the printing function [Keyword.posvar]. *)

let beforeendp =
  Keyword.(posvar Before WhereEnd FlavorPosition)
    (* "_endpos__0_" *)

let startp =
  Keyword.(posvar Left WhereStart FlavorPosition)
    (* "_startpos" *)

let endp =
  Keyword.(posvar Left WhereEnd FlavorPosition)
    (* "_endpos" *)

let startpos ids i =
  Keyword.(posvar (RightNamed ids.(i)) WhereStart FlavorPosition)
    (* sprintf "_startpos_%s_" ids.(i) *)

let endpos ids i =
  Keyword.(posvar (RightNamed ids.(i)) WhereEnd FlavorPosition)
    (* sprintf "_endpos_%s_" ids.(i) *)

(* -------------------------------------------------------------------------- *)

(* Types for semantic values. *)

let semvtype nt =
  match Nonterminal.ocamltype nt with
  | None ->
      (* [nt] has unknown type. If we have performed type inference, this
         cannot happen. However, type inference is still optional, so we
         must tolerate this. *)
      ntvar nt
  | Some ocamltype ->
      TypTextual ocamltype

let semvtypetok tok =
  match Terminal.ocamltype tok with
  | None ->
      (* This terminal symbol has type [unit], so it is omitted in every
         stack cell, unless [X.represent_values] is true. *)
      if X.represent_values then [ tunit ] else []
  | Some ocamltype ->
      [ TypTextual ocamltype ]

let semvtypes = function
  | Symbol.T tok ->
      semvtypetok tok
  | Symbol.N nt ->
      [ semvtype nt ]

let semvtypetok1 tok =
  match Terminal.ocamltype tok with
  | None ->
      tunit
  | Some ocamltype ->
      TypTextual ocamltype

let semvtype1 = function
  | Symbol.T tok ->
      semvtypetok1 tok
  | Symbol.N nt ->
      semvtype nt

(* -------------------------------------------------------------------------- *)

(* [destruct_token_def] constructs the definition of a function that
   deconstructs tokens. *)

let destruct_token_def name codomain bindsemv mkbranch =
  def name @@
  annotate (arrow TokenType.ttoken codomain) @@
  efun [ PVar token ] @@
  ematch (EVar token) @@
  Terminal.map_real @@ fun t ->
  let pat  = tokpat t (if bindsemv then PVar semv else PWildcard)
  and body = mkbranch t in
  branch pat body

(* -------------------------------------------------------------------------- *)

end (* Make *)
