(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module defines some internal naming conventions for use by the two
   code generators, [CodeBackend] and [TableBackend]. It also offers a few
   code generation facilities. *)

open IL
open FrontAPI
open MiddleAPI

module Make (G : GRAMMAR) (X : sig
  include NOPREFIX_SETTINGS
  include REPRESENT_SETTINGS (* actually, just [represent_values] *)
  include TOKEN_TYPE_SETTINGS
end) : sig
open G

(* -------------------------------------------------------------------------- *)

(* Prefixes. *)

(**[prefix] is applied to a variable in order to reduce the likelihood
   of a name collision. *)
val prefix: string -> string

(**[dataprefix] is applied to a data constructor in order to reduce the
   likelihood of a name collision. *)
val dataprefix: string -> string

(**[tvprefix] is applied to a type variable in order to reduce the likelihood
   of a name collision. *)
val tvprefix: string -> string

(* -------------------------------------------------------------------------- *)

(* Conventional names. *)

(**[ntvar nt] is the type variable that is associated with the nonterminal
   symbol [nt]. *)
val ntvar: Nonterminal.t -> typ

(**The variable [semv] is used to hold a semantic value. *)
val semv : string

(**The variable [stack] is used to hold a stack. *)
val stack: string

(**The variable [state] is used to hold a state. *)
val state: string

(**The variable [token] is used to hold a token. *)
val token: string

(**The following variables are used to hold start and end positions. *)
val beforeendp: string
val startp:     string
val endp:       string
val startpos:   string array -> int -> string
val endpos:     string array -> int -> string

(* -------------------------------------------------------------------------- *)

(* Types for semantic values. *)

(**[semvtype nt] is the type of the semantic value associated with the
   nonterminal symbol [nt]. *)
val semvtype: Nonterminal.t -> typ

(**[semvtype1 symbol] is the type of the semantic value associated
   with the symbol [symbol]. If [symbol] is a terminal symbol whose
   semantic value has type [unit] then this type is [unit]. *)
val semvtype1: Symbol.t -> typ

(**[semvtypes symbol] is the type of the semantic value associated with
   [symbol]. Because a terminal symbol whose semantic value has type [unit]
   actually carries no semantic value at runtime, this type can be absent.
   Thus, [semvtypes symbol] is a list of length 0 or 1. *)
val semvtypes: Symbol.t -> typ list

(* -------------------------------------------------------------------------- *)

(**[destruct_token_def name codomain bindsemv branch] generates the definition
   of a function that deconstructs tokens. [name] is the name of the function
   that is generated. [codomain] is its return type. [bindsemv] determines
   whether the variable [semv] should be bound. [branch] is applied to each
   (real) terminal symbol [t] and must produce code for the branch [t]. *)
val destruct_token_def: string -> typ -> bool -> (Terminal.t -> expr) -> valdef

end
