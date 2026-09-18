(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**The module [Driver] offers a unified API to the parser,
   which is constructed by ocamlyacc in stage 1
   and by Menhir in stage 2. *)

open Lexing
open Syntax

(**[parse priority lexbuf] reads an [.mly] file from the lexing buffer
   [lexbuf], and returns a partial grammar. The integer parameter [priority]
   is the priority that is assigned to the semantic actions. Priority is used
   to type-check the semantic actions that originate in [standard.mly] first. *)
val parse: int -> lexbuf -> partial_grammar
