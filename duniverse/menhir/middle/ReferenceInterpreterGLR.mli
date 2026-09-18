(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Lexing
open MiddleAPI

module Make (Lr1 : LR1) : sig
open Lr1.Lr0.G

  (**[interpret nt lexer lexbuf] runs the parser, starting with the start
     non-terminal symbol [nt], with the lexer [lexer] and the lexing buffer
     [lexbuf]. It either succeeds and returns [Some cst], where [cst] is a
     concrete syntax tree, or fails and returns [None]. *)
  val interpret :
    Nonterminal.t ->
    (lexbuf -> Terminal.t) ->
    lexbuf ->
    CST.cst option

end
