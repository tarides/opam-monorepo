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

module Make (G : GRAMMAR) : sig
  open G

  exception EndOfStream

  (**[stream] turns a finite list of terminal symbols into a stream of
     terminal symbols, represented as a pair of a lexer and a lexing buffer,
     so as to be usable with Menhir's traditional API. This lexer can raise
     [EndOfStream]. *)
  val stream: Terminal.t list -> (lexbuf -> Terminal.t) * lexbuf

end
