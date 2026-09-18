(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This is the ocamlyacc-specific driver. There is nothing special to do. We
   handle syntax errors in a minimalistic manner. This error handling code
   will be exercised only if there is a syntax error in [stage2/parser.mly]
   during stage 2 of the bootstrap process. *)

let parse priority lexbuf =
  Lexer.priority := priority;
  try
    Parser.grammar Lexer.main lexbuf
  with Parsing.Parse_error ->
    Report.Just.error [Range.current lexbuf] "syntax error."
