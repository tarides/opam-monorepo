(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This module provides an abstraction of the input stream that is convenient
   for use in an LR or GLR parser. It manages a one-place buffer which stores
   the lookahead symbol. It also manages two vectors of positions, which are
   used to translate dates into positions. *)

open Lexing

(**A date is an index into the input stream, viewed as a sequence of tokens;
   therefore it represents a place in between two tokens.*)
type date = int

(**An input stream is a (finite or infinite) sequence of tokens. *)
type 'a input

(**[create lexer lexbuf] creates an input stream out of the lexer [lexer] and
   lexing buffer [lexbuf]. *)
val create : (lexbuf -> 'a) -> lexbuf -> 'a input

(**[lookahead input] returns the first token of the input stream, without
   consuming it. *)
val lookahead : 'a input -> 'a

(**[consume input] consumes the first token of the input stream. It is an
   error to call [consume] without first calling [lookahead] at least once. *)
val consume : 'a input -> unit

(**[start_position input date] translates the date [date] to a position, under
   the assumption that this date represents a start date, that is, the left
   end of a date interval. *)
val start_position : 'a input -> date -> position

(**[end_position input date] translates the date [date] to a position, under
   the assumption that this date represents an end date, that is, the right
   end of a date interval. *)
val end_position : 'a input -> date -> position
