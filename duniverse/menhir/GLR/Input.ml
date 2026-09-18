(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

open Lexing
module V = MiniVector

type 'token input = {

  lexer: lexbuf -> 'token;
  (**The lexer. *)

  lexbuf: lexbuf;
  (**The lexing buffer. *)

  mutable lookahead_token: 'token;
  (**The first unconsumed token may or may not have been obtained already
     from the lexer. This field should have type ['token option]. To avoid
     allocating an option, we use the address of the [input] object itself
     as a sentinel value, standing for [None]. (Cute, eh?) (Danger!)
     We save 2 words of memory per token, and about 2% in time, it seems. *)

  startpos: position V.vector;
  (**A vector of the start positions of the tokens that we have consumed. *)

  endpos: position V.vector;
  (**A vector of the end positions of the tokens that we have consumed. *)

  initp: position;
  (**The initial position. This position is obtained from the lexing buffer
     at the very beginning. *)

}

let create lexer lexbuf =
  let lookahead_token = Obj.magic () (* overwritten below *)
  and startpos = V.create()
  and endpos = V.create()
  and initp = lexbuf.lex_curr_p in
  let input = { lexer; lexbuf; lookahead_token; startpos; endpos; initp } in
  input.lookahead_token <- Obj.magic input;
  input

let[@inline] consume input =
  (* It would be surprising if the parser would consume a token without
     having examined it first. (An LR parser will never do this.) So, we
     consider this situation an error. *)
  assert (input.lookahead_token != Obj.magic input);
  (* Push the positions of the current lookahead symbol (which we are about
     to consume) into the position vectors. *)
  V.push input.startpos input.lexbuf.lex_start_p;
  V.push input.endpos input.lexbuf.lex_curr_p;
  (* Record the fact that we have not yet read the next token. *)
  input.lookahead_token <- Obj.magic input

let[@inline] lookahead input =
  let token = input.lookahead_token in
  if token != Obj.magic input then
    token
  else
    let token = input.lexer input.lexbuf in
    input.lookahead_token <- token;
    token

(* -------------------------------------------------------------------------- *)

(* A note on keeping track of positions. *)

(* Every node in the GSS carries a date, that is, an offset into the input,
   measured in number of tokens. A date represents a position *between* two
   tokens. *)

(* It would make sense to give the user access to dates, perhaps via new
   keywords that could be used in the semantic actions. *)

(* For the moment, for historical reasons, the user does not have access to
   dates. Instead the user has access to Menhir's traditional position
   keywords, such as [$startpos], [$endpos], [$startpos(x)], etc. *)

(* There is an unfortunate mismatch between the concept of position and the
   concept of date. Indeed, because all positions are originally produced by
   the lexer, every position must be the start position or the end position of
   some token. Thus, in between two tokens, we have *two* possible positions:
   the end position of the first token, and the start position of the second
   token. This creates a problem in the translation of a date to a position:
   which of the two positions should be used? A somewhat sensible approach,
   which we adopt, is to use the end position of the first token if the date
   is supposed to represent an end date, and to the use the start position of
   the second token if the date is supposed to represent a start date. *)

(* This approach still has a potentially surprising property. The empty date
   interval [\[i, i)] is translated to an interval [\[pos1, pos2)] where the
   position [pos2] possibly lies leftwards of the position [pos1]. To avoid
   this problem, perhaps a good rule of thumb is to avoid using the keywords
   [$startpos] and [$endpos] inside an epsilon production, and to avoid using
   the keywords [$startpos(x)] and [$endpos(x)] when the symbol denoted by [x]
   is nullable. Another idea is to just be aware of this problem and exchange
   the positions [pos1] and [pos2] in this case. *)

(* In this module, the vectors [startpos] and [endpos] record the start
   position and end position of the input symbols that have been consumed.
   These vectors serve as a basis in the translation of dates to
   positions. *)

type date =
  int

let[@inline] end_position input date =
  let endpos = input.endpos in
  assert (0 <= date && date <= V.length endpos);
  if 0 < date then
    V.unsafe_get endpos (date-1)
  else
    (* This case is problematic, but should arise only if we are asked about
       the end position of an epsilon production or of a nullable symbol. *)
    (* In this case, instead of the end position of the token at date [-1],
       which does not exist, we return the initial position of the input
       stream. *)
    input.initp

let[@inline] start_position input date =
  let startpos = input.startpos in
  assert (0 <= date && date <= V.length startpos);
  if date < V.length startpos then
    V.unsafe_get startpos date
  else
    (* This case is problematic, but should arise only if we are asked about
       the start position of an epsilon production or of a nullable symbol. *)
    (* In this case, instead of the start position of the token at [date],
       which does not exist, we return the end position of the previous
       token. *)
    end_position input date
