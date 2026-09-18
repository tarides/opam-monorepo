(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf
open Lexing

type range =
  position * position

let dummy =
  (dummy_pos, dummy_pos)

let is_dummy (startp, endp) =
  startp == dummy_pos || endp == dummy_pos

let[@inline] make x =
  x

let[@inline] startp (startp, _endp) =
  startp

let[@inline] endp (_startp, endp) =
  endp

let[@inline] filename (startp, _endp) =
  startp.pos_fname

let[@inline] linenum (startp, _endp) =
  startp.pos_lnum

let[@inline] linecount (startp, endp) =
  endp.pos_lnum - startp.pos_lnum

let[@inline] column (startp, _endp) =
  startp.pos_cnum - startp.pos_bol

let[@inline] end_column (startp, endp) =
  endp.pos_cnum - startp.pos_bol (* intentionally [startp.pos_bol] *)

let decrement (pos : position) : position =
  let column = pos.pos_cnum in
  let column = max 0 (column - 1) in
  { pos with pos_cnum = column }

let decrement ((startp, endp) : range) : range =
  make (decrement startp, endp)

let show range =
  (* [filename] is hopefully not empty. *)
  sprintf
    "File \"%s\", line %d, characters %d-%d"
    (filename range) (linenum range)
    (column range) (end_column range)

let current lexbuf =
  (lexeme_start_p lexbuf, lexeme_end_p lexbuf)

type ranges =
  range list

let bprint b ranges =
  ranges |> List.iter @@ fun range ->
  bprintf b "%s:\n" (show range)

let fprint f ranges =
  ranges |> List.iter @@ fun range ->
  fprintf f "%s:\n" (show range)
