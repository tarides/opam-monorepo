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

(**A range denotes a range (an interval) in some source file.
   It is essentially a pair of positions. *)
type range

(**[dummy] is a dummy range. It is a pair of two dummy positions. *)
val dummy : range

(**[is_dummy range] determines whether [range] is a dummy range.
   A range is considered a dummy range as soon as its start
   position or end position is a dummy position. *)
val is_dummy : range -> bool

(**[make] converts a pair of positions to a range. *)
val make : position * position -> range

(**[current lexbuf] is the range of the current token in the lexing
   buffer [lexbuf]. *)
val current : lexbuf -> range

(**[startp range] is the start position of the range [range]. *)
val startp : range -> position

(**[endp range] is the end position of the range [range]. *)
val endp : range -> position

(**[filename range] is the file name of the range [range]. *)
val filename : range -> string

(**[linenum range] is the line number of the start of the range [range]. *)
val linenum : range -> int

(**[linecount range] is the number of lines in the range [range]. *)
val linecount : range -> int

(**[column range] is the column number of the start of the range [range]. *)
val column : range -> int

(**[decrement range] moves the start position of [range] by one towards
   the left (assuming that this range does not start at the beginning of
   a line). *)
val decrement : range -> range

(**[show range] is a string representation of the range [range]
   in an Emacs-like format. *)
val show : range -> string

(**[ranges] is an abbreviation for [range list]. *)
type ranges =
  range list

(**[bprint b ranges] prints a string representation of the ranges [ranges]
   to the buffer [b]. Each range is converted to a string via [show] and is
   terminated with a colon and a newline. *)
val bprint : Buffer.t -> ranges -> unit

(**[fprint f ranges] prints a string representation of the ranges
   [ranges] to the output channel [f]. Each range is converted to a
   string via [show] and is terminated with a colon and a newline. *)
val fprint : out_channel -> ranges -> unit
