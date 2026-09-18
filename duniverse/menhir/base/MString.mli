(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module provides various string-related utilities. *)

(* -------------------------------------------------------------------------- *)

(* Buffering. *)

(**[with_buffer n f] creates a fresh buffer of size [n], passes it to
   the function [f], then returns the final content of the buffer. *)
val with_buffer: int -> (Buffer.t -> unit) -> string

(* -------------------------------------------------------------------------- *)

(* Printing. *)

(**An iterator. *)
type 'a iter =
  ('a -> unit) -> unit

(**[this_then_that x y] produces a function [f] whose first invocation returns
   [x] and whose following invocations return [y]. This can be useful to deal
   with separators, delimiters, etc. *)
val this_then_that: 'a -> 'a -> (unit -> 'a)

(**[separated_list print sep xs] converts the list [xs] into a string
   by applying [print] to each element and by inserting [sep] as a
   separator between two successive elements. *)
val separated_list: ('a -> string) -> string -> 'a list -> string

(**[separated_iter print sep xs] converts the iterator [xs] into a
   string by applying [print] to each element and by inserting [sep]
   as a separator between two successive elements. *)
val separated_iter: ('a -> string) -> string -> 'a iter -> string

(**[preceded_list print del xs] converts the list [xs] into a string
   by applying [print] to each element and by preceding each element
   with the delimiter [del]. *)
val preceded_list:  ('a -> string) -> string -> 'a list -> string

(**[preceded_iter print del xs] converts the iterator [xs] into a
   string by applying [print] to each element and by preceding each
   element with the delimiter [del]. *)
val preceded_iter:  ('a -> string) -> string -> 'a iter -> string

(**[subarray_bullet print sep start dot bullet xs] converts a segment
   of the array [xs], beginning at offset [start], into a string, by
   applying [print] to each element and by inserting [sep] as a
   separator between two successive elements. Furthermore, if the
   offset [dot] is part of the closed interval [\[start, n\]] then, at
   this offset, the string [bullet] is inserted, as if it were an
   element. *)
val subarray_bullet:
  ('a -> string) ->
  string ->
  int ->
  int -> string ->
  'a array ->
  string

(**[count n] is an English rendition of the number [n]. For sufficiently small
   numbers, a word is used; for higher numbers, a numeric form is used. *)
val count: int -> string

(**[nth n] is an English rendition of the word "[n]-th". For sufficiently
   small numbers, a word is used; for higher numbers, a numeric form is
   used. *)
val nth: int -> string

(**[padded_index n i] produces a padded string representation of the index
   [i], which must lie in the semi-open interval [\[0, n)]. Padding is used in
   such a way that all indices are mapped to strings of equal length. This
   ensures that alphabetical ordering coincides with numeric ordering. *)
val padded_index: int -> int -> string

(* -------------------------------------------------------------------------- *)

(* Analysis. *)

(**[longest_valid_prefix valid s] returns the longest prefix of the
   string [s] that is composed of valid characters according to the
   user-supplied function [valid]. *)
val longest_valid_prefix: (char -> bool) -> string -> string

(**[longest_run c s] computes the length of the longest run of consecutive
   characters equal to [c] in the string [s]. *)
val longest_run: char -> string -> int

(* -------------------------------------------------------------------------- *)

(* Transformations. *)

(**[normalize s] returns a copy of [s] where parentheses and commas
   are replaced with underscores. *)
val normalize: string -> string

(**[unquote s] assumes that the string [s] begins and ends with a quote
   character ['"']. It removes these quotes and returns the remainder
   of the string, in which escape sequences have been decoded. *)
val unquote: string -> string
