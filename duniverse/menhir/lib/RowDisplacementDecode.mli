(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**Row displacement aims to compress a two-dimensional table where some values
   are considered insignificant. *)

(* This idea reportedly appears in Aho and Ullman's "Principles of Compiler
   Design" (1977). It is evaluated in Tarjan and Yao's "Storing a Sparse
   Table" (1979) and in Dencker, Dürre, and Heuft's "Optimization of Parser
   Tables for Portable Compilers" (1984). *)

(* We place the encoding functions and the decoding functions in two distinct
   modules. This is the decoding module. It is part of the runtime support
   library MenhirLib. *)

(**A displacement is a nonnegative integer, which, once decoded in a certain
   way, represents a possibly negative offset into a data array. *)
type displacement =
  int

(**A compressed table is represented as a pair of a displacement array and a
   data array. If the functions [get_displacement] and [get_data] offer read
   access to these arrays, then [get get_displacement get_data i j] returns
   the value found at indices [i] and [j] in the compressed table. This call
   is permitted only if the value found at indices [i] and [j] in the original
   table is significant. *)
val get:
  ('displacement -> int -> displacement) ->
  ('data -> int -> 'a) ->
  'displacement * 'data ->
  int -> int ->
  'a

(**The auxiliary function [decode] is part of the implementation of [get].
   It is exposed because it is used by the specialized versions of [get]
   that the table back-end generates. See [TableUtils]. *)
val decode: displacement -> int
