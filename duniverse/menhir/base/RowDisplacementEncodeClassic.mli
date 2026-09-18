(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**Row displacement aims to compress a two-dimensional table where some values
   are considered insignificant.

   A faster compression algorithm is available in [RowDisplacementEncode].
   The "classic" implementation offered in this module will be removed in
   the future. *)

(**A displacement is a nonnegative integer, which, once decoded in a certain
   way, represents a possibly negative offset into a data array. *)
type displacement =
  int

(**A compressed table is represented as a pair of a displacement array and a
   data array. *)
type 'a table =
  displacement array * 'a array

(**[compress insignificant dummy t] turns the two-dimensional table [t] into a
   compressed table. The parameter [insignificant] determines which data
   values are insignificant, and can thus be overwritten with other values.
   The parameter [dummy] is used to fill holes in the data array. The type
   ['a] must support OCaml's polymorphic equality test. *)
val compress:
  ('a -> bool) ->
  'a ->
  'a array array ->
  'a table
