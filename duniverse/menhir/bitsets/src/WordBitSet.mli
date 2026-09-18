(******************************************************************************)
(*                                                                            *)
(*                                  Bitsets                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2025--2025 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers immutable bit sets that fit within one word of memory,
   that is, one OCaml value of type [int]. These bit sets can store integer
   values in the semi-open interval [\[0, bound)], where [bound] is
   [Sys.word_size - 1], that is, usually 63. *)

(**[bound] is [Sys.word_size - 1], that is, usually 63. *)
val bound: int

(**A bit set represents a set of integers and is itself an integer value. *)
include API.SET
  with type elt = int
   and type t = private int

(**/**)

(* The following operations are offered by the module [WordBitSet] but are
   not offered by other varieties of bit sets. Therefore they are not part
   of the signature [SET]. We do not document their existence. *)

(* [iter_delta] and [fold_delta] are generalized variants of [iter] and
   [fold]. They add the constant [delta] on the fly to each set element
   before yielding it. *)

val iter_delta: int -> (elt -> unit) -> t -> unit
val fold_delta: int -> (elt -> 'b -> 'b) -> t -> 'b -> 'b

(**[shift s delta] shifts the elements of the set [s] up by [delta],
   where [0 <= delta <= bound] must hold. The result is returned as
   a pair of two sets, namely:
   - the set [{ x + delta         | x ∈ set, x + delta  < bound }] and
   - the set [{ x + delta - bound | x ∈ set, x + delta >= bound }]. *)
val shift : t -> int -> t * t

(**[check] is used only during testing. *)
val check : t -> unit
