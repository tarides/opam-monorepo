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

(**This module offers an implementation of mutable bit vectors, that is,
   mutable resizable arrays of bits.

   Although a bit vector can be thought of as a sequence of bits, it can also
   be thought of as a vector of words. The functions {!length}, {!grow},
   {!get}, {!set} use or return addresses that are measured in words. In a
   vector, the address 0 denotes the first word; the address 1 denotes the
   second word; and so on. *)

(**A mutable vector of bits. *)
type vector

(**A word is a group of [n] bits, where the constant [n] is
   [WordBitSet.bound], that is, typically 63. *)
type word =
  WordBitSet.t

(**An address is measured in words.*)
type address =
  int

(**[create()] returns a fresh vector of length 1,
   initialized with 0 bits. *)
val create : unit -> vector

(**[length v] returns the current length of the vector [v] in words. *)
val length : vector -> address

(**[grow v a] grows the vector [v], if necessary, so that the address [a]
   is a valid address in this vector. When the vector is grown, the new
   space is initialized with 0 bits. *)
val grow : vector -> address -> unit

(**[get v a] reads a word at address [a] in the vector [v].
   The address [a] must be a valid address in the vector [v]. *)
val get : vector -> address -> word

(**[set v a w] writes the word [w] at address [a] in the vector [v].
   The address [a] must be a valid address in the vector [v]. *)
val set : vector -> address -> word -> unit

(**[width v] returns the width of the vector [v], measured in bits. If no bit
   is set in the vector [v] then its width is zero. If the most significant
   bit that is set in the vector [v] lies at bit index [i] then the width of
   the vector [v] is [i+1]. *)
val width : vector -> int
