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

(**This module offers immutable bit sets that fit within one word of memory.
   It is a copy of [WordBitSet] that has been restricted to a small subset
   of operations. *)

(**[bound] is [Sys.word_size - 1], that is, usually 63. *)
val bound: int

(**A bit set represents a set of integers and is itself an integer value. *)
include API.MINI
  with type elt = int
   and type t = private int
