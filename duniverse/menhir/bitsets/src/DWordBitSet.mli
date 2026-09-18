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

(**This module offers immutable bit sets that fit within two words of
   memory. These bit sets can store integer values in the semi-open interval
   [\[0, bound)], where [bound] is [2 * WordBitSet.bound], that is, usually
   126. *)

(**[bound] is [2 * WordBitSet.bound], that is, usually 126. *)
val bound: int

(**A bit set represents a set of integers. *)
include API.SET
  with type elt = int

(**/**)

(**[check] is used only during testing. *)
val check : t -> unit
