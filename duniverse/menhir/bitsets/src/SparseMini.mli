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

(**This module offers immutable bit sets whose elements are not subject to a
   bound. It is a copy of [SparseBitSet] that has been restricted to a small
   subset of operations. *)

(**A bit set represents a set of nonnegative integers. *)
include API.MINI
  with type elt = int
