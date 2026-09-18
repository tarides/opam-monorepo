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

(**This module offers immutable bit sets that can store integer values in the
   semi-open interval [\[0, bound)], where [bound] is chosen by the user. *)

(**[Make] expects an integer parameter [n] and provides an implementation of
   sets of integers, where every integer element must lie in the semi-open
   interval [\[0, n)]. *)
module Make (N : sig
  val n: int
end) ()
: sig
  (**[bound] is equal to the user-supplied bound [n]. *)
  val bound : int
  include API.SET with type elt = int
end
