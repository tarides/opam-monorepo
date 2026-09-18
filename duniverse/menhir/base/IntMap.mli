(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers two implementations of integer sets and integer maps,
   with conversions between sets and maps. *)

(**The first implementation, [Unbounded], uses Patricia trees to implement
   both integer sets and integer maps. There is no restriction on the integer
   values that are used as elements or keys. *)
module Unbounded
: BaseAPI.MAP_AND_SET with type key = int

(**The second implementation, [Make], expects an integer parameter [n] and
   provides integer sets and integer maps where every element or key must lie
   in the semi-open interval [\[0, n)]. Integer sets are implemented as bit
   sets; integer maps are implemented as Patricia trees. *)
module Make (N : sig
  val n: int
end) ()
: BaseAPI.MAP_AND_BITSET with type key = int
