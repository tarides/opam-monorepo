(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

module Unbounded = struct

  (* We represent integer maps as Patricia trees. *)

  include Patricia

  (* We represent integer sets as Patricia trees. *)

  module Domain = IntSet

  let domain = IntSet.domain
  let lift = IntSet.lift

end

module Make (N : sig
  val n: int
end) ()
= struct

  (* We represent integer maps as Patricia trees. *)

  include Patricia

  (* We represent integer sets as bit sets. *)

  (* Bit sets can be 250 times more compact than Patricia trees.
     Indeed, for values of [n] up to 252,
     a bit set occupies 5 words,
     whereas a Patricia tree would occupy 4 words per node.  *)

  module Domain =
    Bitsets.BoundedBitSet.Make(N)()

  (* Conversions. *)

  let domain m =
    fold (fun k _v s ->
      Domain.add k s
    ) m Domain.empty

  let lift f s =
    Domain.fold (fun k m ->
      add k (f k) m
    ) s empty

end
