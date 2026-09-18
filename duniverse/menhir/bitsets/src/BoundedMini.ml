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

module Make (N : sig
  val n: int
end) ()
= struct
  open N

  let () =
    assert (n >= 0)

  let bound = n

  module type MINI =
    API.MINI with type elt = int

  include (val
    if n <= WordMini.bound then
      (module WordMini : MINI)
    else
      (module SparseMini : MINI)
    : MINI)

end
