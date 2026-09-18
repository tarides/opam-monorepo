(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

type property = int

type t = property

let bottom =
  0

let infinity =
  max_int

let[@inline] finite n =
  n

let equal : property -> property -> bool =
  (=)

let[@inline] is_maximal p =
  p = infinity

let max : property -> property -> property =
  max

let max_lazy p q =
  if p = infinity then infinity else max p (q())

let add p q =
  if p = infinity || q = infinity then
    infinity
  else
    let n = p + q in
    assert (n < infinity);
    n

let add_lazy p q =
  if p = infinity then
    infinity
  else
    let q = q() in
    if q = infinity then
      infinity
    else
      let n = p + q in
      assert (n < infinity);
      n

let print p =
  if p = infinity then "infinity" else string_of_int p

let[@inline] to_int p =
  p
