(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Range

type 'a located =
  { p : range; v : 'a }

let[@inline] locate p v =
  { p; v }

let[@inline] position { p; _ } =
  p

let[@inline] value { v; _ } =
  v

let map f { p; v } =
  let v = f v in { p; v }

let parenthesize { p; v } =
  locate (Range.decrement p) ("(" ^ v ^ ")")
