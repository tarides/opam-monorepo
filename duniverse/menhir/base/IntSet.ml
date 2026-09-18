(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

include Patricia

type elt = key

(* This is quick and slightly dirty: we implement a set of integers
   as a map of integers to unit values. Thus, we waste one word of
   memory per node. *)

type set =
  unit t

type t = set

let[@inline] singleton x =
  singleton x ()

let[@inline] add x s =
  add x () s

let[@inline] equal s1 s2 =
  equal (fun () () -> true) s1 s2

let[@inline] choose s =
  let x, () = choose s in
  x

let[@inline] iter yield s =
  iter (fun x () -> yield x) s

let[@inline] fold yield s accu =
  fold (fun x () accu -> yield x accu) s accu

let[@inline] elements s =
  fold (fun x xs -> x :: xs) s []

let domain m =
  mapi (fun _k _v -> ()) m

let lift f s =
  mapi (fun k () -> f k) s
