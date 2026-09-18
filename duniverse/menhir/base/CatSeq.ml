(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* We maintain the invariant that the children of [SConcat] are never
   empty. This is exploited in [first]. Also, this allows us to claim
   that the complexity of [elements] is linear in the length of the
   sequence. *)

type 'a seq =
| SZero
| SOne of 'a
| SConcat of 'a seq * 'a seq

let empty =
  SZero

let[@inline] singleton x =
  SOne x

let append xs ys =
  match xs, ys with
  | SZero, _ ->
      ys
  | _, SZero ->
      xs
  | _, _ ->
      SConcat (xs, ys)

let rec concat xss =
  match xss with
  | [] ->
      empty
  | xs :: xss ->
      append xs (concat xss)

let rec elements xs accu =
  match xs with
  | SZero ->
      accu
  | SOne x ->
      x :: accu
  | SConcat (xs1, xs2) ->
      elements xs1 (elements xs2 accu)

let[@inline] elements xs =
  elements xs []

let rec first xs =
  match xs with
  | SZero ->
      (* We disallow applying [first] to an empty sequence. *)
      assert false
  | SOne x ->
      x
  | SConcat (xs1, _) ->
      (* Our invariant guarantees that [xs1] is nonempty. *)
      first xs1
