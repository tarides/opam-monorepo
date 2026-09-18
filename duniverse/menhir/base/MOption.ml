(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let hash hash = function
  | Some x ->
      hash x
  | None ->
      Hashtbl.hash None

let split o =
  match o with
  | None ->
      None, None
  | Some (x, x') ->
      Some x, Some x'

let sub (<=) o1 o2 =
  match o1, o2 with
  | None, _ ->
      true
  | Some _, None ->
      false
  | Some x1, Some x2 ->
      x1 <= x2

let fold f o accu =
  match o with
  | None ->
      accu
  | Some x ->
      f x accu
