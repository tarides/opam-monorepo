(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Report

exception Invalid

type ('a, 'b) validator =
  channel -> 'a -> 'b (* may raise [Invalid] *)

let option validate c o =
  Option.map (validate c) o

let list validate c xs =
  List.map (validate c) xs

let robustly validate c x =
  try Some (validate c x) with Invalid -> None

let robust_list validate c xs =
  xs |> MList.filter_map @@ fun x ->
  robustly validate c x
