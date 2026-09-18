(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

type comment =
  string

type 'a or_comment =
| Thing of 'a
| Comment of comment

type 'a t =
  'a or_comment

let iter f = function
  | Thing s ->
      f s
  | Comment _ ->
      ()

let fold f accu = function
  | Thing s ->
      f accu s
  | Comment _ ->
      accu

let map f = function
  | Thing s ->
      Thing (f s)
  | Comment c ->
      Comment c

let to_option = function
  | Thing s ->
      Some s
  | Comment _ ->
      None

let things xs =
  MList.filter_map to_option xs

let count accu = function
  | Thing _ ->
      accu + 1
  | Comment _ ->
      accu

let count xs =
  List.fold_left count 0 xs

let validate validate c =
  map (validate c)

let rec gather (xs : 'a t list) : ('a * comment list) list =
  match xs with
  | Comment _ :: xs ->
      gather xs
  | Thing x :: xs ->
      gather_thing x [] xs
  | [] ->
      []

and gather_thing x cs xs =
  match xs with
  | Comment c :: xs ->
      gather_thing x (c :: cs) xs
  | _ ->
      (x, List.rev cs) :: gather xs
