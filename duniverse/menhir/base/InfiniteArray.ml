(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

type 'a t = {
    default: 'a;
    mutable table: 'a array;
    mutable extent: int;
      (* [extent] is the index of the greatest [set] ever, plus one *)
  }

let default_size =
  16384 (* must be non-zero *)

let make x = {
  default = x;
  table = Array.make default_size x;
  extent = 0;
}

let rec new_length length i =
  if i < length then
    length
  else
    new_length (2 * length) i

let grow a i =
  let n = Array.length a.table in
  let n' = new_length (2 * n) i in
  let table' = Array.make n' a.default in
  Array.blit a.table 0 table' 0 n;
  a.table <- table'

let[@inline] ensure a i =
  assert (0 <= i);
  let n = Array.length a.table in
  if n <= i then grow a i

let[@inline] get a i =
  ensure a i;
  Array.unsafe_get a.table (i)

let[@inline] set a i x =
  ensure a i;
  Array.unsafe_set a.table (i) x;
  if a.extent <= i then
    a.extent <- i + 1

let[@inline] extent a =
  a.extent

let[@inline] domain a =
  Array.sub a.table 0 a.extent
