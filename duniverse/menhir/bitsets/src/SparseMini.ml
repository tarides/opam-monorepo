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

module W =
  WordMini

type offset =
  int

let[@inline] compare_offsets (o1 : offset) (o2 : offset) : int =
  compare o1 o2

type elt =
  int

type word =
  W.t

type t =
  | N
  | C of offset * word * t

let empty =
  N

let[@inline] singleton x =
  let i = x mod W.bound in
  let base = x - i in
  (* This is [add1 base i empty], specialized. *)
  C (base, W.singleton i, empty)

let rec union s1 s2 =
  match s1, s2 with
  | N, s
  | s, N ->
      s
  | C (o1, w1, qs1), C (o2, w2, qs2) ->
      if o1 < o2 then
        let qs1' = union qs1 s2 in
        if qs1 == qs1' then s1 else
        C (o1, w1, qs1')
      else if o1 > o2 then
        let qs2' = union s1 qs2 in
        if qs2 == qs2' then s2 else
        C (o2, w2, qs2')
      else
        let w = W.union w1 w2 in
        let qs = union qs1 qs2 in
        if W.equal w2 w && qs2 == qs then s2 else
        if W.equal w1 w && qs1 == qs then s1 else
        C (o1, w, qs)

let rec equal s1 s2 =
  s1 == s2 ||
  match s1, s2 with
  | N, N ->
      true
  | C _, N
  | N, C _ ->
      false
  | C (o1, w1, qs1), C (o2, w2, qs2) ->
      o1 = o2 && W.equal w1 w2 && equal qs1 qs2

let rec compare s1 s2 =
  if s1 == s2 then 0 else
  match s1, s2 with
  | N  , N ->  0
  | C _, N -> +1
  | N, C _ -> -1
  | C (o1, w1, qs1), C (o2, w2, qs2) ->
      let c = compare_offsets o1 o2 in if c <> 0 then c else
      let c = W.compare w1 w2 in if c <> 0 then c else
      let c = compare qs1 qs2 in c
