(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(* This is a stripped-down copy of the weight-balanced binary trees found in
   the library Baby by François Pottier. *)

type 'v tree =
  | TLeaf
  | TNode of { l : 'v tree; v : 'v; r : 'v tree; w : int }

let[@inline] weight t =
  match t with
  | TLeaf ->
      1
  | TNode { w; _ } ->
      w

let[@inline] cardinal t =
  weight t - 1

let alpha =
  29 (* in percent *)

let[@inline] not_left_heavy wl wr =
  alpha * wl <= (100-alpha) * wr

let[@inline] left_heavy wl wr =
  not (not_left_heavy wl wr)

let[@inline] not_right_heavy wl wr =
  not_left_heavy wr wl

let[@inline] right_heavy wl wr =
  not (not_right_heavy wl wr)

let[@inline] like_weights wl wr =
  not_left_heavy wl wr && not_right_heavy wl wr

let[@inline] siblings l r =
  like_weights (weight l) (weight r)

let rec check t =
  match t with
  | TLeaf ->
      ()
  | TNode { l; r; w; _ } ->
      check l;
      check r;
      assert (w = weight l + weight r);
      assert (siblings l r)

let[@inline] create'' w l v r =
  assert (w = weight l + weight r);
  (* This assertion can fail, (hopefully) just because a double rotation
     can temporarily create a node that does not satisfy the invariant.
  assert (siblings l r);
   *)
  TNode { l; v; r; w }

let[@inline] create l v r =
  let w = weight l + weight r in
  create'' w l v r

let[@inline] create' wl l v wr r =
  assert (wl = weight l && wr = weight r);
  let w = wl + wr in
  create'' w l v r

let[@inline] singleton x =
  let w = 2 in
  create'' w TLeaf x TLeaf

let impossible () =
  assert false

let rotate_left l v r =
  match r with
  | TLeaf -> impossible()
  | TNode { l = rl; v = rv; r = rr; _ } ->
  create (create l v rl) rv rr

let rotate_right l v r =
  match l with
  | TLeaf -> impossible()
  | TNode { l = ll; v = lv; r = lr; _ } ->
  create ll lv (create lr v r)

let balance_right_heavy wl l v wr r =
  assert (wl = weight l && wr = weight r);
  assert (right_heavy wl wr);
  match r with
  | TLeaf -> impossible()
  | TNode { l = rl; v = rv; r = rr; _ } ->
  let wrl = weight rl in
  let wrr = wr - wrl in
  assert (wrr = weight rr);
  if like_weights wl wrl && like_weights (wl + wrl) wrr then
    (* [rotate_left l v r] *)
    let w = wl + wr in
    create'' w (create' wl l v wrl rl) rv rr
  else
    rotate_left l v (rotate_right rl rv rr)

let balance_left_heavy wl l v wr r =
  assert (wl = weight l && wr = weight r);
  assert (left_heavy wl wr);
  match l with
  | TLeaf -> impossible()
  | TNode { l = ll; v = lv; r = lr; _ } ->
  let wll = weight ll in
  let wlr = wl - wll in
  assert (wlr = weight lr);
  if like_weights wlr wr && like_weights wll (wlr + wr) then
    (* [rotate_right l v r] *)
    let w = wl + wr in
    create'' w ll lv (create' wlr lr v wr r)
  else
    rotate_right (rotate_left ll lv lr) v r

(* The following functions are unused.

let[@inline] balance_maybe_right_heavy wl l v wr r =
  assert (wl = weight l && wr = weight r);
  assert (not_left_heavy wl wr);
  if not_right_heavy wl wr then
    create' wl l v wr r
  else
    balance_right_heavy wl l v wr r

let[@inline] balance_maybe_left_heavy wl l v wr r =
  assert (wl = weight l && wr = weight r);
  assert (not_right_heavy wl wr);
  if not_left_heavy wl wr then
    create' wl l v wr r
  else
    balance_left_heavy wl l v wr r

let rec join_maybe_left_heavy l v wr r =
  assert (wr = weight r);
  let wl = weight l in
  assert (not_right_heavy wl wr);
  if not_left_heavy wl wr then
    create' wl l v wr r
  else
    join_left_heavy wl l v wr r

and join_left_heavy wl l v wr r =
  assert (wl = weight l && wr = weight r);
  assert (left_heavy wl wr);
  match l with
  | TLeaf -> impossible()
  | TNode { l = ll; v = lv; r = lr; _ } ->
  let wll = weight ll in
  let wlr = wl - wll in
  assert (wlr = weight lr);
  balance_maybe_right_heavy
    wll ll
    lv
    (wlr + wr) (join_maybe_left_heavy lr v wr r)

let rec join_maybe_right_heavy wl l v r =
  assert (wl = weight l);
  let wr = weight r in
  assert (not_left_heavy wl wr);
  if not_right_heavy wl wr then
    create' wl l v wr r
  else
    join_right_heavy wl l v wr r

and join_right_heavy wl l v wr r =
  assert (wl = weight l && wr = weight r);
  assert (right_heavy wl wr);
  match r with
  | TLeaf -> impossible()
  | TNode { l = rl; v = rv; r = rr; _ } ->
  let wrl = weight rl in
  let wrr = wr - wrl in
  assert (wrr = weight rr);
  balance_maybe_left_heavy
    (wl + wrl) (join_maybe_right_heavy wl l v rl)
    rv
    wrr rr

let join l v r =
  let wl = weight l and wr = weight r in
  if not_left_heavy wl wr then
    if not_right_heavy wl wr then
      create' wl l v wr r
    else
      join_right_heavy wl l v wr r
  else
    join_left_heavy wl l v wr r
 *)

let rec quasi_siblings l r =
  if weight l <= weight r then
    like_weights (weight l) (weight r - 1) ||
    like_weights (weight l + 1) (weight r)
  else
    quasi_siblings r l

let join_quasi_siblings l v r =
  assert (quasi_siblings l r);
  let wl = weight l and wr = weight r in
  if not_left_heavy wl wr then
    if not_right_heavy wl wr then
      create' wl l v wr r
    else
      balance_right_heavy wl l v wr r
  else
    balance_left_heavy wl l v wr r

let empty =
  TLeaf

let rec add cmp x t =
  match t with
  | TLeaf ->
      singleton x
  | TNode { l; v; r; _ } ->
      let c = cmp x v in
      if c = 0 then
        t
      else if c < 0 then
        let l' = add cmp x l in
        if l == l' then t else join_quasi_siblings l' v r
      else
        let r' = add cmp x r in
        if r == r' then t else join_quasi_siblings l v r'

let rec add_absent cmp x t =
  match t with
  | TLeaf ->
      singleton x
  | TNode { l; v; r; _ } ->
      let c = cmp x v in
      assert (c <> 0);
      if c < 0 then
        let l' = add_absent cmp x l in
        if l == l' then t else join_quasi_siblings l' v r
      else
        let r' = add_absent cmp x r in
        if r == r' then t else join_quasi_siblings l v r'

let rec mem cmp x t =
  match t with
  | TLeaf ->
      false
  | TNode { l; v; r; _ } ->
      let c = cmp x v in
      c = 0 || mem cmp x (if c < 0 then l else r)

let rec find cmp x t =
  match t with
  | TLeaf ->
      None
  | TNode { l; v; r; _ } ->
      let c = cmp x v in
      if c = 0 then Some v
      else find cmp x (if c < 0 then l else r)

let rec iter f t =
  match t with
  | TLeaf ->
      ()
  | TNode { l; v; r; _ } ->
      iter f l; f v; iter f r

let rec for_all p t =
  match t with
  | TLeaf ->
      true
  | TNode { l; v; r; _ } ->
      for_all p l && p v && for_all p r

let[@inline] is_singleton t =
  match t with
  | TLeaf ->
      false
  | TNode { w; _ } ->
      (* [w] is [weight t]. The weight of a singleton is 2. Relying on the
         weight removes the need to read the fields [l] and [r]. *)
      w = 2

let[@inline] extract_singleton t =
  match t with
  | TNode { l; v; r; w } ->
      assert (l = TLeaf && r = TLeaf && w = 2);
      v
  | _ ->
      assert false
