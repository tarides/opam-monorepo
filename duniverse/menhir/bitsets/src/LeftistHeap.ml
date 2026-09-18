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

module Make (Key : sig
  type t
  val compare: t -> t -> int
end) = struct

  type key =
    Key.t

  type 'a heap =
    | Leaf
    | Node of 'a heap * key * 'a * 'a heap * int

  type 'a t =
    'a heap

  let empty =
    Leaf

  let[@inline] singleton k v =
    Node (Leaf, k, v, Leaf, 1)

  let[@inline] rank t =
    match t with
    | Leaf ->
        0
    | Node (_, _, _, _, rank) ->
        rank

  let rec check (parent_key : key option) t =
    match t with
    | Leaf ->
        ()
    | Node (l, k, _v, r, rk) ->
        check (Some k) l;
        check (Some k) r;
        (* The rank [rk] is the length of the right spine. *)
        assert (rk = rank r + 1);
        match parent_key with None -> () | Some pk ->
        (* The heap property holds: the key [k] of this node is less
           than the key [pk] of the parent node. *)
        assert (Key.compare pk k <= 0)

  let check t =
    check None t

  let rec merge t1 t2 =
    match t1, t2 with
    | Leaf, t
    | t, Leaf ->
        t
    | Node (l1, k1, v1, r1, _), Node (l2, k2, v2, r2, _) ->
        merge_node_node
          l1 k1 v1 r1 t1
          l2 k2 v2 r2 t2

  (* This is [merge t1 t2] where [t2] is a node. *)
  and merge_node t1 l2 k2 v2 r2 t2 =
    match t1 with
    | Leaf ->
        t2
    | Node (l1, k1, v1, r1, _) ->
        merge_node_node
          l1 k1 v1 r1 t1
          l2 k2 v2 r2 t2

  (* This is [merge t1 t2] where [t1] and [t2] are nodes. *)
  and merge_node_node l1 k1 v1 r1 t1 l2 k2 v2 r2 t2 =
    if Key.compare k1 k2 > 0 then
      merge_node_node_ordered
        l2 k2 v2 r2 (* t2 *)
        l1 k1 v1 r1 t1
    else
      merge_node_node_ordered
        l1 k1 v1 r1 (* t1 *)
        l2 k2 v2 r2 t2

  (* This is [merge t1 t2] where [t1] and [t2] are nodes
     and [k1] is less than or equal to [k2]. *)
  and merge_node_node_ordered l1 k1 v1 r1 (* t1 *) l2 k2 v2 r2 t2 =
    assert (Key.compare k1 k2 <= 0);
    let r1t2 =
      (* [merge r1 t2] *)
      merge_node r1
        l2 k2 v2 r2 t2
    in
    let lrank = rank l1
    and rrank = rank r1t2 in
    if lrank >= rrank
    then Node (l1, k1, v1, r1t2, rrank + 1)
    else Node (r1t2, k1, v1, l1, lrank + 1)
         (* left becomes right due to being shorter *)

  let[@inline] insert k v t =
    merge (singleton k v) t

  let pop (t : 'a heap) : ((key * 'a) * 'a heap) option =
    match t with
    | Leaf ->
        None
    | Node (l, k, v, r, _) ->
        Some ((k, v), merge l r)

  type 'a pop2 =
    | Head of key * 'a * key * 'a * 'a heap
    | Tail of key * 'a
    | Done

  let pop2 (t : 'a heap) : 'a pop2 =
    match t with
    | Leaf ->
        Done
    | Node (Leaf, k, v, _, _) ->
        Tail (k, v)
    | Node (Node (ll, lk, lv, lr, _), k, v, Leaf, _) ->
        Head (k, v, lk, lv, merge ll lr)
    | Node (
        (Node (ll, lk, lv, lr, _) as l),
        k, v,
        (Node (rl, rk, rv, rr, _) as r),
        _
      ) ->
        if Key.compare lk rk <= 0
        then Head (k, v, lk, lv, merge (merge ll lr) r)
        else Head (k, v, rk, rv, merge (merge rl rr) l)

end
