(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

open GSS

(* Reduction paths. *)

(* An (immutable) explicit representation of reduction paths is needed,
   because (following McPeak) we insert reduction paths into a priority
   queue. *)

(* A reduction path begins at a top node that is able to reduce a production
   [prod] and follows [n] edges where [n] is the length of this production. *)

(* We want [grow] to be cheap, so we represent a path as a reversed list of
   edges. (This also lets us share path prefixes in [foreach_path].) *)

type ('s, 'v) t =
  | Empty of ('s, 'v) node
  | Edge of ('s, 'v) edge * ('s, 'v) t

let[@inline] empty top =
  Empty top

(* One might think that, as soon as an edge becomes part of a reduction path,
   its semantic value should no longer be modified, so this edge should be
   locked. Therefore, [grow edge path] should lock the edge [edge]. This is
   incorrect; locking the edge at this point would be too early. The reduction
   path that is being constructed here will be inserted into the reduction
   queue, but will not be processed immediately. A path's edges should be
   locked only when a reduction along this path is actually performed. *)

let[@inline] grow edge path =
  Edge (edge, path)

let[@inline] leftmost path =
  match path with
  | Empty top ->
      top
  | Edge (edge, _path) ->
      edge.node

let rec rightmost path =
  match path with
  | Empty top ->
      top
  | Edge (_, path) ->
      rightmost path

let rec length path =
  match path with Empty _ -> 0 | Edge (_, path) -> 1 + length path

let extract path =
  assert (length path = 1);
  match path with
  | Empty _ ->
      assert false
  | Edge (edge, _path) ->
      edge

let rec lock path =
  match path with
  | Empty _ ->
      ()
  | Edge (edge, path) ->
      edge.locked <- true;
      lock path
