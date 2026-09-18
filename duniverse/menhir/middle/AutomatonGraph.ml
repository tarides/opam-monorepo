(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf
open MiddleAPI

module Make (A : MINIMAL_LR1_AUTOMATON) = struct
module Lr0 = A.Lr0
  open Lr0.G

(* To avoid requesting [A : LR1], we copy a few lines of code from [Lr1]. *)

let iter yield =
  for i = 0 to A.n - 1 do yield (A.decode i) done

let core node =
  Lr0.ALR1.core (A.state node)

let incoming_symbol node =
  Lr0.incoming_symbol (core node)

open Dot

module P = Print (struct

  type vertex =
    A.node

  let name node =
    sprintf "s%d" (A.encode node)

  (* Shift transitions are solid; goto transitions are dashed. *)

  let edge_style symbol =
    if Symbol.is_terminal symbol then Solid else Dashed

  let successors (f : ?style:style -> label:string -> vertex -> _) source =
    A.transitions source |> SymbolMap.iter @@ fun symbol target ->
    (* Edges are unlabeled, because it is more economical to place the edge
       label in the target node. Some whitespace is artificially placed in the
       label to obtain better placement of nodes and edges. *)
    let style = edge_style symbol in
    let label = "        " in
    f ~style ~label target

  let vertex_label node =
    match incoming_symbol node with
    | None ->
        sprintf "%d" (A.encode node)
    | Some nt ->
        (* The incoming symbol and the node number, stacked vertically. *)
        sprintf "{%s|%d}"
          (Symbol.print false nt)
          (A.encode node)

  let iter (f : ?shape:shape -> ?style:style -> label:string -> vertex -> _) =
    iter @@ fun node ->
    let label = vertex_label node in
    f ~shape:Record ~label node

end)

let print filename =
  let f = open_out filename in
  P.print f;
  close_out f

end (* Make *)
