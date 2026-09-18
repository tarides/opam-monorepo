(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* The construction of the canonical LR(1) automaton is formulated as
   a forward graph traversal. *)

open MiddleAPI

module Make (Lr0 : LR0) = struct
module Lr0 = Lr0
open Lr0.G

type lr0state =
  Lr0.node

type lr1state =
  Lr0.ALR1.t

(* -------------------------------------------------------------------------- *)

(* Give an implicit definition of the graph that we wish to traverse. *)

module G = struct

  type t = lr1state

  let foreach_root f =
    Lr0.entry |> ProductionMap.iter @@ fun _prod (c : lr0state) ->
    f (Lr0.ALR1.start c)

  let foreach_successor (state : lr1state) f =
    Lr0.foreach_outgoing_edge (Lr0.ALR1.core state) @@ fun symbol _ ->
    let successor = Lr0.ALR1.transition symbol state in
    f successor

end

(* -------------------------------------------------------------------------- *)

(* Traversing this graph yields a numbering of the LR(1) states in the
   canonical automaton. *)

include Fix.GraphNumbering.ForOrderedType(Lr0.ALR1)(G)
  (* This defines [n : int],
                  [encode : lr1state -> node],
                  [decode : node -> lr1state]. *)

type node =
  int

type t =
  node

(* -------------------------------------------------------------------------- *)

(* Expose the mapping of nodes to LR(1) states. *)

let state : node -> lr1state =
  decode

(* -------------------------------------------------------------------------- *)

(* Expose the entry nodes of the LR(1) automaton. *)

let entry : node ProductionMap.t =
  Lr0.entry |> ProductionMap.map @@ fun (c : lr0state) ->
  encode (Lr0.ALR1.start c)

(* -------------------------------------------------------------------------- *)

(* Expose the transitions of the LR(1) automaton. *)

let[@inline] transition symbol (i : node) : node =
  encode (Lr0.ALR1.transition symbol (state i))

let transitions (i : node) : node SymbolMap.t =
  Lr0.outgoing_symbols (Lr0.ALR1.core (state i)) @@ fun symbol ->
  transition symbol i

(* -------------------------------------------------------------------------- *)

(* Expose the bijection between nodes and numbers. *)

let encode (i : node) : int = i
let decode (i : int) : node = i

(* -------------------------------------------------------------------------- *)

end (* Make *)
