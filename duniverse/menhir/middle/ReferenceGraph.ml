(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open MiddleAPI

module Make (G : GRAMMAR) = struct

  open G
  module Set = NonterminalSet
  module Map = NonterminalMap

  (* Construct the graph in memory. *)

  let forward : Set.t Map.t ref =
    ref Map.empty

  let successors nt =
    try Map.find nt !forward with Not_found -> Set.empty

  let () =
    Production.iter @@ fun prod ->
    let nt1 = Production.nt prod
    and rhs = Production.rhs prod in
    rhs |> Array.iter @@ fun symbol ->
    match symbol with
    | Symbol.T _   -> ()
    | Symbol.N nt2 ->
        forward := Map.add nt1 (Set.add nt2 (successors nt1)) !forward

  (* Prepare to print. *)

  module P = Dot.Print (struct
    type vertex = Nonterminal.t
    let name nt =
      Printf.sprintf "nt%d" (Nonterminal.encode nt)
    let successors (f : ?style:Dot.style -> label:string -> vertex -> unit) nt =
      successors nt |> Set.iter @@ fun successor ->
      f ~label:"" successor
    let iter (f : ?shape:Dot.shape -> ?style:Dot.style -> label:string -> vertex -> unit) =
      Nonterminal.iter @@ fun nt ->
      f ~label:(Nonterminal.print false nt) nt
  end)

  let print filename =
    let f = open_out filename in
    P.print f;
    close_out f

end (* Make *)
