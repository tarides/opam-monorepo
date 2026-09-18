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

module Run (A : LR1_AUTOMATON) = struct
include A
open Lr0.G

let start_time =
  Time.start()

include Fix.Numbering.Operations(A)

(* -------------------------------------------------------------------------- *)

(* Mutable state. *)

(* This array is indexed by production indices. *)

(* The array [_sites] maps each production to a list of the nodes where this
   production can be reduced. *)

let _sites : A.node list array =
  Production.init @@ fun _ -> []

(* -------------------------------------------------------------------------- *)

(* Read and write accessors. *)

let[@inline] reduction_sites prod =
  _sites.(Production.encode prod)

let[@inline] record prod node =
  let i = Production.encode prod in
  _sites.(i) <- node :: _sites.(i)

(* -------------------------------------------------------------------------- *)

(* Populate the array. *)

let () =
  iter @@ fun node ->
  reductions node |> TerminalMap.iter @@ fun _t prods ->
  prods |> List.iter @@ fun prod ->
  record prod node

(* -------------------------------------------------------------------------- *)

(* Diagnostics. *)

(* We warn about productions that clearly can never be reduced because there
   is no state in which they can be reduced. *)

(* There could be other productions that are never reduced because the only
   configurations (state & lookahead symbol) where they can be reduced are
   unreachable. We do not detect or report those productions, because 1- this
   requires a more complex reachability analysis (see [LRijkstraFast]) and 2-
   in fact, through the use of the inspection API, it might be possible to
   bring the automaton into a state where one of them can be reduced. *)

let diagnostics c =
  let count = ref 0 in
  let () =
    Production.iter @@ fun prod ->
    if reduction_sites prod = [] then
      let () = incr count in
      match Production.test_start prod with
      | Some nt ->
          Report.warning c (Nonterminal.positions nt)
            "symbol %s is never accepted."
            (Nonterminal.print false nt)
      | None ->
          Report.warning c (Production.positions prod)
            "production %s is never reduced."
            (Production.print prod)
  in
  if !count > 0 then
    let plural_mark, be = if !count > 1 then ("s", "are") else ("", "is") in
    Report.warning c []
      "in total, %d production%s %s never reduced." !count plural_mark be

(* -------------------------------------------------------------------------- *)

let () =
  Time.stop start_time "Computing reduction sites"

end (* Run *)
