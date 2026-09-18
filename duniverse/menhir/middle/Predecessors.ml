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

module Make (A : MINIMAL_LR1_AUTOMATON) = struct
open A.Lr0.G

include Fix.Numbering.Operations(A)

(* -------------------------------------------------------------------------- *)

let _predecessors : A.node list array =
  Array.make A.n []

let record_predecessor source target =
  let i = A.encode target in
  _predecessors.(i) <- source :: _predecessors.(i)

let () =
  iter @@ fun source ->
  A.transitions source |> SymbolMap.iter @@ fun _symbol target ->
  record_predecessor source target

let predecessors target =
  let i = A.encode target in
  _predecessors.(i)

(* -------------------------------------------------------------------------- *)

end (* Make *)
