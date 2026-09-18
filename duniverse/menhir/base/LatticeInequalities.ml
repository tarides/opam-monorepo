(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

module Make (M : Fix.IMPERATIVE_MAPS) (P : Fix.MINIMAL_SEMI_LATTICE) () = struct

  type variable =
    M.key

  type property =
    P.property

  let join =
    P.leq_join

  (* A map of each variable to its upper bounds (its successors). *)

  let upper : variable list M.t =
    M.create()

  let successors x =
    try M.find x upper with Not_found -> []

  let edge x y =
    M.add x (y :: successors x) upper

  (* A map of each variable to its lower bound (a constant). *)

  let lower : property M.t =
    M.create()

  let source p y =
    let p' =
      match M.find y lower with
      | exception Not_found ->
          p
      | q ->
          join p q
    in
    M.add y p' lower

  (* Running the analysis. *)

  module Solve () = struct

    module G = struct
      type nonrec variable = variable
      type nonrec property = property
      let foreach_root contribute =
        M.iter contribute lower
      let foreach_successor x p contribute =
        List.iter (fun y -> contribute y p) (successors x)
    end

    include Fix.DataFlow.Run(M)(P)(G)

  end

end
