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

module Make (Lr0 : LR0) = struct
open Lr0.G

(* This flag, which is reserved for internal use, causes more information
   about SLR(1) conflict states to be printed. *)

let tell_me_everything =
  false

(* [make_slr_state] turns an LR(0) state into a closed SLR(1) state. *)

let make_slr_state (s : Lr0.node) : Lr0.CLR1.t =

  (* Obtain the set of LR(0) items associated with the state [s]. *)

  let items = Lr0.items s in

  (* Unfortunately, this set is not closed. We do not have a function that
     computes the closure of a set of LR(0) items -- we could build one using
     the functor [Lr0.Closure], but that would be overkill. So, we first
     convert this set to a set of LR(1) items, then compute the closure at
     this level, and finally turn this LR(1) state into an SLR(1) state by
     letting the lookahead sets be the FOLLOW sets. This is somewhat ugly and
     naïve, but seems to work. *)

  (* Convert this set to a set of LR(1) items. Here, we can use any set of
     terminal symbols as the lookahead set. We use the empty set. *)

  let s = Item.Map.lift (fun _item -> TerminalSet.empty) items in

  (* Compute the LR(1) closure. *)

  let s = Lr0.CLR1.closure s in

  (* We now have an LR(1) state that has the correct set of LR(0) items but
     phony lookahead information. We convert it into an SLR(1) state by
     deciding that, for each item, the lookahead set is the FOLLOW set of the
     symbol that appears on the left-hand side of the item. *)

  Item.Map.fold (fun item ts accu ->
    let prod, _pos = Item.export item in
    let nt = Production.nt prod in
    let follow_nt = Analysis.follow nt in
    assert (TerminalSet.subset ts follow_nt); (* sanity check *)
    Item.Map.add item follow_nt accu
  ) s Item.Map.empty

let count_violations () : int =
  let count = ref 0 in
  Lr0.iter (fun s ->
  let s = make_slr_state s in
  if Lr0.CLR1.has_conflict s then
    let () = incr count in
    if tell_me_everything then
      Printf.fprintf
        stderr
        "The following SLR(1) state has a conflict:\n%s"
        (Lr0.CLR1.print "" s)
  );
  !count

end (* Make *)
