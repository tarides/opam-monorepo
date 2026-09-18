(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open StringSet
let value = Located.value
open Syntax

let rec fn_parameter accu (p : parameter) =
  (* [p] cannot be [ParamAnonymous _]. *)
  let x, ps = Parameter.destruct p in
  let accu = add (value x) accu in
  fn_parameters accu ps

and fn_parameters accu ps =
  List.fold_left fn_parameter accu ps

let fn_producer accu ((_, p, _) : producer) =
  fn_parameter accu p

let fn_branch accu branch =
  List.fold_left fn_producer accu branch.pb_producers

let fn_branches accu branches =
  List.fold_left fn_branch accu branches

(* Public functions. *)

let branches branches =
  fn_branches empty branches

let rule rule =
  diff
    (branches rule.pr_branches)
    (of_list rule.pr_parameters)
