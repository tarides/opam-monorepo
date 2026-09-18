(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open PlainSyntax
open PlainSyntaxAccessors

let has_final_error_only branch : bool =
  let producers = Array.of_list branch.producers in
  let n = Array.length producers in
  MRef.with_state true @@ fun ok ->
  producers |> Array.iteri @@ fun i producer ->
  if i < n - 1 && producer.prod_symbol = "error" then
    ok := false

let final_only c reason grammar =
  let keep branch : bool =
    if has_final_error_only branch then
      true
    else begin
      Report.warning c [branch.branch_position]
        "%sthe error token may appear only at the end of a production\n\
         (and the semantic action must abort the parser).\n\
         This production will be ignored."
        reason;
      false
    end
  in
  let filter rule = { rule with branches = List.filter keep rule.branches } in
  { grammar with rules = StringMap.map filter grammar.rules }

let nowhere c reason grammar =
  let keep branch : bool =
    if error_free_branch branch then
      true
    else begin
      Report.warning c [branch.branch_position]
        "%sthe error token must not be used.\n\
         This production will be ignored."
        reason;
      false
    end
  in
  let filter rule = { rule with branches = List.filter keep rule.branches } in
  { grammar with rules = StringMap.map filter grammar.rules }
