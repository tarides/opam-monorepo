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

let kill_properties props =
  let tk_associativity = UndefinedAssoc
  and tk_precedence = UndefinedPrecedence in
  { props with tk_associativity; tk_precedence }

let kill_tokens tokens =
  tokens
  |> StringMap.map kill_properties
  |> StringMap.filter (fun _ props -> props.tk_is_declared)

let kill_branch branch =
  { branch with prec_annotation = None }

let kill_rule rule =
  { rule with branches = List.map kill_branch rule.branches }

let kill grammar =
  let tokens = kill_tokens grammar.tokens
  and on_error_reduce = StringMap.empty
  and rules = StringMap.map kill_rule grammar.rules in
  { grammar with tokens; on_error_reduce; rules }
