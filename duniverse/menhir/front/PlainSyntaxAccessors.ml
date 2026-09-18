(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Located
open PlainSyntax

let prod_id producer =
  value producer.prod_id

let prod_attributes producer =
  producer.prod_attributes

let ocamltype_of_token grammar symbol =
  match StringMap.find symbol grammar.tokens with
  | properties ->
      assert properties.tk_is_declared;
      properties.tk_ocamltype
  | exception Not_found ->
      assert false

let ocamltype_of_symbol grammar symbol =
  try
    Some (StringMap.find symbol grammar.types)
  with Not_found ->
    None

let ocamltype_of_start_symbol grammar symbol =
  try
    StringMap.find symbol grammar.types
  with Not_found ->
    assert false

(* Due to the combined use of [StringMap.fold] and [::], the following three
   functions produce lists that are sorted in reverse alphabetical order. To
   avoid surprising the user, it would be nice to correct this; however this
   would visibly change the output of Menhir (by renumbering states, etc.)
   so I prefer not to. *)

let tokens grammar =
  StringMap.fold (fun token properties tokens ->
    if properties.tk_is_declared
    then token :: tokens
    else tokens
  ) grammar.tokens []

let typed_tokens grammar =
  StringMap.fold (fun token properties tokens ->
    if properties.tk_is_declared
    then (token, properties.tk_ocamltype) :: tokens
    else tokens
  ) grammar.tokens []

let nonterminals grammar =
  StringMap.fold (fun nt _ rules -> nt :: rules) grammar.rules []

let[@inline] foreach_nonterminal grammar yield =
  StringMap.iter (fun nt _rule ->
    yield nt
  ) grammar.rules

let[@inline] foreach_branch grammar yield =
  StringMap.iter (fun _nt rule ->
    List.iter yield rule.branches
  ) grammar.rules

let error_free_symbol symbol =
  symbol <> "error"

let error_free_producer producer =
  error_free_symbol producer.prod_symbol

let error_free_branch branch =
  List.for_all error_free_producer branch.producers

let error_free_rule rule =
  List.for_all error_free_branch rule.branches

let error_free_grammar grammar =
  StringMap.for_all (fun _nt rule -> error_free_rule rule) grammar.rules

let grammar_uses_error_token grammar =
  not (error_free_grammar grammar)

let is_terminal grammar symbol =
  StringMap.mem symbol grammar.tokens || symbol = "error"

let is_nonterminal grammar symbol =
  let result = StringMap.mem symbol grammar.rules in
  assert (
    result || is_terminal grammar symbol ||
    (Printf.eprintf "DEBUG: unknown symbol: %s\n%!" symbol; false)
  );
  result

let alias grammar symbol =
  try
    let properties = StringMap.find symbol grammar.tokens in
    assert properties.tk_is_declared;
    properties.tk_alias
  with Not_found ->
    assert false

let unquoted_alias grammar symbol =
  alias grammar symbol
  |> Option.map MString.unquote

let rec print_production nt branch =
  Printf.sprintf "%s ->%s" nt (print_production_rhs branch.producers)

and print_production_rhs producers =
  MString.with_buffer 80 @@ fun b ->
  producers |> List.iter @@ fun producer ->
  Printf.bprintf b " %s" producer.prod_symbol

type order =
  | Lt
  | Gt
  | Eq
  | Incomparable

let precedence_order p1 p2 : order =
  match p1, p2 with
  | UndefinedPrecedence, _
  | _, UndefinedPrecedence ->
      Incomparable
  | PrecedenceLevel fl1, PrecedenceLevel fl2 ->
      let file1, level1 = value fl1
      and file2, level2 = value fl2 in
      if not (InputFile.equal file1 file2) then
        Incomparable
      else if level1 > level2 then
        Gt
      else if level1 < level2 then
        Lt
      else
        Eq

let production_order p1 p2 : order =
  match p1, p2 with
  | ProductionLevel (file1, level1), ProductionLevel (file2, level2) ->
      if not (InputFile.equal file1 file2) then
        Incomparable
      else if level1 > level2 then
        Gt
      else if level1 < level2 then
        Lt
      else
        Eq

let transform_branch_producers f branch =
  { branch with producers = f branch.producers }
