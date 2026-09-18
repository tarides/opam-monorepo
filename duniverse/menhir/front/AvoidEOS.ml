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

(* It is really painful to build pieces of grammars in abstract syntax. *)

(* We introduce a new terminal symbol [EOF]. *)

let eof =
  "MENHIR_EOF"

let properties = {
  tk_ocamltype = None;
  tk_position = Range.dummy;
  tk_alias = None;
  tk_attributes = [];
  tk_associativity = UndefinedAssoc;
  tk_precedence = UndefinedPrecedence;
  tk_is_declared = true;
}

(* For each start symbol [S], we introduce a new start symbol
   [S'], defined by the production [S' := S EOF]. *)

let rename symbol =
  "_menhir_" ^ symbol

let producer id symbol =
  let prod_id = Located.locate Range.dummy id
  and prod_symbol = symbol
  and prod_attributes = []
  and prod_inlined = false in
  { prod_id; prod_symbol; prod_attributes; prod_inlined }

let new_start_branch symbol =
  let branch_position = Range.dummy
  and producers = [ producer "_1" symbol; producer "_2" eof ]
  and action = Action.make 0 StringSet.empty [] IL.EUnit
  and prec_annotation = None
  and production_level = ProductionLevel (InputFile.builtin, 0)
  and br_attributes = [] in
  { branch_position; producers; action;
    prec_annotation; production_level; br_attributes }

let new_start_rule symbol =
  let branches = [new_start_branch symbol]
  and positions = []
  and inline_flag = false
  and attributes = []
  and merge = None in
  { branches; positions; inline_flag; attributes; merge }

let transform grammar =
  let start_symbols = StringSet.map rename grammar.start_symbols
  and tokens = StringMap.add eof properties grammar.tokens
  and rules =
    StringSet.fold (fun symbol rules ->
      StringMap.add (rename symbol) (new_start_rule symbol) rules
    ) grammar.start_symbols grammar.rules
  in
  { grammar with start_symbols; tokens; rules }
