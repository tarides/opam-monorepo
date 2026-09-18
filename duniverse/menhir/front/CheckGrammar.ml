(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let signal, warning = Report.(signal, warning)
open Attribute
open Located
open Syntax
open FrontAPI

module Make (E : sig
  open Report

  (**A channel for reporting warnings and errors. *)
  val main: channel

  (**[can_complain_about symbol] determines whether it is permitted to report
     an error, using [signal], about the symbol [symbol]. This function
     typically returns [true] if queried about a certain symbol for this time,
     and returns [false] if queried again about the same symbol. This lets us
     avoid flooding the user with multiple reports about a single symbol. *)
  val can_complain_about : symbol -> bool

end)
(X : CHECK_GRAMMAR_SETTINGS)
= struct
open E

let reserved =
  [ "error" ]

(* -------------------------------------------------------------------------- *)

(* This local module helps share parameters and mutable state. *)

module Run (G : sig val grammar : grammar end) = struct
open G

(* -------------------------------------------------------------------------- *)

(* As of 2025/10/15, we systematically use [signal] instead of [error].
   This lets us report more than one error. *)

let undefined pos symbol =
  if can_complain_about symbol then
    signal main [pos] "The symbol %s is undefined." symbol

(* -------------------------------------------------------------------------- *)

(* Check that every start symbol has a definition as a nonterminal symbol
   and has a %type declaration. *)

let is_named nt (param, _ocamltype) =
  Parameter.equal (Parameter.var nt) param

let () =
  grammar.p_start_symbols |> StringMap.iter @@ fun nt pos ->
  if not (StringMap.mem nt grammar.p_rules) then
    undefined pos nt
  else if not (List.exists (is_named nt) grammar.p_types) then
    signal main [pos] "the type of the start symbol %s is unspecified." nt

(* -------------------------------------------------------------------------- *)

(* [check_decl kind must_be_nonterminal param] checks that every symbol that
   occurs within the parameter [param] is well-defined. *)

(* The parameter [kind] is the kind of declaration that is being inspected:
   it can be %type, %on_error_reduce, or %attribute. *)

(* The parameter [must_be_nonterminal] determines whether the root symbol
   must be a nonterminal symbol. It is reset to [false] in the recursive
   calls: e.g. in [X(Y,Z)], only [X] must be a nonterminal symbol. *)

let rec check_decl (kind : string) (must_be_nonterminal : bool) param =
  (* Destructure the head and arguments. *)
  let head, params = Parameter.destruct param in
  let head, pos = value head, position head in
  (* Check if [head] is a nonterminal or terminal symbol. *)
  let is_nonterminal =
    StringMap.mem head grammar.p_rules
  and is_terminal =
    StringMap.mem head grammar.p_tokens ||
    List.mem head reserved
  in
  (* If [head] is not satisfactory, fail. *)
  if not (is_terminal || is_nonterminal) then
    undefined pos head
  else if (must_be_nonterminal && not is_nonterminal) then
    signal main [pos]
      "%s is a terminal symbol,\n\
       but %s declarations are applicable only to nonterminal symbols."
      (Parameter.print ", " param) kind
  (* Then, check the children. *)
  else
    List.iter (check_decl kind false) params

(* A variant. *)

let check_decl' kind must_be_nonterminal (param, _) =
  check_decl kind must_be_nonterminal param

(* Special cases. *)

let check_type_decl param =
  check_decl' "%type" true param

let check_on_error_reduce_decl param =
  check_decl' "%on_error_reduce" true param

let check_attribute_decl param =
  check_decl "%attribute" false param

let check_attribute_decls (params, _) =
  List.iter check_attribute_decl params

(* Check that every %type, %on_error_reduce or %attribute declaration
   refers to well-defined symbols and has, at its head, a nonterminal
   symbol. *)

let () =
  List.iter check_type_decl            grammar.p_types;
  List.iter check_on_error_reduce_decl grammar.p_on_error_reduce;
  List.iter check_attribute_decls      grammar.p_symbol_attributes

(* -------------------------------------------------------------------------- *)

(* We record which tokens (terminal symbols) are used. *)

let used =
  ref StringSet.empty

let mark (t : terminal) =
  used := StringSet.add t !used

(* -------------------------------------------------------------------------- *)

(* [check_symbol rule prec symbol] resolves an occurrence of the symbol
   [symbol] and performs several checks about it. *)

(* The parameter [rule] is the rule where this symbol appears. *)

(* The parameter [prec] indicates whether this occurrence of [symbol] is
   a %prec annotation. *)

let check_symbol rule prec (symbol : symbol located) =
  let symbol, pos = value symbol, position symbol in
  if not prec && List.mem symbol rule.pr_parameters then
    (* A parameter of this rule. *)
    ()
  else if not prec && List.mem symbol reserved then
    (* A reserved token. Mark it as used. *)
    mark symbol
  else if not prec && StringMap.mem symbol grammar.p_rules then
    (* A nonterminal symbol. *)
    ()
  else
    match StringMap.find symbol grammar.p_tokens with
    | properties ->
        (* A token or pseudo-token. Mark it as used. *)
        mark symbol;
        if not prec && not properties.tk_is_declared
        && can_complain_about symbol then
          (* A pseudo-token, declared by %left, %right or %nonassoc, cannot
             be used as a normal identifier. It can appear only in a %prec
             annotation. *)
          signal main [ pos ]
            "The symbol %s has not been declared by %%token,\n\
             so cannot be used here." symbol
    | exception Not_found ->
        (* An unknown symbol. *)
        undefined pos symbol

(* -------------------------------------------------------------------------- *)

(* Check that every rule, branch, and producer is well-formed. *)

let check_producer rule (_id, param, _ : producer) =
  Parameter.iter (check_symbol rule false) param

let check_branch rule branch =
  let producers = branch.pb_producers in
  (* Check each producer. *)
  producers |> List.iter (check_producer rule);
  (* Check the branch's optional %prec annotation. *)
  branch.pb_prec_annotation |> Option.iter (check_symbol rule true);
  (* Check that no variable is bound twice, i.e.,
     no two producers define the same variable. *)
  let cmp (x1, _, _) (x2, _, _) = String.compare (value x1) (value x2) in
  MList.foreach_duplicate cmp producers @@ fun (x, _, _) ->
  signal main [branch.pb_position]
    "there are multiple producers named %s in this sequence."
    (value x)

let check_rule_has_distinct_formal_parameters rule =
  MList.foreach_duplicate String.compare rule.pr_parameters @@ fun x ->
    signal main rule.pr_positions
      "several parameters of this rule are named \"%s\"." x

let check_inline_rule_has_no_attribute rule =
  match rule.pr_inline, rule.pr_attributes with
  | true, attr :: _ ->
      signal main [attr.origin]
        "the nonterminal symbol %s is declared %%inline.\n\
         It cannot carry an attribute."
        rule.pr_nt
  | true, []
  | false, _ ->
      ()

let check_inline_rule_has_no_merge_fun rule =
  match rule.pr_inline, rule.pr_merge with
  | true, Some _ ->
      signal main rule.pr_positions
        "the nonterminal symbol %s is declared %%inline.\n\
         It cannot carry a merge function."
        rule.pr_nt
  | true, None
  | false, _ ->
      ()

let check_merge_fun_keywords mflo =
  let open Keyword in let open KeywordSet in
  mflo |> Option.iter @@ fun mfl ->
  let keywords = Action.keywords (value mfl) in
  let permitted = of_list [ startpos; endpos ] in
  let keywords = diff keywords permitted in
  if not (is_empty keywords) then
    let keyword = choose keywords in
    signal main [position mfl]
      "the keyword %s cannot be used in a merge function."
      (Keyword.print keyword)

let check_rule nt rule =
  (* The fact that the formal parameters of each rule have distinct names
     has already been checked in JoinGrammars. Nevertheless, it costs us
     only a few lines of code to repeat this check here. *)
  check_rule_has_distinct_formal_parameters rule;
  (* Check each branch. *)
  List.iter (check_branch rule) rule.pr_branches;
  (* It is forbidden to use %inline on a %start symbol. *)
  if (rule.pr_inline && StringMap.mem nt grammar.p_start_symbols) then
     signal main rule.pr_positions
       "%s cannot be both a start symbol and inlined." nt;
  (* If a rule is marked %inline, then it must not carry an attribute. *)
  check_inline_rule_has_no_attribute rule;
  (* If a rule is marked %inline, then it must not carry a %merge function. *)
  check_inline_rule_has_no_merge_fun rule;
  (* If there is a %merge function then it should mention no keywords. *)
  check_merge_fun_keywords rule.pr_merge

(* Check every rule. *)

let () =
  grammar.p_rules |> StringMap.iter check_rule

let () =
  check_merge_fun_keywords grammar.p_default_merge

(* -------------------------------------------------------------------------- *)

(* Emit a warning about each unused token. *)

(* These warnings can be disabled by the user-supplied function
   [X.ignore_unused_token]. *)

let () =
  grammar.p_tokens |> StringMap.iter @@ fun t properties ->
  if not (X.ignore_unused_token t || StringSet.mem t !used) then
    warning main [properties.tk_position]
      "the token %s is unused." t

(* -------------------------------------------------------------------------- *)

(* If zero tokens have been declared, reject the grammar. *)

let is_declared _t properties =
  properties.tk_is_declared

let () =
  let declared = StringMap.filter is_declared grammar.p_tokens in
  if StringMap.is_empty declared then
    signal main [] "no tokens have been declared."

(* 2021/11/23: If a token is named [Error], warn. This is undesirable,
   as it creates a collision in the public interface of the generated
   parser between the token [Error] and the exception [Error]. OCaml
   itself may warn about this collision. *)

let () =
  match StringMap.find "Error" grammar.p_tokens with
  | properties ->
      if properties.tk_is_declared then
        warning main [properties.tk_position]
          "please do not name a terminal symbol Error."
  | exception Not_found ->
      ()

(* Check that every token has an alias. (This is optional.) *)

let () =
  if X.require_aliases then
    grammar.p_tokens |> StringMap.iter @@ fun t properties ->
    if properties.tk_is_declared && properties.tk_alias = None then
      warning main [properties.tk_position]
        "no alias has been defined for the token %s." t

(* -------------------------------------------------------------------------- *)

(* Check that the [@name] attributes carried by productions are valid OCaml
   identifiers. This property is required by the unparsing API. We require
   this property even if [--unparsing] is off. *)

let valid_ocaml_identifier (x : string) : bool =
  Lex.valid_ocaml_identifier (Lexing.from_string x)

let check_attribute attr =
  if attr.key = "name" && not (valid_ocaml_identifier attr.payload) then
    signal main [attr.origin]
      "the name of a production must be a valid OCaml identifier"

let check_branch branch =
  List.iter check_attribute branch.pb_attributes

let check_rule _nt rule =
  List.iter check_branch rule.pr_branches

let () =
  StringMap.iter check_rule grammar.p_rules

(* -------------------------------------------------------------------------- *)

(* Check that no two %type declarations bear on the same nonterminal symbol.  *)

(* Apply the same check on %on_error_reduce declarations. *)

(* Before 2025/10/16, this check was performed only after the expansion of
   parameterized nonterminal symbols. *)

module H =
  Hashtbl.Make(Parameter)

let check_declarations (kind : string) (decls : (parameter * _) list) =
  let table = H.create 32 in
  decls |> List.iter @@ fun decl ->
  let param, _ = decl in
  match H.find table param with
  | exception Not_found ->
      H.add table param param
  | param' ->
      (* We cannot easily use [can_complain_about] because the thing
         at hand is a parameter, not a symbol. Never mind. *)
      let positions = Parameter.([position param; position param']) in
      signal main positions
        "there are multiple %s declarations for the symbol %s."
        kind (Parameter.print ", " param)

let () =
  check_declarations "%type" grammar.p_types;
  check_declarations "%on_error_reduce" grammar.p_on_error_reduce

(* -------------------------------------------------------------------------- *)

(* In order to guarantee that our current encoding of items works (see the
   module [Item]), we check that every production is reasonably short. *)

let low_bits =
  10 (* This must match the value in [Item]. *)

let low_limit =
  1 lsl low_bits

let () =
  grammar.p_rules |> StringMap.iter @@ fun _ rule ->
  rule.pr_branches |> List.iter @@ fun branch ->
  let n = List.length branch.pb_producers in
  if low_limit <= n then
    signal main
      [branch.pb_position]
      "The length of this production is %d, which exceeds the limit of %d."
      n (low_limit - 1)

(* -------------------------------------------------------------------------- *)

let finished =
  ()

end (* Run *)

let check grammar =
  let module R = Run(struct let grammar = grammar end) in
  R.finished

end (* Make *)
