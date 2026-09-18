(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let sprintf = Printf.sprintf
open Located
open Attribute
open PlainSyntax
open PlainSyntaxAccessors

(* When the original grammar is split over several files, it may be IMPOSSIBLE
   to print it out into a single file, as that would introduce a total ordering
   (between rules, between priority declarations, between %on_error_reduce
   declarations) that did not exist originally. We currently do not warn about
   this problem. Nobody has ever complained about it. We use the function
   [InputFile.compare], whose use is discouraged, in order to obtain a total
   order on precedence levels, and we obey this order when printing. *)

type mode =
  | PrintNormal
  | PrintForOCamlyacc
  | PrintUnitActions of bool

module Print (X : sig
  val mode : mode
  val channel : out_channel
  val grammar : grammar
end) = struct
open X

let emit format =
  Printf.fprintf channel format

(* -------------------------------------------------------------------------- *)

(* Printing an OCaml type. *)

let print_ocamltype ty : string =
  sprintf " <%s>" @@
    match ty with
    | Declared fragment ->
        value fragment
    | Inferred t ->
        t

let print_ocamltype ty : string =
  let s = print_ocamltype ty in
  match mode with
  | PrintForOCamlyacc ->
      (* ocamlyacc does not allow a %type declaration to contain
         a new line. Replace it with a space. *)
      String.map (function '\r' | '\n' -> ' ' | c -> c) s
  | PrintNormal
  | PrintUnitActions _ ->
      s

(* -------------------------------------------------------------------------- *)

(* Printing the type of a terminal symbol. *)

let print_token_type (prop : properties) =
  match prop.tk_ocamltype, mode with
  | None   , _
  | Some  _, PrintUnitActions true ->
      (* When %token is not followed by a type,
         this means that the type is [unit]. *)
      ""
  | Some ty, PrintNormal
  | Some ty, PrintForOCamlyacc
  | Some ty, PrintUnitActions false ->
      print_ocamltype ty

(* -------------------------------------------------------------------------- *)

(* Printing the type of a nonterminal symbol. *)

let print_nonterminal_type ty =
  match mode with
  | PrintNormal
  | PrintForOCamlyacc ->
      print_ocamltype ty
  | PrintUnitActions _ ->
      " <unit>"

(* -------------------------------------------------------------------------- *)

(* Printing a binding for a semantic value. *)

let print_binding id =
  match mode with
  | PrintNormal ->
      id ^ " = "
  | PrintForOCamlyacc
  | PrintUnitActions _ ->
      (* need not, or must not, bind a semantic value *)
      ""

(* -------------------------------------------------------------------------- *)

(* Testing whether it is permitted to print OCaml code
   (semantic actions, prelude, postlude). *)

let if_ocaml_code_permitted f =
  match mode with
  | PrintNormal
  | PrintForOCamlyacc ->
      f()
  | PrintUnitActions _ ->
      (* In these modes, all OCaml code is omitted: semantic actions,
         preludes, postludes, etc. *)
      ()

(* -------------------------------------------------------------------------- *)

(* Testing whether attributes should be printed. *)

let attributes_printed : bool =
  match mode with
  | PrintNormal
  | PrintUnitActions _ ->
      true
  | PrintForOCamlyacc ->
      false

(* -------------------------------------------------------------------------- *)

(* Printing a semantic action. *)

let print_semantic_action branch =
  let e = Action.expr branch.action in
  match mode with
  | PrintUnitActions _ ->
      (* In the unit-action modes, we print a pair of empty braces. *)
      ()
  | PrintNormal ->
      ILPrinter.print_expr channel e
  | PrintForOCamlyacc ->
       (* In ocamlyacc-compatibility mode, the code must be wrapped in
          [let]-bindings whose right-hand side uses the [$i] keywords. *)
      let bindings =
        List.mapi (fun i producer ->
          let id = prod_id producer
          and symbol = producer.prod_symbol in
          (* Test if [symbol] is a terminal symbol whose type is [unit]. *)
          let is_unit_token =
            is_terminal grammar symbol &&
            ocamltype_of_token grammar symbol = None
          in
          (* Define the variable [id] as a synonym for [$(i+1)]. *)
          (* As an exception to this rule, if [symbol] is a terminal symbol
             which has been declared *not* to carry a semantic value, then
             we cannot use [$(i+1)] -- ocamlyacc does not allow it -- so we
             use the unit value instead. *)
          IL.PVar id,
          if is_unit_token then
            IL.EUnit
          else
            IL.EVar (sprintf "$%d" (i + 1))
        ) branch.producers
      in
      (* The identifiers that we bind are pairwise distinct. *)
      (* We must use simultaneous bindings (that is, a [let/and] form), as
          opposed to a cascade of [let] bindings. Indeed, ocamlyacc internally
          translates [$i] to [_i] (just like us!), so name captures will occur
          unless we restrict the use of [$i] to the outermost scope. (Reported
          by Kenji Maillard.) *)
      let e = ILConstruction.eletand (bindings, e) in
      ILPrinter.print_expr channel e

(* -------------------------------------------------------------------------- *)

(* Printing preludes and postludes. *)

let print_preludes () =
  grammar.preludes |> List.iter @@ fun prelude ->
  emit "%%{%s%%}\n" (value prelude)

let print_postludes () =
  grammar.postludes |> List.iter @@ fun postlude ->
  emit "%s\n" (value postlude)

(* -------------------------------------------------------------------------- *)

(* Printing %start declarations. *)

let print_start_symbols () =
  grammar.start_symbols |> StringSet.iter @@ fun symbol ->
  emit "%%start %s\n" (MString.normalize symbol)

(* -------------------------------------------------------------------------- *)

(* Printing %parameter declarations. *)

let print_parameter parameter =
  emit "%%parameter<%s>\n" (value parameter)

let print_parameters () =
  match mode with
  | PrintNormal ->
      List.iter print_parameter grammar.parameters
  | PrintForOCamlyacc
  | PrintUnitActions _ ->
       (* %parameter declarations are not supported by ocamlyacc,
          and presumably become useless when the semantic actions
          are removed. *)
      ()

(* -------------------------------------------------------------------------- *)

(* Printing attributes. *)

let print_attribute { key; payload; origin } =
  ignore origin;
  if attributes_printed then
    emit " [@%s %s]" key payload

let print_attributes attrs =
  List.iter print_attribute attrs

(* -------------------------------------------------------------------------- *)

(* Printing token declarations and precedence declarations. *)

let print_assoc = function
  | LeftAssoc ->
      sprintf "%%left"
  | RightAssoc ->
      sprintf "%%right"
  | NonAssoc ->
      sprintf "%%nonassoc"
  | UndefinedAssoc ->
      ""

let compare_pairs compare1 compare2 (x1, x2) (y1, y2) =
  let c = compare1 x1 y1 in
  if c <> 0 then c
  else compare2 x2 y2

let compare_tokens (_token, prop) (_token', prop') =
  match prop.tk_precedence, prop'.tk_precedence with
  | UndefinedPrecedence, UndefinedPrecedence ->
      0
  | UndefinedPrecedence, PrecedenceLevel _ ->
      -1
  | PrecedenceLevel _, UndefinedPrecedence ->
      1
  | PrecedenceLevel mv, PrecedenceLevel mv' ->
      compare_pairs InputFile.compare Int.compare (value mv) (value mv')

let print_token_declarations () =
  grammar.tokens |> StringMap.iter @@ fun token prop ->
  if prop.tk_is_declared then begin
    emit "%%token%s %s" (print_token_type prop) token;
    print_attributes prop.tk_attributes;
    emit "\n"
  end

let print_precedence_declarations () =
  (* Sort the tokens wrt. precedence, and group them into levels. *)
  let levels = MList.group compare_tokens (StringMap.bindings grammar.tokens) in
  (* Print the precedence declarations: %left, %right, %nonassoc. *)
  levels |> List.iter @@ fun (level : (string * properties) list) ->
  let (_token, prop) = List.hd level in
  (* Do nothing about the tokens that have no precedence. *)
  if prop.tk_precedence <> UndefinedPrecedence then begin
    emit "%s" (print_assoc prop.tk_associativity);
    List.iter (fun (token, _prop) ->
      emit " %s" token
    ) level;
    emit "\n"
  end

let print_tokens f =
  print_token_declarations f;
  print_precedence_declarations f

(* -------------------------------------------------------------------------- *)

(* Printing %type declarations. *)

let print_types () =
  grammar.types |> StringMap.iter @@ fun symbol ty ->
  emit "%%type%s %s\n"
    (print_nonterminal_type ty)
    (MString.normalize symbol)

(* -------------------------------------------------------------------------- *)

(* Printing branches and rules. *)

let print_producer sep producer =
  emit "%s%s%s"
    (sep())
    (print_binding (prod_id producer))
    (MString.normalize producer.prod_symbol);
  print_attributes producer.prod_attributes

let print_branch branch =
  (* Print the producers. *)
  let sep = MString.this_then_that "" " " in
  List.iter (print_producer sep) branch.producers;
  (* Print the %prec annotation, if there is one. *)
  Option.iter (fun x ->
    emit " %%prec %s" (value x)
  ) branch.prec_annotation;
  (* Newline, indentation, semantic action. *)
  emit "\n    {";
  print_semantic_action branch;
  emit "}";
  (* The branch attributes follow the semantic action. *)
  print_attributes branch.br_attributes;
  emit "\n"

(* Because the resolution of reduce/reduce conflicts is implicitly dictated by
   the order in which productions appear in the grammar, the printer should be
   careful to preserve this order. *)

(* 2016/08/25: As noted above, when two productions originate in different files,
   we have a problem. We MUST print them in some order, even though they should
   be incomparable. In that case, we use the order in which the source files are
   specified on the command line. However, this behavior is undocumented, and
   should not be exploited. (In previous versions of Menhir, the function passed
   to [List.sort] was not transitive, so it did not make any sense!) *)

let compare_production_levels bpl bpl' =
  match bpl, bpl' with
  | ProductionLevel (m, l), ProductionLevel (m', l') ->
      compare_pairs InputFile.compare Int.compare (m, l) (m', l')

let compare_branches (b : branch) (b' : branch) =
  compare_production_levels b.production_level b'.production_level

let compare_rules (_nt, (r : rule)) (_nt', (r' : rule)) =
  match r.branches, r'.branches with
  | [], [] ->
      0
  | [], _ ->
      -1
  | _, [] ->
      1
  | b :: _, b' :: _ ->
      (* To compare two rules, it suffices to compare their first productions. *)
      compare_branches b b'

let print_branches branches =
  (* Menhir accepts a leading "|", but bison does not. Let's not print it.
     So, we print a bar-separated list. *)
  let sep = MString.this_then_that ("  ") ("| ") in
  branches |> List.iter @@ fun branch ->
  emit "%s" (sep());
  print_branch branch

let print_merge_fun omf =
  omf |> Option.iter @@ fun mf ->
  match mode with
  | PrintNormal ->
      emit "%%merge ";
      ILPrinter.print_expr channel (Action.expr mf);
      emit "\n"
  | PrintForOCamlyacc ->
      ()
  | PrintUnitActions _ ->
      emit "%%merge ";
      emit "{ fun _ _ -> () }";
      emit "\n"

let print_rule (nt, r) =
  emit "\n%s" (MString.normalize nt);
  print_attributes r.attributes;
  emit ":\n";
  print_branches r.branches;
  print_merge_fun r.merge

let print_rules () =
  let rules = List.sort compare_rules (StringMap.bindings grammar.rules) in
  List.iter print_rule rules

(* -------------------------------------------------------------------------- *)

(* Printing %on_error_reduce declarations. *)

let print_on_error_reduce_declarations () =
  let cmp (_nt, oel) (_nt', oel') = compare_production_levels oel oel' in
  MList.group cmp (StringMap.bindings grammar.on_error_reduce)
  |> List.iter @@ fun level ->
  emit "%%on_error_reduce";
  List.iter (fun (nt, _level) ->
    emit " %s" (MString.normalize nt)
  ) level;
  emit "\n"

let print_on_error_reduce_declarations f =
  match mode with
  | PrintNormal
  | PrintUnitActions _ ->
      print_on_error_reduce_declarations f
  | PrintForOCamlyacc ->
      (* %on_error_reduce declarations are not supported by ocamlyacc *)
      ()

(* -------------------------------------------------------------------------- *)

(* Printing %attribute declarations. *)

let print_grammar_attribute { key; payload; origin } =
  ignore origin;
  if attributes_printed then
    emit "%%[@%s %s]\n" key payload

let print_grammar_attributes () =
  List.iter print_grammar_attribute grammar.gr_attributes

(* -------------------------------------------------------------------------- *)

(* The main entry point. *)

let print f =
  print_parameters f;
  if_ocaml_code_permitted (fun () -> print_preludes f);
  print_start_symbols f;
  print_tokens f;
  print_types f;
  print_on_error_reduce_declarations f;
  print_grammar_attributes f;
  emit "%%%%\n";
  print_rules f;
  emit "\n%%%%\n";
  if_ocaml_code_permitted (fun () -> print_postludes f)

end (* Print *)

let print mode channel grammar =
  let module P = Print(struct
    let mode = mode
    let channel = channel
    let grammar = grammar
  end) in
  P.print()
