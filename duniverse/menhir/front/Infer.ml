(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf
open PlainSyntax
open PlainSyntaxAccessors
open IL
open ILConstruction
open FrontAPI

module Make
(G : sig val grammar : grammar end)
(X : sig
  include API_SETTINGS
  include BASE_SETTINGS
  include COMMENT_SETTINGS
  include EXN_SETTINGS
  include INFER_SETTINGS
  include TOKEN_TYPE_SETTINGS
end)
= struct
open G

module TokenType =
  TokenType.Make(X)
module Interface =
  Interface.Make(X)

(* -------------------------------------------------------------------------- *)

(* Naming conventions. *)

(* The type variable associated with a nonterminal symbol. Its name begins
   with a prefix which ensures that it begins with a lowercase letter and
   cannot clash with OCaml keywords. *)

let ntvar symbol =
  TypVar (sprintf "tv_%s" (MString.normalize symbol))

(* The term variable associated with a nonterminal symbol. Its name begins
   with a prefix which ensures that it begins with a lowercase letter and
   cannot clash with OCaml keywords. *)

let encode symbol =
  sprintf "xv_%s" (MString.normalize symbol)

let decode s =
  let n = String.length s in
  if not (n >= 3 && String.sub s 0 3 = "xv_") then
    Lexmli.fail();
  String.sub s 3 (n - 3)

(* -------------------------------------------------------------------------- *)

(* The type of a nonterminal symbol. *)

(* 2020/05/25. We must be careful how we process the type information given to
   us by the user via [%type] and [%start] annotations. Duplicating this
   information without care can be unsound: e.g., some OCaml types, such as
   the open polymorphic variant type [> `A ], contain implicit type variables.
   When such a type annotation is duplicated, the two copies are unrelated:
   this can cause us to infer a type that is too general (issue #37).

   To avoid this issue, part of the solution is to enforce sharing by using a
   type variable everywhere (one per nonterminal symbol).

   At first sight, it looks as though we could use this type variable
   everywhere, and use the user-provided type annotation exactly once.
   However, another OCaml feature, type-directed disambiguation, depends on
   explicit type information, and does not always make a correct choice if
   this information is not immediately available (prior to unification).

   Thus, whenever we need to annotate an expression or a pattern with the type
   of a nonterminal symbol, we must annotate it *both* with the type variable
   that corresponds to this symbol (so as to enforce sharing) and with the
   user-provided type, if there is one (so as to allow type-directed
   disambiguation).

   Also, when reading the types inferred by the OCaml type-checker, we must
   read every type, even in the cases where a type was already provided by
   the user. Indeed, the type inferred by OCaml may be more specific than the
   type provided by the user. E.g., the user may declare the type [> `A ],
   while OCaml may infer the type [> `A | `B ]. *)

(* [annotate_expr nt e] annotates the expression [e] with the type information
   associated with the nonterminal symbol [nt]. The expression is annotated
   possibly twice: once with the type variable [ntvar nt] and once with the
   type that the user has possibly provided via a %type declaration. *)

let opt_annotate_expr (oty : ocamltype option) (e : expr) : expr =
  MOption.fold (fun ty e -> annotate (TypTextual ty) e) oty e

let annotate_expr (nt : nonterminal) (e : expr) : expr =
  let e = annotate (ntvar nt) e in
  let e = opt_annotate_expr (ocamltype_of_symbol grammar nt) e in
  e

(* [annotate_merge_fun nt e] annotates the expression [e] with the type
   of a merge function for the nonterminal symbol [nt]. As above, the
   expression is annotated possibly twice. *)

let annotate_merge_fun nt e =
  let merge ty = marrow [ ty; ty ] ty in
  let e = annotate (merge (ntvar nt)) e in
  let oty = ocamltype_of_symbol grammar nt in
  let return ty = marrow [ TypTextual ty; TypTextual ty ] (TypTextual ty) in
  let e = MOption.fold (fun ty e -> annotate (return ty) e) oty e in
  e

(* [annotate_pat p nt] annotates the pattern [p] with the type information
   associated with the nonterminal symbol [nt]. *)

let opt_annotate_pat (p : pattern) (oty : ocamltype option) : pattern =
  MOption.fold (fun ty p -> PAnnot (p, TypTextual ty)) oty p

let annotate_pat (p : pattern) (nt : nonterminal) : pattern =
  let p = PAnnot (p, ntvar nt) in
  let p = opt_annotate_pat p (ocamltype_of_symbol grammar nt) in
  p

(* [test_token_type symbol] returns the type of the terminal symbol
   [symbol], if [symbol] is indeed a terminal symbol. Otherwise, it
   raises [Not_found]. *)

let test_token_type (symbol : symbol) : typ =
  let props = StringMap.find symbol grammar.tokens in
  match props.tk_ocamltype with
  | None ->
      tunit
  | Some ocamltype ->
      TypTextual ocamltype

(* [annotate_pat p symbol] annotates the pattern [p] with the type information
   associated with the symbol [symbol], which may be a terminal or nonterminal
   symbol. *)

let annotate_pat (p : pattern) (symbol : symbol) : pattern =
  try
    (* If this succeeds then [symbol] is a terminal symbol. *)
    PAnnot (p, test_token_type symbol)
  with Not_found ->
    (* If we reach this point then [symbol] is a nonterminal symbol. *)
    annotate_pat p symbol

(* -------------------------------------------------------------------------- *)

(* Code production for semantic actions. *)

(* [action_rhs_formals branch] constructs a list of formal parameters of the
   semantic action of the production [branch]. This list contains only the
   formal parameters that depend on the production's right-hand side. *)

(* The names and types of the conventional internal variables that correspond
   to keywords ($startpos,etc.) are hardwired in this code. It would be nice
   if these conventions were more clearly isolated and perhaps moved to the
   [Action] or [Keyword] module. *)

(* We use [pvarlocated] rather than [PVar] so that a binding is located in the
   source file (not in the generated file) by the OCaml compiler. This is
   important when a variable declared by the user turns out to be unused in a
   semantic action. We want the unused-variable warning (or error) to be
   correctly located. *)

let action_rhs_formals branch : pattern list =
  List.fold_left (fun formals producer ->
    let symbol = producer.prod_symbol
    and lid = producer.prod_id in
    let id = Located.value lid in
    let startp, endp, starofs, endofs, loc =
      sprintf "_startpos_%s_" id,
      sprintf "_endpos_%s_" id,
      sprintf "_startofs_%s_" id,
      sprintf "_endofs_%s_" id,
      sprintf "_loc_%s_" id
    in
    annotate_pat (pvarlocated lid) symbol ::
    PAnnot (PVar startp, tposition) ::
    PAnnot (PVar endp, tposition) ::
    PAnnot (PVar starofs, tint) ::
    PAnnot (PVar endofs, tint) ::
    PAnnot (PVar loc, tlocation) ::
    formals
  ) [] branch.producers

(* [action_fixed_formals formals] extends the list [formals] with the formal
   parameters that do not depend on the right-hand side of the production. *)

let action_fixed_formals formals : pattern list =
  PAnnot (PVar "_startpos", tposition) ::
  PAnnot (PVar "_endpos", tposition) ::
  PAnnot (PVar "_endpos__0_", tposition) ::
  PAnnot (PVar "_symbolstartpos", tposition) ::
  PAnnot (PVar "_startofs", tint) ::
  PAnnot (PVar "_endofs", tint) ::
  PAnnot (PVar "_endofs__0_", tint) ::
  PAnnot (PVar "_symbolstartofs", tint) ::
  PAnnot (PVar "_sloc", tlocation) ::
  PAnnot (PVar "_loc", tlocation) ::
  formals

(* [action_formals branch] is the list of all formal parameters of the
   semantic action in the production [branch]. *)

let action_formals branch : pattern list =
  action_fixed_formals (action_rhs_formals branch)

(* [action_def nt branch] turns a branch into an OCaml function definition. *)

let action_def nt branch : expr =
  efun (action_formals branch) @@
  annotate_expr nt (Action.expr branch.action)

(* -------------------------------------------------------------------------- *)

(* Code production for merge functions. *)

(* [merge_def] turns a merge function into an OCaml function definition. *)

let merge_formals : pattern list =
  PAnnot (PVar "_startpos", tposition) ::
  PAnnot (PVar "_endpos", tposition) ::
  []

let merge_def nt (mf : merge_fun) : expr =
  efun merge_formals @@
  annotate_merge_fun nt (Action.expr mf)

(* [default_merge_def] turns a default merge function into an OCaml function
   definition. *)

(* A default merge function receives a string (the name of a nonterminal
   symbol) and two values of unknown type and must return a value of the
   same type. In other words, it must be polymorphic. *)

let default_merge_def (mf : merge_fun) : expr =
  efun merge_formals @@
  let semv = "opaque_semantic_value" in
  let sigma =
    local_scheme [semv] @@
    marrow [ tstring; tname semv; tname semv ] (tname semv)
  in
  poly sigma @@
  Action.expr mf

(* -------------------------------------------------------------------------- *)

(* Code production for the entire grammar. *)

(* [basic_bindings()] turns the productions into a list of OCaml function
   definitions. Each function is nameless. *)

(* The productions that come from Menhir's standard library are placed first,
   so that type errors are not reported in them. A notion of priority is used
   for this purpose. *)

let basic_bindings () =
  let cmp (prio1, _) (prio2, _) = Int.compare prio1 prio2 in
  StringMap.fold (fun nt rule accu ->
    List.fold_left (fun bindings branch ->
      let binding = (PWildcard, action_def nt branch)
      and priority = Action.priority branch.action in
      (priority, binding) :: bindings
    ) accu rule.branches
  ) grammar.rules []
  |> List.sort cmp
  |> List.map snd

(* [merge_bindings bindings] turns the merge functions into more bindings
   and adds them to the accumulator [bindings]. *)

let merge_bindings bindings =
  StringMap.fold (fun nt rule bindings ->
    match rule.merge with
    | None ->
        bindings
    | Some mf ->
        (PWildcard, merge_def nt mf) :: bindings
  ) grammar.rules bindings

(* [default_merge_binding bindings] turns the default merge function into zero
   or one more bindings and adds them to the accumulator [bindings]. *)

let default_merge_binding bindings =
  match grammar.default_merge with
  | None ->
      bindings
  | Some mfl ->
      let mf = Located.value mfl in
      (PWildcard, default_merge_def mf) :: bindings

(* [program()] turns the grammar into an OCaml program. *)

let program () =

  (* Create entry points (toplevel variables) whose types are the unknowns
     that we are looking for, that is, the type variables [ntvar nt].
     Constrain these variables via the OCaml code in [bindings()]. *)

  let (pats : pattern list), (tys : typ list) =
    StringMap.fold (fun nt _ (pats, tys) ->
      PVar (encode (MString.normalize nt)) :: pats,
      ntvar nt :: tys
    ) grammar.rules ([], [])
  in

  let main_def =
    pdef (PTuple pats) @@
    let bindings =
      default_merge_binding @@
      merge_bindings @@
      basic_bindings ()
    in
    ELet (bindings, annotate (TypTuple tys) EBottom)
  in

  (* Insert markers to delimit the part of the file that we are interested in.
     These markers are recognized by [Lexmli]. This lets us skip the values,
     types, exceptions, etc., that might be defined by the user's prologue or
     postlogue. *)

  let begin_def = def "menhir_begin_marker" (EIntConst 0)
  and end_def = def "menhir_end_marker" (EIntConst 0) in

  (* Issue the test program. We include the definition of the type of tokens,
     because, in principle, the semantic actions may refer to it or to its
     data constructors. *)

  [ SIFunctor (grammar.parameters,
    interface_to_structure (TokenType.tokentypedef grammar) @
    SIFragment grammar.preludes ::
    SIValDefs (false, [ begin_def; main_def; end_def ]) ::
    SIFragment grammar.postludes ::
  [])]

(* -------------------------------------------------------------------------- *)

(* [write_ml filename ()] writes the program [program()]
   to the file [filename]. *)

let write_ml filename () =
  let f = open_out filename in
  let module P = ILPrinter.Make(struct
    let f = f
    let print_line_directives = Some filename
    let comment = false
  end) in
  P.program (program());
  close_out f

(* [write_mli filename ()] writes the parser's interface
   to the file [filename]. *)

let write_mli filename () =
  Interface.write grammar filename ()

(* -------------------------------------------------------------------------- *)

(* [depend mode] creates an [.ml] file and an [.mli] file, then invokes
   [ocamldep] to compute dependencies for us. *)

(* If an old [.ml] or [.mli] file exists, we are careful to preserve it. We
   temporarily move it out of the way and restore it when we are done. There
   is no reason why dependency analysis should destroy existing files. *)

type entry =
  string (* basename *) * string (* filename *)

type line =
  entry (* target *) * entry list (* dependencies *)

let depend mode =

  let mlname, mliname = X.base ^ ".ml", X.base ^ ".mli" in

  let ocamldep_command =
    sprintf "%s %s %s"
      X.ocamldep (Filename.quote mlname) (Filename.quote mliname)
  in

  let output : string =
    IO.moving_away mlname @@ fun () ->
    IO.moving_away mliname @@ fun () ->
    IO.with_file mlname (write_ml mlname) @@ fun () ->
    IO.with_file mliname (write_mli mliname) @@ fun () ->
    IO.invoke_or_die ocamldep_command
  in

  (* Echo ocamldep's output. *)

  print_string output;

  (* If [--raw-depend] was specified on the command line, stop here. This
     option is used by [omake] and by [ocamlbuild], which perform their own
     postprocessing of [ocamldep]'s output. For users of [make], who use
     [--depend], some postprocessing is required, which is performed below. *)

  match mode with
  | `RawDepend -> ()
  | `Depend ->

    (* Make sense out of ocamldep's output. *)

    let lexbuf = Lexing.from_string output in
    let lines : line list =
      try
        Lexdep.main lexbuf
      with Lexdep.Error msg ->
        (* Echo the error message, followed with ocamldep's output. *)
        Report.Just.error [] "%s" (msg ^ output)
    in

    (* Look for the line that concerns the [.cmo] target, and echo a modified
       version of this line, where the [.cmo] target is replaced with [.ml]
       and [.mli] targets, and where the dependency over the [.cmi] file is
       dropped.

       In doing so, we assume that the user's [Makefile] supports bytecode
       compilation, so that it makes sense to request [bar.cmo] to be built,
       as opposed to [bar.cmx]. This is not optimal, but will do. [ocamldep]
       exhibits the same behavior. *)

    lines |> List.iter @@ fun ((_, target), dependencies) ->
    if Filename.check_suffix target ".cmo" then
      let dependencies =
        List.filter (fun (basename, _) -> basename <> X.base) dependencies
      in
      if List.length dependencies > 0 then begin
        printf "%s %s:" mlname mliname;
        List.iter (fun (_basename, filename) ->
          printf " %s" filename
        ) dependencies;
        printf "\n%!"
      end

(* -------------------------------------------------------------------------- *)

(* Augmenting a grammar with inferred type information. *)

(* The parameter [output] contains the output of [ocamlc -i]. *)

(* [build output] returns a map of (normalized) nonterminal symbols
   to OCaml types. *)

type env =
  ocamltype StringMap.t

let extract (output : string) (id, startofs, endofs) : nonterminal * ocamltype =
  let ty = String.sub output startofs (endofs - startofs) in
  decode id, Inferred ty

let build (output : string) : env =
  try
    Lexmli.main (Lexing.from_string output)
    |> List.map (extract output)
    |> StringMap.of_list
  with Lexmli.Failure ->
    Report.Just.error [] "failed to make sense of ocamlc's output."

(* [lookup env nt] looks up the nonterminal symbol [nt] in the table [env]
   and fails if it is absent. *)

let lookup (env : env) (nt : nonterminal) : ocamltype =
  try
    StringMap.find (MString.normalize nt) env
  with Not_found ->
    (* No type information has been inferred for this symbol. This is
       strange. Fail gracefully. *)
    Report.Just.error [] "found no inferred type for %s." nt

(* [augment env] augments the %type declaration table [grammar.types] with the
   information contained in the table [env]. Every type declared type in the
   table [grammar.types] is overridden with the inferred type obtained by
   looking up [env]. *)

let augment (env : env) : env =
  StringMap.fold (fun nt _ types ->
    StringMap.add nt (lookup env nt) types
  ) grammar.rules grammar.types

(* [read_reply] returns a new grammar, which is augmented with new %type
   declarations. *)

let read_reply (output : string) : grammar =
  let env = build output in
  let types = augment env in
  { grammar with types }

(* -------------------------------------------------------------------------- *)

(* Inferring types for a grammar's nonterminal symbols. *)

(* We invoke [ocamlc] to perform type inference; then we call [read_reply] to
   make sense out of its output. *)

let infer () =
  let mlname = X.base ^ ".ml" in
  let ocamlc_command =
    sprintf "%s -c -i %s" X.ocamlc (Filename.quote mlname)
  in
  write_ml mlname ();
  let output = IO.invoke_or_die ocamlc_command in
  (* 2015/10/05: intentionally do not remove the [.ml] file if [ocamlc]
       fails. (Or if an exception is thrown.) *)
  Sys.remove mlname;
  read_reply output

(* -------------------------------------------------------------------------- *)

(* [write_query] is just [write_ml]. *)

let write_query filename =
  write_ml filename ()

(* -------------------------------------------------------------------------- *)

(* [read_reply] reads the reply from the file [filename], which contains the
   output of [ocamlc -i]. *)

let read_reply filename =
  read_reply (IO.read_whole_file filename)

end (* Make *)
