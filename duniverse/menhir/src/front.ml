(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* The front-end. This module performs a series of toplevel side effects. *)

open Channels

(* ------------------------------------------------------------------------- *)

(* When the new OCaml type inference protocol is used, Menhir is called twice:
   first with [--infer-write-query], then with [--infer-read-reply]. This
   means that any warnings or information messages that are issued before
   OCaml type inference takes place are duplicated, unless we do something
   about it. To address this issue, when [--infer-read-reply] is set, we
   disable all output until the point where the inferred [.mli] file is read.
   Then, we enable output again, and continue. *)

let () =
  match Settings.infer with
  | Settings.IMReadReply _ ->
      Channels.disable()
  | _ ->
      ()

(* ------------------------------------------------------------------------- *)

(* Reading a grammar from a file. *)

let load_grammar_from_contents priority filename content =
  InputFile.with_file_content filename content @@ fun lexbuf ->
  let grammar = Driver.parse priority lexbuf in
  { grammar with pg_filename = filename }

let load_grammar_from_file filename : Syntax.partial_grammar =
  try
    let priority = 1
    and contents = IO.read_whole_file filename in
    load_grammar_from_contents priority filename contents
  with Sys_error msg ->
    Report.Just.error [] "%s" msg

(* ------------------------------------------------------------------------- *)

(* Read all of the grammar files that are named on the command line,
   plus the standard library, unless suppressed by [--no-stdlib] or
   [--rocq]. *)

let grammars () : Syntax.partial_grammar list =
  List.map load_grammar_from_file Settings.filenames

let grammars () : Syntax.partial_grammar list =
  if Settings.no_stdlib || Settings.backend = `RocqBackend then
    grammars()
  else
    (* As 20190924, the standard library is no longer actually read from a
       file. Instead, its text is built into the Menhir executable: it is
       found in the string [Standard_mly.contents]. We parse it just as if
       it had been read from a file, and pretend that the file name is
       "<standard.mly>". This file name can appear in generated
       parsers, because Menhir produces # directives that point back to
       source (.mly) files. *)
    let standard_library =
      let priority = 0 in
      load_grammar_from_contents
        priority
        "<standard.mly>"
        Standard_mly.contents
    in
    standard_library :: grammars()

let grammars =
  Time.time "Lexing and parsing" @@ fun () ->
  grammars()

(* ------------------------------------------------------------------------- *)

(* Eliminate anonymous rules. *)

let grammars : Syntax.partial_grammar list =
  Time.time "Eliminating anonymous rules" @@ fun () ->
  List.map EliminateAnonymousRules.transform grammars

(* Remove all uses of token aliases. *)

let grammars =
  monitor @@ fun c ->
  Time.time "Eliminating token aliases" @@ fun () ->
  ExpandTokenAliases.transform c grammars

(* ------------------------------------------------------------------------- *)

(* If several grammar files were provided on the command line, merge them.   *)

(* Then, check that the resulting grammar is well-formed. *)

(* Even if [JoinGrammars] signals some errors, we allow [CheckGrammar]
   to run. This lets us signal more errors and blur the distinction
   between [JoinGrammars] and [CheckGrammars] as far as the user is
   concerned. *)

let grammar : Syntax.grammar =
  monitor @@ fun c ->

  let module E = struct
    let main = c
    let info = c
    (* For each symbol [s], we report at most one error about this symbol. *)
    let can_complain_about : string -> bool =
      let reported = ref StringSet.empty in
      fun symbol ->
        not (StringSet.mem symbol !reported) &&
        let () = reported := StringSet.add symbol !reported in
        true
  end in

  let module J = JoinGrammars.Make(E) in
  let grammar =
    Time.time "Joining" @@ fun () ->
    J.join grammars
  in

  let module C = CheckGrammar.Make(E)(Settings) in
  let () =
    Time.time "Checking the grammar" @@ fun () ->
    C.check grammar
  in

  grammar

(* ------------------------------------------------------------------------- *)

(* Check that the grammar is well-sorted; infer the sort of every symbol. *)

(* At verbosity level 3, print the sort of every symbol. *)

let sorts =
  monitor @@ fun c ->
  Time.time "Inferring sorts" @@ fun () ->
  SortInference.infer c (getG 3) grammar

(* ------------------------------------------------------------------------- *)

(* This is the grammar before expansion. *)

let grammar_before_expansion =
  grammar

(* ------------------------------------------------------------------------- *)

(* Expand away all applications of parameterized nonterminal symbols,
   so as to obtain a grammar without parameterized nonterminal symbols. *)

let grammar : PlainSyntax.grammar =
  monitor @@ fun c ->
  Time.time "Expanding parameterized nonterminal symbols" @@ fun () ->
  let open SelectiveExpansion in
  (* First, perform a selective expansion: expand away all parameters of
     higher sort, keeping the parameters of sort [*]. This process always
     terminates. *)
  let grammar1 = expand ExpandHigherSort sorts grammar in
  (* This "first-order parameterized grammar" can then be submitted to
     the termination check. *)
  CheckSafeExpansion.check c grammar1;
  (* If it passes the check, then full expansion is safe. We drop [grammar1]
     and start over from [grammar]. This is required in order to get correct
     names. (Expanding [grammar1] would yield an equivalent grammar, with
     more complicated names, reflecting the two steps of expansion.) *)
  let grammar = expand ExpandAll sorts grammar in
  (* This yields an unparameterized grammar. *)
  Drop.drop grammar

(* ------------------------------------------------------------------------- *)

(* If [--strategy simplified] has been selected, ensure that the [error]
   token is used only at the end of productions (2021/10/31). *)

let grammar =
  match Settings.strategy with
  | `Simplified ->
      monitor @@ fun c ->
      Time.time "Checking where the error token is used" @@ fun () ->
      let reason = "when --strategy simplified is selected,\n" in
      CheckErrorTokenUsage.final_only c reason grammar
  | `Legacy ->
      grammar

(* If [--test-GLR] or [--GLR] has been selected, remove the productions that
   use the [error] token. If just [--GLR] has been selected, let the user know
   that we are doing this, by emitting warnings. If both [--GLR] and
   [--test-GLR] have been selected, do not emit warnings. *)

let grammar =
  if Settings.enabled_GLR then
    if Settings.test_GLR then
      Time.time "Checking that the error token is not used" @@ fun () ->
      let reason = "because --test-GLR is selected,\n" in
      CheckErrorTokenUsage.nowhere Report.null reason grammar
    else
      monitor @@ fun c ->
      Time.time "Checking that the error token is not used" @@ fun () ->
      let reason = "when --GLR is selected,\n" in
      CheckErrorTokenUsage.nowhere c reason grammar
  else
    grammar

(* ------------------------------------------------------------------------- *)

(* If [--kill-priorities] has been passed, kill all priority declarations. *)

(* If [--avoid-eos] has been passed, artificially introduce a new [EOF]
   terminal symbol so as to guarantee the absence of end-of-stream
   conflicts. *)

(* These two features can be useful when testing the GLR parsing algorithm. *)

let grammar =
  if Settings.kill_priorities then
    KillPriorityDeclarations.kill grammar
  else
    grammar

let grammar =
  if Settings.avoid_eos then
    AvoidEOS.transform grammar
  else
    grammar

(* ------------------------------------------------------------------------- *)

(* If [--only-tokens] was specified on the command line,
   produce the definition of the [token] type and stop. *)

let () =
  let open Settings in
  if token_type_only then
    let module TokenType = TokenType.Make(Settings) in
    TokenType.write base inspection comment grammar;
    exit 0

(* ------------------------------------------------------------------------- *)

(* Perform reachability analysis. *)

let grammar =
  monitor @@ fun c ->
  Time.time "Detecting unreachable symbols" @@ fun () ->
  Trim.trim c grammar

(* ------------------------------------------------------------------------- *)

(* If [--infer] was specified on the command line, perform type inference.
   The OCaml type of every nonterminal symbol is then known. *)

(* If [--depend] or [--raw-depend] was specified on the command line,
   perform dependency analysis and stop. *)

(* The purpose of [--depend] and [--raw-depend] is to support [--infer].
   Indeed, [--infer] is implemented by producing a mock [.ml] file (which
   contains just the semantic actions) and invoking [ocamlc]. This requires
   certain [.cmi] files to exist. So, [--(raw-)depend] is a way for us to
   announce which [.cmi] files we need. It is implemented by producing the
   mock [.ml] file and running [ocamldep] on it. We also produce a mock
   [.mli] file, even though in principle it should be unnecessary -- see
   comment in [nonterminalType.mli]. *)

(* If [--infer-write-query] was specified on the command line, write a
   mock [.ml] file and stop. It is then up to the user (or build system)
   to invoke [ocamlc -i] on this file, so as to do type inference. *)

(* If [--infer-read-reply] was specified on the command line, read the
   inferred [.mli] file. The OCaml type of every nonterminal symbol is
   then known, just as with [--infer]. *)

let grammar =
  let module G = struct let grammar = grammar end in
  let module Infer = Infer.Make(G)(Settings) in
  Settings.(match infer with
  | IMNone ->
      grammar
  | IMInfer ->
      let grammar =
        Time.time "Inferring types for nonterminal symbols" @@ fun () ->
        Infer.infer()
      in
      grammar
  | IMDependRaw ->
      Infer.depend `RawDepend;
      exit 0
  | IMDependPostprocess ->
      Infer.depend `Depend;
      exit 0
  | IMWriteQuery filename ->
      Infer.write_query filename;
      exit 0
  | IMReadReply filename ->
      Channels.enable();
      let grammar =
        Time.time "Reading inferred types for nonterminals" @@ fun () ->
        Infer.read_reply filename
      in
      grammar
  )

(* ------------------------------------------------------------------------- *)

(* Expand away some of the position keywords. *)

let grammar =
  Time.time "Expanding away certain position keywords" @@ fun () ->
  KeywordExpansion.expand grammar

(* ------------------------------------------------------------------------- *)

let grammar_before_inlining =
  grammar

(* ------------------------------------------------------------------------- *)

(* Expand away the nonterminal symbols marked %inline. *)

(* If [--no-inline] is specified on the command line, then all %inline
   annotations are ignored; this step is skipped. *)

(* In [--rocq] mode, %inline is forbidden. There are two reasons for this.
   One technical reason is that inlining requires constructing composite
   semantic actions (using [Action.compose], etc.) and this construction is
   currently OCaml-specific. (This could be rather easily changed, though.)
   A more philosophical reason is that we don't want to have a large gap
   between the grammar written by the user in the .mly file and the grammar
   written by Menhir in the .v file. The latter grammar is the reference
   grammar, the one with respect to which the generated parser is proved
   correct. *)

let grammar =
  if Settings.inline then
    monitor @@ fun c ->
    if Settings.backend = `RocqBackend then
      CheckInlineUsage.check c grammar;
    Time.time "Expanding away %inline nonterminal symbols" @@ fun () ->
    Inlining.inline c grammar
    (* 2018/05/23 Removed the warning that was issued when %inline was used
       but --infer was turned off. Most people should use ocamlbuild or dune
       anyway. *)
  else
    grammar

(* ------------------------------------------------------------------------- *)

(* If requested by the user, expand away some or all occurrences of nullable
   symbols. *)

(* This process can fail in the presence of certain %prec annotations or
   %on_error_reduce declarations. If [--test-GLR] has been passed on the
   command line then we silently erase these problematic annotations, so
   as to guarantee success. *)

let message policy =
  match policy with
  | `ExpandNullableSymbols ->
      "Expanding away all nullable symbols"
  | `ExpandNullableSuffixParticipants
  | `ExpandNullableSuffixes ->
      "Expanding away some nullable symbols"

let grammar =
  match Settings.expand_nullable with
  | Some policy ->
      monitor @@ fun c ->
      Time.time (message policy) @@ fun () ->
      let forceful = Settings.test_GLR in
      ExpandNullable.transform c (getG 1) forceful policy grammar
  | None ->
      grammar

(* ------------------------------------------------------------------------- *)

(* Check that we have just the right amount of %merge functions. *)

let () =
  monitor @@ fun c ->
  CheckMergeFunctions.check Settings.enabled_GLR c grammar

(* ------------------------------------------------------------------------- *)

(* If [--only-preprocess] or [--only-preprocess-drop] was specified on the
   command line, print the grammar and stop. Otherwise, continue. *)

let () =
  match Settings.preprocess_mode with
  | Settings.PMOnlyPreprocess mode ->
      PlainSyntaxPrinter.print mode stdout grammar;
      exit 0
  | Settings.PMNormal ->
      ()
