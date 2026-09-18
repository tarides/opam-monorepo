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
open PlainSyntaxPrinter

let error format =
  ksprintf (fun s -> prerr_string s; exit 1) format

let unsupported feature () =
  error "Error: %s is no longer supported.\n" feature

(* ------------------------------------------------------------------------- *)
(* Prepare for parsing the command line. *)

let backend =
  ref `Unspecified

let set_backend b () =
  backend := b

let mode =
  ref `DefineTokenType

let token_type_only =
  ref false

let only_tokens () =
  mode := `DefineTokenType;
  token_type_only := true

let is_uppercase_ascii c =
  c >= 'A' && c <= 'Z'

let is_capitalized_ascii s =
  String.length s > 0 &&
  is_uppercase_ascii s.[0]

let codeonly m =
  if not (is_capitalized_ascii m) then
    (* Not using module [Error] to avoid a circular dependency. *)
    error "Error: %s is not a valid OCaml module name.\n" m;
  mode := `UseExternalTokenType m;
  token_type_only := false

let version =
  ref false

(* Note that --canonical overrides --no-pager. If both are specified, the result
   is a canonical automaton. *)

let construction_mode =
  ref `Pager

let explain =
  ref false

let base =
  ref ""

let dump =
  ref false

let dump_menhirLib =
  ref None

let dump_resolved =
  ref false

let reference_graph =
  ref false

let automaton_graph =
  ref false

let trace =
  ref false

let noprefix =
  ref false

type preprocess_mode =
  | PMNormal
  | PMOnlyPreprocess of mode

let preprocess_mode =
  ref PMNormal

let v () =
  dump := true;
  explain := true

let inline =
  ref true

type infer_mode =
    (* Perform no type inference. This is the default mode. *)
  | IMNone
    (* Perform type inference by invoking ocamlc directly. *)
  | IMInfer                (* --infer *)
  | IMDependRaw            (* --raw-depend *)
  | IMDependPostprocess    (* --depend *)
    (* Perform type inference by writing a mock .ml file and
       reading the corresponding inferred .mli file. *)
  | IMWriteQuery of string (* --infer-write-query <filename> *)
  | IMReadReply of string  (* --infer-read-reply <filename> *)

let show_infer_mode = function
  | IMNone ->
      ""
  | IMInfer ->
      "--infer"
  | IMDependRaw ->
      "--raw-depend"
  | IMDependPostprocess ->
      "--depend"
  | IMWriteQuery _ ->
      "--infer-write-query"
  | IMReadReply _ ->
      "--infer-read-reply"

let infer =
  ref IMNone

let set_infer_mode mode2 =
  let mode1 = !infer in
  match mode1, mode2 with
  | IMNone, _ ->
      infer := mode2
  (* It is valid to specify [--infer] in conjunction with [--depend] or
     [--raw-depend]. The latter command then takes precedence. This is
     for compatibility with Menhir prior to 2018/05/23. *)
  | IMInfer, (IMInfer | IMDependRaw | IMDependPostprocess) ->
      infer := mode2
  | (IMDependRaw | IMDependPostprocess), IMInfer ->
      ()
  | _, _ ->
      error "Error: you cannot use both %s and %s.\n"
        (show_infer_mode mode1)
        (show_infer_mode mode2)

let enable_infer () =
  set_infer_mode IMInfer

let enable_depend () =
  set_infer_mode IMDependPostprocess

let enable_raw_depend () =
  set_infer_mode IMDependRaw

let enable_write_query filename =
  set_infer_mode (IMWriteQuery filename)

let enable_read_reply filename =
  set_infer_mode (IMReadReply filename)

let code_inlining =
  ref true

let represent_positions =
  ref false

let represent_states =
  ref false

let represent_values =
  ref false

let represent_everything () =
  represent_positions := true;
  represent_states := true;
  represent_values := true

let comment =
  ref false

let ocamlc =
  ref "ocamlc"

let ocamldep =
  ref "ocamldep"

let filenames =
  ref StringSet.empty

let no_stdlib =
  ref false

let insert name =
  filenames := StringSet.add name !filenames

let interpret =
  ref `No

let optimization_level =
  ref 1
    (* -O2 was the default since the new code back-end appeared, but
        was perhaps a little too aggressive. From 2023/04/15 on,
        -O1 is the default. *)

let inspection =
  ref false

let unparsing =
  ref false

let rocq_no_version_check =
  ref false

let rocq_no_complete =
  ref false

let rocq_no_actions =
  ref false

let fixedexc =
  ref false

let exn_carries_state =
  ref false

type suggestion =
  | SuggestNothing
  | SuggestCompFlags
  | SuggestLinkFlags of string (* "cmo" or "cmx" *)
  | SuggestWhereIsMenhirLibSource
  | SuggestUseOcamlfind

let suggestion =
  ref SuggestNothing

let ignored_unused_tokens =
  ref StringSet.empty

let ignore_unused_token t =
  ignored_unused_tokens := StringSet.add t !ignored_unused_tokens

let ignore_all_unused_tokens =
  ref false

let ignore_all_unused_precedence_levels =
  ref false

let list_errors =
  ref false

let compile_errors =
  ref None

let set_compile_errors filename =
  compile_errors := Some filename

let compare_errors =
  ref []

let add_compare_errors filename =
  compare_errors := filename :: !compare_errors

let merge_errors =
  ref []

let add_merge_errors filename =
  merge_errors := filename :: !merge_errors

let update_errors =
  ref None

let set_update_errors filename =
  update_errors := Some filename

let echo_errors =
  ref None

let set_echo_errors filename =
  echo_errors := Some filename

let echo_errors_concrete =
  ref None

let set_echo_errors_concrete filename =
  echo_errors_concrete := Some filename

let cmly =
  ref false

let rocq_lib_path =
  ref (Some "MenhirLib")

let require_aliases =
  ref false

let random_sentence_symbol =
  ref None

let random_sentence_goal =
  ref 0

let random_sentence_style =
  ref `Abstract

let random_sentence_abstract symbol =
  random_sentence_symbol := Some symbol;
  random_sentence_style := `Abstract

let random_sentence_concrete symbol =
  random_sentence_symbol := Some symbol;
  random_sentence_style := `Concrete;
  require_aliases := true

let strategy =
  ref `Unspecified

let set_strategy = function
  | "legacy" ->
      strategy := `Legacy
  | "simplified" ->
      strategy := `Simplified
  | _ ->
      error "Error: --strategy should be followed with legacy | simplified.\n"

let stacklang_dump =
  ref false

let stacklang_graph =
  ref false

let stacklang_test =
  ref false

let specialize_token =
  ref false

let pack_classic =
  ref false

let expand_nullable =
  ref None

let test_GLR =
  ref false

let kill_priorities =
  ref false

let avoid_eos =
  ref false

(* When new command line options are added, please update both the manual
   in [doc/manual.tex] and the man page in [doc/menhir.1]. *)

(* Please note that there is a very short length limit on the explanations
   here, since the output of [menhir -help] must fit in 80 columns. *)

let unsupported switch =
  switch, Arg.Unit (unsupported switch), " (no longer supported)"

let rocq_options (rocq : string) =
  let prefix = sprintf "--%s" rocq in
  [
    prefix ^ "",
    Arg.Unit (set_backend `RocqBackend),
    " Generate a verified Rocq parser"
    ;
    prefix ^ "-lib-path",
    Arg.String (fun path -> rocq_lib_path := Some path),
    "<path> Set the path to the Rocq library MenhirLib"
    ;
    prefix ^ "-lib-no-path",
    Arg.Unit (fun () -> rocq_lib_path := None),
    " Use unqualified references to the Rocq library MenhirLib"
    ;
    prefix ^ "-no-version-check",
    Arg.Set rocq_no_version_check,
    " Do not generate a version check in the Rocq parser"
    ;
    prefix ^ "-no-actions",
    Arg.Set rocq_no_actions,
    " Ignore the semantic actions in the Rocq parser"
    ;
    prefix ^ "-no-complete",
    Arg.Set rocq_no_complete,
    " Do not generate a proof of completeness of the Rocq parser"
    ;
  ]

let options = [
  "--automaton-graph", Arg.Set automaton_graph, " (undocumented)";
  "--avoid-eos", Arg.Set avoid_eos, " (undocumented)";
  "--base", Arg.Set_string base, "<basename> Specifies a base name for the output file(s)";
  "--canonical", Arg.Unit (fun () -> construction_mode := `Canonical), " Construct a canonical Knuth LR(1) automaton";
  "--cmly", Arg.Set cmly, " Write a .cmly file";
  "--code", Arg.Unit (set_backend `CodeBackend), " Use the code back-end (default)";
  unsupported "--code-ancient";
  "--comment", Arg.Set comment, " Include comments in the generated code";
  "--compare-errors", Arg.String add_compare_errors, "<filename> (used twice) Compare two .messages files";
  "--compile-errors", Arg.String set_compile_errors, "<filename> Compile a .messages file to OCaml code";
  "--depend", Arg.Unit enable_depend, " Invoke ocamldep and display dependencies";
  "--dump", Arg.Set dump, " Write an .automaton file";
  "--dump-menhirLib", Arg.String (fun path -> dump_menhirLib := Some path), "<path> Dump menhirLib.{ml,mli} at <path>";
  "--dump-resolved", Arg.Set dump_resolved, " Write an .automaton.resolved file";
  "--echo-errors", Arg.String set_echo_errors, "<filename> Echo the sentences in a .messages file";
  "--echo-errors-concrete", Arg.String set_echo_errors_concrete, "<filename> Echo the sentences in a .messages file";
  unsupported "--error-recovery";
  "--exn-carries-state", Arg.Set exn_carries_state, " Declares exception Error of int";
  "--expand-nullable-symbols", Arg.Unit (fun () -> expand_nullable := Some `ExpandNullableSymbols), " (undocumented)";
  "--expand-nullable-suffix-participants", Arg.Unit (fun () -> expand_nullable := Some `ExpandNullableSuffixParticipants), " (undocumented)";
  "--expand-nullable-suffixes", Arg.Unit (fun () -> expand_nullable := Some `ExpandNullableSuffixes), " (undocumented)";
  "--explain", Arg.Set explain, " Explain conflicts in <basename>.conflicts";
  "--external-tokens", Arg.String codeonly, "<module> Import token type definition from <module>";
  "--fixed-exception", Arg.Set fixedexc, " Declares Error = Parsing.Parse_error";
  "--GLR", Arg.Unit (set_backend `GLRBackend), " Use the GLR back-end";
  "--infer", Arg.Unit enable_infer, " Invoke ocamlc to do type inference";
  "--infer-protocol-supported", Arg.Unit (fun () -> exit 0), " Stop with exit code 0";
  "--infer-write-query", Arg.String enable_write_query, "<filename> Write mock .ml file";
  "--infer-read-reply", Arg.String enable_read_reply, "<filename> Read inferred .mli file";
  "--inspection", Arg.Set inspection, " Generate the inspection API";
  "--interpret", Arg.Unit (fun () -> interpret := `Interpret `DoNotShowCST), " Interpret the sentences provided on stdin";
  "--interpret-show-cst", Arg.Unit (fun () -> interpret := `Interpret `ShowCST), " Show a concrete syntax tree upon acceptance";
  "--interpret-error", Arg.Unit (fun () -> interpret := `InterpretError), " Interpret an error sentence";
  "--kill-priorities", Arg.Set kill_priorities, " (undocumented)";
  "--lalr", Arg.Unit (fun () -> construction_mode := `LALR), " Construct an LALR(1) automaton";
  "--list-errors", Arg.Set list_errors, " Produce a list of erroneous inputs";
  "--log-automaton", Arg.Set_int Channels.logA, "<level> Log information about the automaton";
  "--log-code", Arg.Set_int Channels.logC, "<level> Log information about the generated code";
  "--log-grammar", Arg.Set_int Channels.logG, "<level> Log information about the grammar";
  "--merge-errors", Arg.String add_merge_errors, "<filename> (used twice) Merge two .messages files";
  "--no-code-generation", Arg.Unit (set_backend `NoBackend), " Do not produce any code";
  "--no-code-inlining", Arg.Clear code_inlining, " (undocumented)";
  "--no-dollars", Arg.Unit (fun () -> ParserAux.dollars := `DollarsDisallowed), " Disallow $i in semantic actions";
  "--no-inline", Arg.Clear inline, " Ignore the %inline keyword";
  "--no-pager", Arg.Unit (fun () -> if !construction_mode = `Pager then construction_mode := `InclusionOnly), " (undocumented)";
  "--no-prefix", Arg.Set noprefix, " (undocumented)";
  "--no-stdlib", Arg.Set no_stdlib, " Do not load the standard library";
  "--ocamlc", Arg.Set_string ocamlc, "<command> Specifies how ocamlc should be invoked";
  "--ocamldep", Arg.Set_string ocamldep, "<command> Specifies how ocamldep should be invoked";
  "--only-preprocess", Arg.Unit (fun () -> preprocess_mode := PMOnlyPreprocess PrintNormal),
                       " Print grammar and exit";
  "--only-preprocess-for-ocamlyacc", Arg.Unit (fun () -> preprocess_mode := PMOnlyPreprocess PrintForOCamlyacc),
                       " Print grammar in ocamlyacc format and exit";
  "--only-preprocess-u", Arg.Unit (fun () -> preprocess_mode := PMOnlyPreprocess (PrintUnitActions false)),
                         " Print grammar with unit actions and exit";
  "--only-preprocess-uu", Arg.Unit (fun () -> preprocess_mode := PMOnlyPreprocess (PrintUnitActions true)),
                          " Print grammar with unit actions & tokens";
  "--only-tokens", Arg.Unit only_tokens, " Generate token type definition only, no code";
  "--pack-classic", Arg.Set pack_classic, " (undocumented)";
  "--unparsing", Arg.Set unparsing, " Generate the unparsing API";
  "--random-seed", Arg.Int Random.init, "<seed> Set the random seed";
  "--random-self-init", Arg.Unit Random.self_init, " Pick a random seed in a system-dependent way";
  "--random-sentence-length", Arg.Set_int random_sentence_goal, "<length> Set the goal length for a random sentence";
  "--random-sentence", Arg.String random_sentence_abstract, "<sym> Generate a random valid sentence";
  "--random-sentence-concrete", Arg.String random_sentence_concrete, "<sym> Generate a random valid sentence";
  "--raw-depend", Arg.Unit enable_raw_depend, " Invoke ocamldep and echo its raw output";
  "--reference-graph", Arg.Set reference_graph, " Write a dependency graph to a .dot file";
  "--represent-states", Arg.Set represent_states, " (undocumented)";
  "--represent-positions", Arg.Set represent_positions, " (undocumented)";
  "--represent-values", Arg.Set represent_values, " (undocumented)";
  "--represent-everything", Arg.Unit represent_everything, " (undocumented)";
  "--require-aliases", Arg.Set require_aliases, " Check that every token has a token alias";
  "--specialize-token", Arg.Set specialize_token, " (undocumented)";
  "--stacklang-dump", Arg.Set stacklang_dump, " (undocumented)";
  "--stacklang-graph", Arg.Set stacklang_graph, " (undocumented)";
  "--stacklang-test", Arg.Set stacklang_test, " (undocumented)";
  "--stdlib", Arg.String ignore, "<directory> Ignored (deprecated)";
  "--strategy", Arg.String set_strategy, "<strategy> Choose an error-handling strategy";
  "--strict", Arg.Set Channels.strict, " Change warnings into errors";
  "--suggest-comp-flags", Arg.Unit (fun () -> suggestion := SuggestCompFlags),
                          " Suggest compilation flags for ocaml{c,opt}";
  "--suggest-link-flags-byte", Arg.Unit (fun () -> suggestion := SuggestLinkFlags "cma"),
                               " Suggest link flags for ocamlc";
  "--suggest-link-flags-opt", Arg.Unit (fun () -> suggestion := SuggestLinkFlags "cmxa"),
                              " Suggest link flags for ocamlopt";
  "--suggest-menhirLib", Arg.Unit (fun () -> suggestion := SuggestWhereIsMenhirLibSource),
                         " Suggest where is MenhirLib";
  "--suggest-ocamlfind", Arg.Unit (fun () -> suggestion := SuggestUseOcamlfind),
                         " (deprecated)";
  "--table", Arg.Unit (set_backend `TableBackend), " Use the table back-end";
  "--test-GLR", Arg.Set test_GLR, " (undocumented)";
  "--timings", Arg.Unit (fun () -> Time.set_output_channel (Some stderr)), " Output internal timings to stderr";
  "--timings-to", Arg.String (fun filename -> Time.set_output_channel (Some (open_out filename))), "<filename> Output internal timings to <filename>";
  "--trace", Arg.Set trace, " Generate tracing instructions";
  "--unused-precedence-levels", Arg.Set ignore_all_unused_precedence_levels, " Do not warn about unused precedence levels";
  "--unused-token", Arg.String ignore_unused_token, "<token> Do not warn that <token> is unused";
  "--unused-tokens", Arg.Set ignore_all_unused_tokens, " Do not warn about any unused token";
  "--update-errors", Arg.String set_update_errors, "<filename> Update auto-comments in a .messages file";
  "--version", Arg.Set version, " Show version number and exit";
  "-b", Arg.Set_string base, "<basename> Synonymous with --base <basename>";
  "-lg", Arg.Set_int Channels.logG, " Synonymous with --log-grammar";
  "-la", Arg.Set_int Channels.logA, " Synonymous with --log-automaton";
  "-lc", Arg.Set_int Channels.logC, " Synonymous with --log-code";
  "-O", Arg.Set_int optimization_level, " (0|1|2) Set optimization level";
  "-t", Arg.Unit (set_backend `TableBackend), " Synonymous with --table";
  "-v", Arg.Unit v, " Synonymous with --dump --explain";
]

let compare (key1, _, _) (key2, _, _) =
  String.compare key1 key2

let options =
  Arg.align (
    options @ rocq_options "coq" @ rocq_options "rocq"
    |> List.sort compare
  )

let usage =
  sprintf "Usage: %s <options> <filenames>" Sys.argv.(0)

(* ------------------------------------------------------------------------- *)
(* Parse the command line. *)

let () =
  Arg.parse options insert usage

(* ------------------------------------------------------------------------- *)
(* If required, print a version number and stop. *)

let () =
  if !version then begin
    printf "menhir, version %s\n" Version.version;
    exit 0
  end

(* ------------------------------------------------------------------------- *)

(* Decide which back-end is used. *)

let enabled_GLR =
  !backend = `GLRBackend

let backend =
  match !interpret, !backend with
  | `Interpret show, `GLRBackend ->
      (* This is --interpret --GLR. *)
      `InterpretGLR show
  | `Interpret show, _ ->
      (* This is --interpret without --GLR. *)
      `Interpret show
  | `InterpretError, `GLRBackend ->
      (* This is --interpret-error --GLR. *)
      error "Error: the combination --interpret-error --GLR is not supported.\n"
  | `InterpretError, _ ->
      (* This is --interpret-error without --GLR. *)
      `InterpretError
  | `No, `Unspecified ->
      (* The code back-end is the default. *)
      `CodeBackend
  | `No, (`RocqBackend | `CodeBackend | `TableBackend | `GLRBackend | `NoBackend as backend) ->
      backend

let () =
  match backend with
  | `GLRBackend | `InterpretGLR _ ->
      assert enabled_GLR
  | _ ->
      assert (not enabled_GLR)

let print_backend backend =
  match backend with
  | `Interpret _
  | `InterpretError ->
      "reference interpreter"
  | `InterpretGLR _ ->
      "GLR reference interpreter"
  | `CodeBackend ->
      "code back-end"
  | `RocqBackend ->
      "Rocq back-end"
  | `TableBackend ->
      "table back-end"
  | `GLRBackend ->
      "GLR back-end"
  | `NoBackend ->
      "no back-end"

let print_strategy strategy =
  match strategy with
  | `Legacy ->
      "legacy"
  | `Simplified ->
      "simplified"

let strategy =
  let strategy = !strategy in
  match strategy, backend with
  | (`Unspecified | `Simplified), `CodeBackend ->
      (* The code back-end supports only the simplified strategy. *)
      `Simplified
  | `Simplified, (`Interpret _ | `InterpretError | `TableBackend) ->
      (* The reference interpreter and the table back-end support both
         strategies. *)
      `Simplified
  | (`Unspecified | `Legacy), (`Interpret _ | `InterpretError | `TableBackend) ->
      (* The reference interpreter and the table back-end support both
         strategies; legacy is the default, for backward compatibility. *)
      `Legacy
  | _, `RocqBackend
  | _, `GLRBackend
  | _, `InterpretGLR _
  | _, `NoBackend ->
      (* These back-ends do not care. *)
      `Simplified
  | (`Legacy as strategy), `CodeBackend ->
      error "Error: the %s does not allow --strategy %s.\n"
        (print_backend backend)
        (print_strategy strategy)

(* ------------------------------------------------------------------------- *)

(* [--exn-carries-state] is supported only by the code back-end,
   and is incompatible with [--fixed-exception]. *)

let () =
  if !exn_carries_state
  && backend <> `CodeBackend then
    error
      "Error: --exn-carries-state is supported only by the code back-end.\n"

let () =
  if !exn_carries_state
  && !fixedexc then
    error
      "Error: --fixed-exception and --exn-carries-state are incompatible.\n"

(* [--fixed-exception] is not compatible with the GLR back-end. *)

let () =
  if !fixedexc
  && backend = `GLRBackend then
    error
      "Error: --fixed-exception is not supported by the GLR back-end.\n"

(* When the GLR back-end is active, the exception [Error] carries a list of
   top nodes. *)

let exn_carries_top_nodes =
  backend = `GLRBackend

(* ------------------------------------------------------------------------- *)

(* Menhir is able to suggest compile and link flags to be passed to the
   OCaml compilers. If required, do so and stop. *)

(* If [--table] is not passed, no flags are necessary. If [--table] is
   passed, then [MenhirLib] needs to be visible (at compile time) and
   linked in (at link time). *)

(* The compilation flags are in fact meant to be used both at compile-
   and link-time. *)

let () =
  match !suggestion with
  | SuggestNothing ->
      ()
  | SuggestCompFlags ->
      if backend = `TableBackend then
        printf "-I %s\n%!" (Installation.libdir());
      exit 0
  | SuggestLinkFlags extension ->
      if backend = `TableBackend then
        printf "menhirLib.%s\n%!" extension;
      exit 0
  | SuggestWhereIsMenhirLibSource ->
      printf "%s\n%!" (Installation.libdir());
      exit 0
  | SuggestUseOcamlfind ->
      printf "false\n";
      exit 0

let write_file filename contents =
  let oc = open_out_bin filename in
  output_string oc contents;
  close_out oc

let () =
  !dump_menhirLib |> Option.iter @@ fun path ->
    write_file (Filename.concat path "menhirLib.ml") MenhirLibSource.impl;
    write_file (Filename.concat path "menhirLib.mli") MenhirLibSource.intf;
    exit 0

(* ------------------------------------------------------------------------- *)
(* Export the settings. *)

let filenames =
  StringSet.elements !filenames

let base =
  if !base = "" then
    match filenames with
    | [] ->
        error "%s\n" usage
    | [ filename ] ->
        Filename.remove_extension filename
    | _ ->
        error "Error: you must specify --base when providing multiple input files.\n"
  else
    !base

let mode =
  !mode

let token_type_only =
  !token_type_only

let construction_mode =
  !construction_mode

let explain =
  !explain

let dump =
  !dump

let dump_resolved =
  !dump_resolved

let reference_graph =
  !reference_graph

let automaton_graph =
  !automaton_graph

let trace =
  !trace

let noprefix =
  !noprefix

let code_inlining =
  !code_inlining

let inline =
  !inline

let comment =
  !comment

let preprocess_mode =
  !preprocess_mode

let ocamlc =
  !ocamlc

let ocamldep =
  !ocamldep

let optimization_level =
  !optimization_level

let inspection =
  !inspection

let () =
  if inspection && backend <> `TableBackend then
    error "Error: --inspection requires --table.\n"

let unparsing =
  !unparsing

let () =
  if unparsing && backend <> `TableBackend then
    error "Error: --unparsing requires --table.\n"

let no_stdlib =
  !no_stdlib

let rocq_no_version_check =
  !rocq_no_version_check

let rocq_no_complete =
  !rocq_no_complete

let rocq_no_actions =
  !rocq_no_actions

let fixedexc =
  !fixedexc

let exn_carries_state =
  !exn_carries_state

let ignored_unused_tokens =
  !ignored_unused_tokens

let ignore_all_unused_tokens =
  !ignore_all_unused_tokens

let ignore_unused_token t =
  ignore_all_unused_tokens ||
  token_type_only ||
  StringSet.mem t ignored_unused_tokens

let ignore_all_unused_precedence_levels =
  !ignore_all_unused_precedence_levels

let list_errors =
  !list_errors

let compile_errors =
  !compile_errors

let compare_errors =
  match !compare_errors with
  | [] ->
      None
  | [ filename2; filename1 ] -> (* LIFO *)
      Some (filename1, filename2)
  | _ ->
      error
        "To compare two .messages files, please use:\n\
         --compare-errors <filename1> --compare-errors <filename2>.\n"

let merge_errors =
  match !merge_errors with
  | [] ->
      None
  | [ filename2; filename1 ] -> (* LIFO *)
      Some (filename1, filename2)
  | _ ->
      error
        "To merge two .messages files, please use:\n\
         --merge-errors <filename1> --merge-errors <filename2>.\n"

let update_errors =
  !update_errors

let echo_errors =
  !echo_errors

let echo_errors_concrete =
  !echo_errors_concrete

let cmly =
  !cmly

let rocq_lib_path =
  !rocq_lib_path

let require_aliases =
  !require_aliases

let random_sentence =
  match !random_sentence_symbol with
  | None ->
      None
  | Some nt ->
      let goal = !random_sentence_goal
      and style = !random_sentence_style in
      Some (nt, goal, style)

let infer =
  !infer

(* If some flags imply that we will NOT produce an OCaml parser, then there is
   no need to perform type inference, so [--infer] is ignored. This saves time
   and dependency nightmares. *)

let skipping_parser_generation =
  backend = `ReferenceInterpreter ||
  backend = `RocqBackend ||
  compile_errors <> None ||
  list_errors ||
  compare_errors <> None ||
  merge_errors <> None ||
  update_errors <> None ||
  echo_errors <> None ||
  false
    (* maybe also: [preprocess_mode <> PMNormal] *)

let infer =
  match infer with
  | IMInfer when skipping_parser_generation ->
      IMNone
  | _ ->
      infer

(* Only the code back-end needs clever computations of which states, values,
   and positions are represented as part of stack cells. Thus, if any other
   back-end is requested, [represent_everything()] is called. *)

let () =
  match backend with
  | `CodeBackend ->
      ()
  | _ ->
      represent_everything()

let represent_positions =
  !represent_positions

let represent_states =
  !represent_states

let represent_values =
  !represent_values

let stacklang_dump =
  !stacklang_dump

let stacklang_graph =
  !stacklang_graph

let stacklang_test =
  !stacklang_test

let specialize_token =
  !specialize_token

let pack_classic =
  !pack_classic

let incremental =
  backend = `TableBackend

let expand_nullable =
  !expand_nullable

(* [--GLR] implies [--expand-nullable-suffix-participants]
   or [--expand-nullable-symbols]. Indeed, our GLR engine
   does not support right nullable rules. *)

let expand_nullable =
  if enabled_GLR then
    match expand_nullable with
    | None
    | Some `ExpandNullableSuffixes ->
        Some `ExpandNullableSuffixParticipants
    | Some `ExpandNullableSuffixParticipants
    | Some `ExpandNullableSymbols ->
        expand_nullable
  else
    expand_nullable

(* [--test-GLR] requires [--GLR]. *)

let test_GLR =
  !test_GLR

let () =
  if test_GLR then
    assert (backend = `GLRBackend)

let kill_priorities =
  !kill_priorities

let avoid_eos =
  !avoid_eos
