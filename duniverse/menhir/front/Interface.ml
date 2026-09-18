(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let if1, provided = MList.(if1, provided)
open PlainSyntax
open PlainSyntaxAccessors
open IL
open ILConstruction
open FrontAPI
open NonterminalType

(* -------------------------------------------------------------------------- *)

(* The internal submodule [Basics]. *)

module Basics (X : sig
  include EXN_SETTINGS
  include TOKEN_TYPE_SETTINGS
end) = struct

  module TokenType =
    TokenType.Make(X)
  open TokenType

  (* The exception [Error]. *)

  let excname =
    "Error"

  (* If [fixedexc] is [true] then the exception [Error] is defined as a
     synonym for [Parsing.Parse_error]. Otherwise it is defined as a new
     exception. *)

  let exceq =
    if X.fixedexc then Some "Parsing.Parse_error" else None

  (* If [exn_carries_state] is [true] then this exception carries an integer
     parameter, which is a state number. If [exn_carries_top_nodes] then this
     exception carries a list of top GSS nodes. Otherwise it carries no
     parameter. *)

  let excparams =
    if X.exn_carries_state then
      [ tint ]
    else if X.exn_carries_top_nodes then
      let tnode = TypApp ("MenhirGLR.GSS.node", [ tint; tobj ]) in
      [ tlist tnode ]
    else
      []

  let excdef =
    { excname; exceq; excparams }

  (* The toplevel function [stop] raises the exception [Error]. *)

  (* Not available in the GLR back-end. *)

  let stop =
    "_eRR"

  let call_stop (s : int) =
    EApp (EVar stop,
      if X.exn_carries_state then [EIntConst s] else [EUnit]
    )

  let stop_def =
    let s = "_s" in
    let args = if X.exn_carries_state then [ EVar s ] else [] in
    let body = EFun ([PVar s], ERaise (EData (excname, args))) in
    def stop body

  (* The name of the internal submodule [Basics]. *)

  let basics_submodule_name =
    "MenhirBasics"
    (* 2017/01/20 This name must be an unlikely name, as it might
       otherwise hide a user-defined module by the same name. *)

  (* The definition of the internal submodule [Basics]. *)

  let basics_submodule_def grammar = [

    (* The module [Basics]. *)
    SIModuleDef (basics_submodule_name, MStruct (

      (* The exception [Error]. *)
      SIExcDefs [ excdef ] ::

      (* The definition of [stop] must be placed at this particular point
         so as to avoid the risk of a name collision. *)
      if1 (not X.exn_carries_top_nodes) (SIValDefs (false, [ stop_def ])) @

      (* The type [token]. *)
      interface_to_structure (
        tokentypedef grammar
      )

    ));

    (* Include the above submodule. *)
    SIInclude (MVar basics_submodule_name);

  ]

(* -------------------------------------------------------------------------- *)

end (* Basics *)

(* -------------------------------------------------------------------------- *)

(* The interface. *)

module Make (X : sig
  include API_SETTINGS
  include COMMENT_SETTINGS
  include EXN_SETTINGS
  include TOKEN_TYPE_SETTINGS
end)
= struct

module B = Basics(X)
let excdef = B.excdef

module TokenType =
  TokenType.Make(X)
open TokenType

(* -------------------------------------------------------------------------- *)

(* The type ['a checkpoint] is defined in the submodule [Interpreter].
   This type exists in the incremental API only. See [IncrementalEngine]. *)

let tcheckpoint t =
  TypApp (interpreter_submodule_name ^ ".checkpoint", [ t ])

(* -------------------------------------------------------------------------- *)

(* The type of the entry point associated with the start symbol [symbol]
   in the monolithic API. *)

let monolithic_decl grammar symbol =
  let ty = TypTextual (ocamltype_of_start_symbol grammar symbol) in
  let scheme = type2scheme (marrow [ arrow tlexbuf ttoken; tlexbuf ] ty) in
  MString.normalize symbol, scheme

let monolithic_decls grammar =
  StringSet.fold (fun symbol decls ->
    monolithic_decl grammar symbol :: decls
  ) grammar.start_symbols []

(* -------------------------------------------------------------------------- *)

(* The monolithic API contains the type [token], the exception [Error],
   and the monolithic entry points. *)

let monolithic_api grammar =
  tokentypedef grammar @
  IIComment "This exception is raised by the monolithic API functions." ::
  IIExcDecls [ excdef ] ::
  IIComment "The monolithic API." ::
  IIValDecls (monolithic_decls grammar) ::
  []

(* -------------------------------------------------------------------------- *)

(* The type of the entry point associated with the start symbol [symbol]
   in the incremental API. *)

let incremental_decl grammar symbol =
  let ty = TypTextual (ocamltype_of_start_symbol grammar symbol) in
  let scheme = type2scheme (arrow tposition (tcheckpoint ty)) in
  MString.normalize symbol, scheme

let incremental_decls grammar =
  StringSet.fold (fun symbol decls ->
    incremental_decl grammar symbol :: decls
  ) grammar.start_symbols []

(* -------------------------------------------------------------------------- *)

(* The inspection API. *)

(* This API forms an optional add-on to the incremental API. *)

let inspection_api grammar () =
  let a = "a" in
  (* Definitions of the types [terminal] and [nonterminal]. *)
  tokengadtdef grammar @
  nonterminalgadtdef grammar @
  (* Include the signature [INSPECTION] with suitable type instantiations. *)
  IIComment "The inspection API." ::
  IIInclude (
    with_types WKDestructive
      "MenhirLib.IncrementalEngine.INSPECTION" [
        [ a ], "lr1state", TypApp ("lr1state", [TypVar a]);
        [], "production", TypApp ("production", []);
        [ a ], tctokengadt, ttokengadt (TypVar a);
        [ a ], tcnonterminalgadt, tnonterminalgadt (TypVar a);
        [ a ], "env", TypApp ("env", [ TypVar a ]);
      ]
  ) ::
  []

(* -------------------------------------------------------------------------- *)

(* The incremental API. *)

let incremental_submodule_name =
  "Incremental"

let with_type_token_eq_token (signature : string) : module_type =
  (* [signature with type token = token] *)
  with_types WKNonDestructive signature
    [
      [], "token", ttoken
        (* we cannot use [tctoken], which may be qualified *)
    ]

let incremental_engine () : module_type =
  with_type_token_eq_token
    "MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE"

let incremental_entry_points grammar : interface =
  IIComment "The entry point(s) to the incremental API." ::
  IIModule (incremental_submodule_name, MTSigEnd [
    IIValDecls (incremental_decls grammar)
  ]) ::
  []

let incremental_api grammar () : interface =
  IIModule (
    interpreter_submodule_name,
    MTSigEnd (
      IIComment "The incremental API." ::
      IIInclude (incremental_engine()) ::
      provided X.inspection (inspection_api grammar)
    )
  ) ::
  (* The entry points must come after the incremental API,
     because their type refers to the type [checkpoint]. *)
  incremental_entry_points grammar

(* -------------------------------------------------------------------------- *)

(* The unparsing API. *)

let unparsing_api grammar () : interface =
  UnparsingAPI.unparsing_API grammar

(* -------------------------------------------------------------------------- *)

(* 2026/01/07 Following a request by Frédéric Bour and Thomas Refis,
   if the table back-end has been selected, then we expose the parse
   tables, with the signature [MenhirLib.TableFormat.TABLES]. This
   is considered undocumented and temporary, as I wish to retain the
   freedom of changing the format of the parse tables in the future. *)

let tables () =
  [
    IIComment "The parse tables.";
    IIComment "Warning: this submodule is undocumented. In the future,\n   \
               its type could change, or it could disappear altogether.";
    IIModule (
      "Tables",   (* must agree with [TableBackend] *)
      with_type_token_eq_token "MenhirLib.TableFormat.TABLES"
    );
  ]

(* -------------------------------------------------------------------------- *)

(* The complete interface of the generated parser. *)

let interface grammar = [
  IIFunctor (grammar.parameters,
    monolithic_api grammar @
    provided X.incremental (incremental_api grammar) @
    provided X.unparsing (unparsing_api grammar) @
    provided X.incremental tables @
    []
  )
]

(* -------------------------------------------------------------------------- *)

(* Writing the interface to a file. *)

let write grammar mliname () =
  let mli = open_out mliname in
  let module P = ILPrinter.Make (struct
    let f = mli
    let print_line_directives = None
    include X
  end) in
  P.interface (interface grammar);
  close_out mli

(* -------------------------------------------------------------------------- *)

end (* Make *)
