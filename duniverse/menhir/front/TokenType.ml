(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let provided = MList.provided
open PlainSyntax
open PlainSyntaxAccessors
open IL
open ILConstruction
open FrontAPI

module Make (X : TOKEN_TYPE_SETTINGS) = struct
open X

let interpreter_submodule_name =
  "MenhirInterpreter"

let tctoken =
  "token"

let ttoken =
  TypApp (tctoken, [])

let tctokengadt =
  "terminal"

let ttokengadt a =
  TypApp (tctokengadt, [ a ])

let ttokengadtdata token =
  "T_" ^ token

(* The token type and the token GADT can be referred to via an unqualified
   name, regardless of how they have been defined (either directly or as an
   abbreviation). However, if the mode is [`UseExternalTokenType _], then the
   data constructors must be qualified. *)

(* If the mode is [`UseExternalTokenType M] then the data constructors of the
   type [token] must be prefixed with [M.] and the data constructors of the
   type [_ terminal] must be prefixed with [M.Interpreter.]. This was fixed
   on 2023/04/28. *)

let tokenprefix id =
  match mode with
  | `UseExternalTokenType m ->
      m ^ "." ^ id
  | `DefineTokenType ->
      id

let tokendata =
  tokenprefix

let tokengadtprefix id =
  match mode with
  | `UseExternalTokenType m ->
      let m' = m ^ "." ^ interpreter_submodule_name in
      m' ^ "." ^ id
  | `DefineTokenType ->
      id

let tokengadtdata token =
  tokengadtprefix (ttokengadtdata token)

let tokentyperhs grammar =
  match mode with
  | `DefineTokenType ->
      (* Algebraic data type. *)
      TDefSum (
        List.map (fun (tok, typo) -> {
          dataname = tok;
          datavalparams =
            (match typo with None -> [] | Some t -> [ TypTextual t ]);
          datatypeparams = None;
          comment = None;
          unboxed = false;
        }) (typed_tokens grammar)
      )
  | `UseExternalTokenType _ ->
      (* Type abbreviation. *)
      TAbbrev (TypApp (tokenprefix tctoken, []))

let tokentypedef grammar =
  [
    IIComment "The type of tokens.";
    IITypeDecls (`Rec, [{
      typename = tctoken;
      typeparams = [];
      typerhs = tokentyperhs grammar;
      typeconstraint = None
    }])
  ]

(* Although the [token] type does not include the [error] token (because
   this token is never produced by the lexer), the token GADT must include
   the [error] token, because this GADT must describe all of the tokens that
   are allowed to appear in a production. *)

let tokengadt_param_rhs grammar =
  match mode with
  | `DefineTokenType ->
      (* Generalized algebraic data type. *)
      let param = "_" in
      param,
      TDefSum (
        (* The ordering of this list matters. We want the data constructors
           to respect the internal ordering (as determined by [typed_tokens]
           in [PlainSyntax]) of the terminal symbols. This may be exploited
           in the table back-end to allow an unsafe conversion of a data
           constructor to an integer code. See [t2i] in
           [InspectionTableInterpreter]. *)
        List.map (fun (token, typo) -> {
          dataname = ttokengadtdata token;
          datavalparams = [];
          datatypeparams =
            Some [ match typo with None -> tunit | Some t -> TypTextual t ];
          comment = None;
          unboxed = false;
        }) (("error", None) :: typed_tokens grammar)
      )
  | `UseExternalTokenType _ ->
      (* Type abbreviation. *)
      let param = "a" in
      param,
      TAbbrev (TypApp (tokengadtprefix tctokengadt, [ TypVar param ]))

let tokengadtdef grammar =
  let param, rhs = tokengadt_param_rhs grammar in
  [
    IIComment "The indexed type of terminal symbols.";
    IITypeDecls (`Rec, [{
      typename = tctokengadt;
      typeparams = [ param ];
      typerhs = rhs;
      typeconstraint = None
    }])
  ]

(* -------------------------------------------------------------------------- *)

(* Creating both an [.mli] file and an [.ml] file is made necessary by the
   fact that the two files can differ when there are functor parameters. *)

let write base inspection comment grammar =
  (* Settings. *)
  let module F = struct
    let print_line_directives = None
    let comment = comment
  end in
  (* Construct the definitions of the types [token] and [_ terminal]. *)
  let interpreter_submodule_def () =
    [ IIModule (interpreter_submodule_name, MTSigEnd (tokengadtdef grammar)) ]
  in
  let i : interface =
    tokentypedef grammar @
    provided inspection interpreter_submodule_def
  in
  (* Write an [.mli] file. *)
  let f = open_out (base ^ ".mli") in
  let module P = ILPrinter.Make (struct include F let f = f end) in
  P.interface [ IIFunctor (grammar.parameters, i) ];
  (* Write an [.ml] file. *)
  let f = open_out (base ^ ".ml") in
  let module P = ILPrinter.Make (struct include F let f = f end) in
  P.program [ SIFunctor (grammar.parameters, interface_to_structure i) ]

end (* Make *)
