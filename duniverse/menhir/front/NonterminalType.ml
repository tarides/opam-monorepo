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
open PlainSyntaxAccessors
open IL

let tcnonterminalgadt =
  "nonterminal"

let tnonterminalgadt a =
  TypApp (tcnonterminalgadt, [ a ])

let tnonterminalgadtdata nt =
  "N_" ^ MString.normalize nt

(* Generating the definition of the type [nonterminal] requires the type of
   every nonterminal symbol to be explicitly given by a %type declaration.
   We assume that this check has been performed up front. *)

(* This said, when [--depend] or [--raw-depend] is used, [nonterminalgadtdef]
   can be invoked by [Infer.depend] via [Interface.write]. Then, we may
   encounter a nonterminal symbol whose type is not known. In that case, we
   give up and define ['a nonterminal] as an abstract type. This lets us
   produce a mock [.mli] file that is an approximation of the real [.mli]
   file.*)

exception MissingOCamlType

let ocamltype_of_symbol grammar nt : typ =
  (* Check that there is a %type declaration for [nt]. *)
  match StringMap.find nt grammar.types with
  | ty ->
      TypTextual ty
  | exception Not_found ->
      raise MissingOCamlType

let datadef grammar nt : datadef =
  {
    dataname       = tnonterminalgadtdata nt;
    datavalparams  = [];
    datatypeparams = Some [ ocamltype_of_symbol grammar nt ];
    comment        = None;
    unboxed        = false;
  }

(* The ordering of the list [datadefs grammar] matters. We want the data
   constructors to respect the internal ordering of the nonterminal symbols,
   as determined by the function [PlainSyntax.nonterminals]. This is exploited
   in the table back-end to perform an unsafe conversion of a data constructor
   to an integer code. See [InspectionTableInterpreter.n2i]. *)

let datadefs grammar : datadef list =
  List.map (datadef grammar) (nonterminals grammar)

let nonterminalgadtdef grammar : interface =
  let comment, datadefs =
    try
      "The indexed type of nonterminal symbols.",
      datadefs grammar
    with MissingOCamlType ->
      (* Give up and define ['a nonterminal] as an abstract type. *)
      "The indexed type of nonterminal symbols (mock!).",
      []
  in
  [
    IIComment comment;
    IITypeDecls (`Rec, [{
      typename = tcnonterminalgadt;
      typeparams = [ "_" ];
      typerhs = TDefSum datadefs;
      typeconstraint = None
    }])
  ]
