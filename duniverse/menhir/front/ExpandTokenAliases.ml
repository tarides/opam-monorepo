(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let value, position = Located.(value, position)
open Syntax

module Make (E : sig
  open Report

  (**A channel for reporting errors. *)
  val main: channel

end) = struct
open E

(* -------------------------------------------------------------------------- *)

(* An environment, or alias map, records the token aliases declared across all
   partial grammars. It is a map of aliases to (located) terminal symbols. In
   a first pass, we build such an environment. Then, we traverse the partial
   grammars and expand all token aliases by looking up this environment. *)

type env =
  terminal located StringMap.t

(* -------------------------------------------------------------------------- *)

(* Extend an alias map by scanning a declaration. *)

let scan_declaration env (decl : declaration located) : env =
  match value decl with
  | DToken (_, t, Some qid, _) ->
      let tloc = Located.map (fun _ -> t) decl in
      begin match StringMap.find qid env with
      | exception Not_found ->
          (* Good: this alias does not exist yet. Record it. *)
          StringMap.add qid tloc env
      | tloc' ->
          (* Oops: [qid] has already been declared as an alias for
             some other token. *)
          Report.error main
            [position tloc; position tloc']
            "%s cannot be declared as an alias for the symbol %s.\n\
             It has already been declared as an alias for %s."
            qid t (value tloc')
      end
  | _ ->
      env

(* Extend an alias map by scanning a partial grammar. *)

let scan_grammar env (g : partial_grammar) : env =
  List.fold_left scan_declaration env g.pg_declarations

(* Extend an alias map by scanning a list of partial grammars. *)

let scan_grammars (gs : partial_grammar list) : env =
  List.fold_left scan_grammar StringMap.empty gs

(* -------------------------------------------------------------------------- *)

(* Expand a possible alias, returning a name which definitely is not an
   alias (and may or may not be a valid terminal symbol). *)

let transform_terminal pos (env : env) (t : terminal) : terminal =
  (* [t] is either a terminal symbol or a token alias. If it starts with
     a double quote, then it must be a token alias. *)
  if t.[0] = '"' then
    match StringMap.find t env with
    | id ->
        value id
    | exception Not_found ->
        Report.error main [pos]
          "the token alias %s was never declared." t
  else
    t

(* Perform alias expansion throughout a partial grammar. *)

let transform_symbol env (sym : terminal located) =
  Located.map (transform_terminal (position sym) env) sym

let rec transform_parameter env (param : parameter) =
  Parameter.map (transform_symbol env) param

and transform_parameters env params =
  List.map (transform_parameter env) params

and transform_producer env (producer : producer) =
  let id, param, attrs = producer in
  id, (transform_parameter env param), attrs

and transform_producers env producers =
  List.map (transform_producer env) producers

and transform_branch env (branch : parameterized_branch) =
  { branch with pb_producers = transform_producers env branch.pb_producers }

and transform_branches env branches =
  List.map (transform_branch env) branches

let transform_rule env rule =
  { rule with pr_branches = transform_branches env rule.pr_branches }

let transform_decl env (decl : declaration located) =
  let pos = position decl in
  Located.map (fun (decl : declaration) ->
    match decl with
    | DCode _
    | DParameter _
    | DToken _
    | DStart _
    | DGrammarAttribute _
    | DDefaultMergeFunction _ ->
        decl
    | DTokenProperties (t, assoc, prec) ->
        let t = transform_terminal pos env t in
        DTokenProperties (t, assoc, prec)
    | DType (ty, param) ->
        DType (ty, transform_parameter env param)
    | DSymbolAttributes (params, attrs) ->
        DSymbolAttributes (transform_parameters env params, attrs)
    | DOnErrorReduce (param, level) ->
        DOnErrorReduce (transform_parameter env param, level)
  ) decl

let transform_grammar env g =
  { g with
    pg_declarations = List.map (transform_decl env) g.pg_declarations;
    pg_rules = List.map (transform_rule env) g.pg_rules }

let transform_grammars env gs =
  List.map (transform_grammar env) gs

(* -------------------------------------------------------------------------- *)

(* The two phases above are combined as follows. *)

let transform gs =
  let env = scan_grammars gs in
  transform_grammars env gs

end (* Make *)

(* -------------------------------------------------------------------------- *)

(* Re-package [Make] as a function. *)

let transform main gs =
  let module M = Make(struct let main = main end) in
  M.transform gs
