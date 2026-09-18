(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Located
open Syntax
open Sort
open SortUnification

(* -------------------------------------------------------------------------- *)

(* An environment maps (terminal and nonterminal) symbols to unification
   variables. *)

type symbol =
  string

module Env =
  StringMap

type env =
  variable Env.t

let find x env : variable =
  try
    Env.find x env
  with Not_found ->
    assert false (* unbound terminal or nonterminal symbol *)

let extend env (xvs : (symbol * variable) list) =
  MList.accumulate env xvs @@ fun env (x, v) ->
  Env.add x v env

(* -------------------------------------------------------------------------- *)

module Make (E : sig
  open Report

  (**A channel for reporting errors. *)
  val main: channel

  (**A channel for information messages. *)
  val info: channel

end) = struct
open E

(* -------------------------------------------------------------------------- *)

(* Error handling. *)

(* The following functions are the only places where [error] is called. *)

(* In [check_arity], in principle, [arity1] is the expected arity and [arity2]
   is the actual arity. This distinction does not make much sense, though, as
   we do not know which is wrong, the declaration site or the use site. So, we
   display a neutral error message. *)

let check_arity sym arity1 arity2 =
  let plural = max arity1 arity2 > 1 in
  if arity1 <> arity2 then
    Report.error main [position sym]
      "does the symbol \"%s\" expect %d or %d argument%s?"
      (value sym)
      (min arity1 arity2)
      (max arity1 arity2)
      (if plural then "s" else "")

(* This variant of [unify] is used when no unification error can arise. *)

let unify_cannot_fail sort1 sort2 =
  try
    unify sort1 sort2
  with
  | Unify _ | Occurs _ ->
      (* If the caller is right, this unification step cannot fail! *)
      assert false

(* In [unify], in principle, [sort1] is the expected sort and [sort2] is
   the actual sort. Again, this distinction does not make much sense, so
   we display a neutral error message. *)

let unify sym sort1 sort2 =
  try
    unify sort1 sort2
  with
  | Unify (v1, v2) ->
      let print v = print (decode v) in
      Report.error main [position sym]
       "how is the symbol \"%s\" parameterized?\n\
        It is used at sorts %s and %s.\n\
        The sort %s is not compatible with the sort %s."
        (value sym) (print sort1) (print sort2) (print v1) (print v2)
  | Occurs (v1, v2) ->
      let print v = print (decode v) in
      Report.error main [position sym]
       "how is the symbol \"%s\" parameterized?\n\
        It is used at sorts %s and %s.\n\
        The sort %s cannot be unified with the sort %s."
        (value sym) (print sort1) (print sort2) (print v1) (print v2)

(* -------------------------------------------------------------------------- *)

(* [allocate xs] allocates a fresh unification variable [v] for every element
   [x] of the list [xs]. It returns the lists [xvs] and [vs]. *)

let allocate (xs : 'a list) : ('a * variable) list * variable list =
  let xvs = List.map (fun x -> x, fresh()) xs in
  let  vs = List.map snd xvs in
  xvs, vs

(* -------------------------------------------------------------------------- *)

(* [check_parameter env param expected] checks that the parameter [param]
   has sort [expected]. A parameter is either a symbol or an application
   of a symbol to a number of parameters. Every application is total: the
   language does not have partial applications. The sort of every
   application is [star], but the sort of a variable is unrestricted. *)

let rec check_parameter env (param : parameter) (expected : variable) =
  match param with
  | ParamVar sym ->
      let x = value sym in
      unify sym expected (find x env)
  | ParamApp (sym, actuals) ->
      let x = value sym in
      (* This application has sort [star]. *)
      unify sym expected star;
      (* Retrieve the expected sort of each parameter. Two cases arise: if [x]
         has already been assigned an arrow sort, then we can retrieve its
         domain, which gives us the expected sort of each actual parameter;
         otherwise, we just make up a fresh arrow sort of appropriate arity.
         We could avoid this case distinction and always use the latter
         method, but the former method, when applicable, yields better error
         messages. If [sym] is a toplevel (nonterminal or terminal) symbol,
         then we will be in the first case, as we have initially assigned an
         arrow sort of appropriate arity to each such symbol. *)
      let v = find x env in
      let expected =
        match domain v with
        | Some expected ->
            check_arity sym (List.length expected) (List.length actuals);
            expected
        | None ->
            let _, expected = allocate actuals in
            unify_cannot_fail v (arrow expected);
            expected
      in
      (* Check the sort of each actual parameter. *)
      List.iter2 (check_parameter env) actuals expected
  | ParamAnonymous _ ->
      (* Anonymous rules have been eliminated already. *)
      assert false

(* -------------------------------------------------------------------------- *)

(* The following functions respectively check that a producer, a branch,
   a rule, and a grammar are well-sorted under an environment [env]. *)

let check_producer env (producer : producer) =
  let (_, param, _) = producer in
  (* A producer must have sort [star]. *)
  check_parameter env param star

let check_branch env (branch : parameterized_branch) =
  List.iter (check_producer env) branch.pb_producers

let enter_rule env (nt : symbol) (rule : parameterized_rule) : env =

  (* For each formal parameter, allocate a fresh variable. *)
  let formals, domain = allocate rule.pr_parameters in

  (* Connect these variables with the sort of the symbol [nt]. *)
  (* Because it is performed first, this particular unification
     cannot fail. *)
  unify_cannot_fail (find nt env) (arrow domain);

  (* Extend the environment. *)
  extend env formals

let check_rule env (nt : symbol) (rule : parameterized_rule) =

  (* Extend the environment within this rule. *)
  let env = enter_rule env nt rule in

  (* Check each branch in this extended environment. *)
  List.iter (check_branch env) rule.pr_branches

let check_grammar env g =

  (* Each rule must be well-sorted. *)
  StringMap.iter (check_rule env) g.p_rules;

  (* The start symbols must have sort [star]. *)
  StringMap.iter (fun nt position ->
    let sym = locate position nt in
    unify sym star (find nt env)
  ) g.p_start_symbols;

  (* A symbol that appears in a [%type] declaration must have sort [star]. *)
  List.iter (fun (param, _) ->
    check_parameter env param star
  ) g.p_types;

  (* Same rule for [%on_error_reduce] declarations. *)
  List.iter (fun (param, _) ->
    check_parameter env param star
  ) g.p_on_error_reduce;

  (* The symbols that appear in [%attribute] declarations must be well-sorted.
     Their sort is not necessarily [star]: it is legal to attach an attribute
     with a parameterized symbol. *)
  List.iter (fun (params, _) ->
    List.iter (fun param ->
      check_parameter env param (fresh())
    ) params
  ) g.p_symbol_attributes

(* -------------------------------------------------------------------------- *)

(* The main function. *)

let infer (g : grammar) : sorts =

  (* For each (terminal or nonterminal) symbol, allocate a unification
     variable. The terminal symbols have sort [star], so we can use
     this particular variable. *)

  let env =
    StringMap.fold (fun tok _ env ->
      Env.add tok star env
    ) g.p_tokens Env.empty
  in
  let env =
    Env.add "error" star env
  in

  let env =
    StringMap.fold (fun nt rule env ->
      let env = Env.add nt (fresh()) env in
      (* The following line unifies the sort of [nt] with an arrow of
         appropriate arity. It cannot fail. This strategy should lead
         to slightly better unification error messages. *)
      let _ : env = enter_rule env nt rule in
      env
    ) g.p_rules env
  in

  (* Impose sort equality constraints. *)

  check_grammar env g;

  (* Decode the environment, so the user doesn't have to deal with
     unification variables. *)

  let env = Env.map decode env in

  (* Ground any unassigned sort variables. (These should occur only in
     unreachable parts of the grammar.) This guarantees that the user
     does not have to deal with sort variables. *)

  let env = Env.map ground env in

  (* If requested, display the inferred sort of every symbol. *)

  let () =
    if Report.live info then
      env |> Env.iter @@ fun x gsort ->
      Report.log info "%s :: %s" x (print (unground gsort))
  in

  env

end (* Make *)

(* -------------------------------------------------------------------------- *)

(* Re-package [Make] as a function. *)

let infer main info g =
  let module I = Make(struct
    let main = main
    let info = info
  end) in
  I.infer g
