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

(* For each anonymous rule, we define a fresh nonterminal symbol, and replace
   the anonymous rule with a reference to this symbol. If the anonymous rule
   appears inside a parameterized rule, then we must define a parameterized
   nonterminal symbol. *)

(* ------------------------------------------------------------------------ *)

(* This functor makes it easy to share mutable internal state between the
   functions that follow. *)

module Run () = struct

(* ------------------------------------------------------------------------ *)

(* A fresh name generator. *)

let next =
  ref 0

let fresh () =
  Printf.sprintf "__anonymous_%d" (MInt.postincrement next)

(* ------------------------------------------------------------------------ *)

(* This accumulator collects the fresh rules that we produce. *)

let rules =
  ref []

(* ------------------------------------------------------------------------ *)

(* [anonymous xs branches] deals with an anonymous rule, which appears inside
   a possibly-parameterized rule whose parameters are [xs], and whose body is
   [branches]. We assume that [branches] does not itself contain any anonymous
   rules. As a side effect, we create a fresh definition, and return its
   name. *)

(* The syntax of anonymous rules currently does not allow them to carry a
   %merge function. This feature could be easily added if desired. *)

let anonymous xs (branches : parameterized_branch list located) : parameter =
  let pos, branches = position branches, value branches in
  (* Compute the free symbols of [branches]. They should form a subset of
     [xs], although we have not yet checked this. We create a definition that
     is parameterized only over the parameters that actually occur free in the
     definition -- i.e., a definition without useless parameters. This seems
     important, as (in some situations) it avoids duplication and leads to
     fewer states in the automaton. *)
  let used = FreeNames.branches branches in
  let xs = List.filter (fun x -> StringSet.mem x used) xs in
  (* Generate a fresh non-terminal symbol. *)
  let symbol = fresh() in
  (* Construct its definition. Note that it is implicitly marked %inline.
     Also, it does not carry any attributes; this is consistent
     with the fact that %inline symbols cannot carry attributes. *)
  let rule = {
    pr_public      = false;
    pr_inline      = true;
    pr_nt          = symbol;
    pr_positions   = [ pos ]; (* this list must not be empty *)
    pr_attributes  = [];
    pr_parameters  = xs;
    pr_branches    = branches;
    pr_merge       = None;
  } in
  (* Record this definition. *)
  rules := rule :: !rules;
  (* Return the symbol that stands for it. *)
  Parameter.apply (locate pos symbol) (List.map Parameter.var xs)

(* ------------------------------------------------------------------------ *)

(* Traversal code. *)

let rec transform_parameter xs (p : parameter) =
  match p with
  | ParamVar x ->
      ParamVar x
  | ParamApp (x, ps) ->
      ParamApp (x, List.map (transform_parameter xs) ps)
  | ParamAnonymous branches ->
      (* Do not forget the recursive invocation! *)
      let branches = Located.map (List.map (transform_branch xs)) branches in
      (* This is where the real work is done. *)
      anonymous xs branches

and transform_producer xs ((x, p, attrs) : producer) =
  x, transform_parameter xs p, attrs

and transform_branch xs branch =
  let pb_producers = List.map (transform_producer xs) branch.pb_producers in
  { branch with pb_producers }

let transform_rule rule =
  let xs = rule.pr_parameters in
  let pr_branches = List.map (transform_branch xs) rule.pr_branches in
  { rule with pr_branches }

end

(* ------------------------------------------------------------------------ *)

(* The main entry point invokes the functor and reads its result. *)

let transform g =
  let module R = Run() in
  let pg_rules = List.map R.transform_rule g.pg_rules in
  let pg_rules = !R.rules @ pg_rules in
  { g with pg_rules }
