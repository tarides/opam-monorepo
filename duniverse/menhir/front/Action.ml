(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Keyword
open IL
open ILConstruction

type t = {

  expr: expr;
  (**The code for this semantic action. *)

  semvars: StringSet.t;
  (**The free variables that this semantic action can use in order to
     refer to a semantic value. *)

  keywords  : KeywordSet.t;
  (**The set of keywords that appear in this semantic action. These keywords
     can be thought of as free variables that refer to positions. They must
     be renamed during inlining. *)

  priority: int;
  (**This priority level is used in [Infer] to sort semantic actions.
     Actions with a lower priority level are processed first by OCaml's
     type-checker, so type errors are less likely to be reported inside
     them. Via inlining, several actions can be combined into one; in
     that case, we take a maximum. *)

}

(* -------------------------------------------------------------------------- *)

(* Construction. *)

let make priority semvars keywords expr =
  let keywords = KeywordSet.of_list keywords in
  { expr; semvars; keywords; priority }

(* -------------------------------------------------------------------------- *)

(* Building [let x = a1 in a2]. *)

let compose x a1 a2 =
  let expr      = blet [ PVar x, a1.expr ] a2.expr
  and semvars   = StringSet.(union a1.semvars (remove x a2.semvars))
  and keywords  = KeywordSet.union a1.keywords a2.keywords
  and priority  = max a1.priority a2.priority in
  { expr; semvars; keywords; priority }

(* Building [let p = x in a]. *)

let bind bvp p x a =
  let expr      = blet [ p, EVar x ] a.expr
  and semvars   = StringSet.(add x (diff a.semvars (of_list bvp)))
  and keywords  = a.keywords
  and priority  = a.priority in
  { expr; semvars; keywords; priority }

(* -------------------------------------------------------------------------- *)

(* Accessors. *)

let[@inline] expr action =
  action.expr

let[@inline] semvars action =
  action.semvars

let[@inline] keywords action =
  action.keywords

let has_beforeend action =
  KeywordSet.mem (Position (Before, WhereEnd, FlavorPosition)) action.keywords

let posvars action =
  KeywordSet.fold (fun keyword accu ->
    let x = kposvar keyword in
    StringSet.add x accu
  ) (keywords action) StringSet.empty

let vars action =
  StringSet.union (semvars action) (posvars action)

let[@inline] priority action =
  action.priority

(* -------------------------------------------------------------------------- *)

(* Defining a keyword in terms of other keywords. *)

let define keyword keywords f action =
  assert (KeywordSet.mem keyword action.keywords);
  let expr      = f action.expr
  and semvars   = action.semvars
  and keywords  = KeywordSet.(union keywords (remove keyword action.keywords))
  and priority  = action.priority in
  { expr; semvars; keywords; priority }

(* -------------------------------------------------------------------------- *)

(* A simultaneous substitution is represented as a pair of finite maps. We
   distinguish a renaming that applies to semantic-value variables and a
   renaming that applies to position-keyword variables. The two are
   unfortunately somewhat linked because our position-keyword variables are
   named after semantic-value variables; this is quite a mess. *)

type subst =
  {
    semvar: string StringMap.t;
    posvar: string StringMap.t;
  }

let empty =
  { semvar = StringMap.empty; posvar = StringMap.empty }

let extend1 x y var =
  assert (not (StringMap.mem x var));
  if x <> y then StringMap.add x y var else var

let extend_semvar x y { semvar; posvar } =
  { semvar = extend1 x y semvar; posvar }

let extend_posvar x y { semvar; posvar } =
  { semvar; posvar = extend1 x y posvar }

let extend = extend_semvar

let apply1 var x =
  try StringMap.find x var with Not_found -> x

let[@inline] apply_semvar phi x =
  apply1 phi.semvar x

let apply_subject phi subject =
  match subject with
  | Before
  | Left ->
      subject
  | RightNamed x ->
      RightNamed (apply_semvar phi x)

let def (x, y) =
  PVar x, EVar y

let[@inline] bindings var =
  var |> StringMap.bindings |> List.map def

let[@inline] bindings phi =
  bindings phi.posvar @
  bindings phi.semvar

let[@inline] restrict_semvar xs { semvar; posvar } =
  let semvar = StringMap.filter (fun x _y -> StringSet.mem x xs) semvar in
  { semvar; posvar }

(* -------------------------------------------------------------------------- *)

(* [rename_keyword f phi keyword] applies [f] to possibly transform the
   keyword [keyword]. If [f] decides to change this keyword (by returning
   [Some _]) then this decision is obeyed. Otherwise, the keyword is renamed
   by the substitution [phi]. In either case, [phi] is extended with a
   renaming decision. *)

let rename_keyword f (phi : subst ref) keyword : keyword =
  match keyword with
  | Position (subject, where, flavor) ->
      let subject', where' =
        match f (subject, where) with
        | Some (subject', where') ->
            subject', where'
        | None ->
            apply_subject !phi subject, where
      in
      let keyword' = Position (subject', where', flavor) in
      phi := extend_posvar (kposvar keyword) (kposvar keyword') !phi;
      keyword'

let rename f phi a =

  (* Rename all keywords, growing [phi] as we go. *)
  let keywords = a.keywords in
  let phi = ref phi in
  let keywords = KeywordSet.map (rename_keyword f phi) keywords in
  let phi = !phi in

  (* Restrict [phi.semvar] to the set [a.semvars], in order to avoid
     generating [let] bindings that would be both useless and harmful:
     if [x] is not free in [e], then we do not want to generate
     [let x = x' in e], as suddenly [x'] would be free in this new
     expression. *)
  let phi = restrict_semvar a.semvars phi in

  (* Apply [phi.semvar] to the set of free variables. *)
  let semvars = StringSet.map (apply_semvar phi) a.semvars in

  (* Construct a new semantic action, where [phi] is translated into
     a set of *simultaneous* [let] bindings. (We cannot use a series
     of nested [let] bindings, as that would cause a capture if the
     domain and codomain of [phi] have a nonempty intersection.) *)
  let expr = eletand (bindings phi, a.expr) in

  { a with expr; semvars; keywords }
