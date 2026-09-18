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
open PlainSyntax
open PlainSyntaxAccessors
open MiddleAPI

module Make
(F : sig
  val grammar : grammar
end)
(G : GRAMMAR)
= struct
open F
open G

(* -------------------------------------------------------------------------- *)

(* A "tick" is a suspended action whose effect (once it is evaluated) is to
   mark a declaration or annotation as useful. *)

type tick =
  unit Lazy.t

let force (tick : tick) =
  Lazy.force tick

let combine (tick1 : tick) (tick2 : tick) : tick =
  lazy (force tick1; force tick2)

(* -------------------------------------------------------------------------- *)

(* This submodule deals with precedence information for tokens, including
   pseudo-tokens. Therefore it is not based on the type [Terminal.t]; it
   uses the type [string] to represent both tokens and pseudo-tokens. *)

module Tok = struct

  (* The set [ever_useful] records the tokens whose precedence level has been
     useful (at least once). This allows emitting warnings about useless
     precedence declarations. *)

  let ever_useful : StringSet.t ref =
    ref StringSet.empty

  (* [use tok] marks the token [tok] as useful. *)

  let use (tok : terminal) =
    ever_useful := StringSet.add tok !ever_useful

  (* [level tok] is invoked when someone wants to consult the precedence level
     of the token [tok]. Yet, this does not imply that this level is useful.
     Indeed, if this level is subsequently compared with [UndefinedPrecedence]
     then this will not allow solving a conflict. So, in addition to a level,
     we return a tick which, when evaluated, records that this precedence
     level was useful. *)

  (* 2025/10/19: Although this was never intended, it is possible to assign
     a priority level to the token [error], and some users have done this.
     This causes a pseudo-token named [error] to appear in [grammar.tokens].
     In this case, the precedence level of this pseudo-token must be used.
     If there is no pseudo-token named [error], then the precedence level
     of the [error] token is undefined. *)

  let level (tok : terminal) : tick * precedence_level =
    match StringMap.find tok grammar.tokens with
    | properties ->
        lazy (use tok), properties.tk_precedence
    | exception Not_found ->
        assert (tok = "error");
        lazy (), UndefinedPrecedence

  (* [level'] is analogous to [level] but expects a terminal symbol. *)

  let level' (t : Terminal.t) =
    level (Terminal.print t)

  (* [diagnostics c] prints warnings about useless precedence declarations
     for terminal symbols (%left, %right, %nonassoc). It should be invoked
     after the automaton has been constructed. *)

  let diagnostics c =
    (* Iterate over all tokens and pseudo-tokens. *)
    grammar.tokens |> StringMap.iter @@ fun tok properties ->
    if not (StringSet.mem tok !ever_useful) then
      match properties.tk_precedence with
      | UndefinedPrecedence ->
          ()
      | PrecedenceLevel fl ->
          Report.warning c [position fl]
            "the precedence level assigned to %s is never useful." tok

end (* Tok *)

(* -------------------------------------------------------------------------- *)

(* This submodule deals with precedence information for productions. *)

module Prod = struct

  (* The array [ever_useful] records, for each %prec annotation, whether this
     annotation is ever used. This allows us to emit a warning about useless
     %prec annotations. *)

  (* 2015/10/06: We take into account the fact that a %prec annotation can be
     duplicated by inlining or by the expansion of parameterized non-terminal
     symbols. Our table is not indexed by productions, but by positions (of
     %prec annotations in the source). Thus, if a %prec annotation is
     duplicated, at least one of its copies should be found useful for the
     warning to be suppressed. *)

  let ever_useful : (range, unit) Hashtbl.t =
    (* assuming that generic hashing and equality on positions are OK *)
    Hashtbl.create 16

  (* [prec_decl prod] returns the optional precedence annotation associated
     with the production [prod]. It also returns a tick which marks this
     annotation as useful. *)

  let prec_decl prod : tick * prec_annotation =
    let osym = Production.prec_decl prod in
    lazy (
      osym |> Option.iter @@ fun sym ->
      (* Mark this %prec annotation as useful. *)
      let pos = position sym in
      Hashtbl.add ever_useful pos ()
    ),
    osym

  (* [scan prod] finds the rightmost terminal symbol in the right-hand side of
     the production [prod]. It is used in the case where this production
     carries no %prec annotation. The precedence level of the production is
     then the precedence level of the rightmost terminal symbol. *)

  let scan prod : Terminal.t option =
    Array.fold_left (fun accu symbol ->
      match symbol with
      | Symbol.T tok ->
          Some tok
      | Symbol.N _ ->
          accu
    ) None (Production.rhs prod)

  (* [precedence prod] computes the precedence level of the production [prod].
     It also returns a tick which marks the information that it has used as
     useful. *)

  let precedence prod : tick * precedence_level =
    let tick1, prec_decl = prec_decl prod in
    match prec_decl with
    | Some terminal ->
        let tok = value terminal in
        let tick2, level = Tok.level tok in
        combine tick1 tick2, level
    | None ->
        match scan prod with
        | None ->
            tick1, UndefinedPrecedence
        | Some tok ->
            let tick2, level = Tok.level' tok in
            combine tick1 tick2, level

  (* [diagnostics c] prints warnings about useless precedence annotations for
     productions (%prec). It should be invoked after the automaton has been
     constructed. *)

  let diagnostics c =
    Production.iterx @@ fun prod ->
    let osym = Production.prec_decl prod in
    osym |> Option.iter @@ fun sym ->
    (* Check whether this %prec annotation was useless. *)
    let pos = position sym in
    if not (Hashtbl.mem ever_useful pos) then begin
      Report.warning c [pos] "this %%prec declaration is never useful.";
      (* Hack: avoid two warnings at the same position. *)
      Hashtbl.add ever_useful pos ()
    end

end (* Prod *)

(* -------------------------------------------------------------------------- *)

(* Resolving shift/reduce conflicts. *)

type choice =
  | ChooseShift
  | ChooseReduce
  | ChooseNeither
  | DontKnow

let tick_unless_incomparable tick (o : order) =
  match o with
  | Incomparable ->
      (* Our information is inconclusive. Do not evaluate [tick]:
         that is, do not record that this information was useful. *)
      ()
  | Eq | Lt | Gt ->
      force tick

let shift_reduce tok prod : choice =
  let tick1, tokp  = Tok.level' tok
  and tick2, prodp = Prod.precedence prod in
  let o = precedence_order tokp prodp in
  tick_unless_incomparable (combine tick1 tick2) o;
  match o with
  | Incomparable ->
      DontKnow
  | Lt ->
      ChooseReduce
  | Gt ->
      ChooseShift
  | Eq ->
      match Terminal.associativity tok with
      | LeftAssoc  -> ChooseReduce
      | RightAssoc -> ChooseShift
      | NonAssoc   -> ChooseNeither
      | _          -> assert false
                      (* If [tok]'s precedence level is defined, then
                         its associativity must be defined as well. *)

(* -------------------------------------------------------------------------- *)

(* Resolving reduce/reduce conflicts. *)

let reduce_reduce prod1 prod2 : Production.t option =
  let p1 = Production.production_level prod1
  and p2 = Production.production_level prod2 in
  match production_order p1 p2 with
  | Lt ->
      Some prod1
  | Gt ->
      Some prod2
  | Eq ->
      None
  | Incomparable ->
      None

(* -------------------------------------------------------------------------- *)

(* Combine the two [diagnostics] functions. *)

let diagnostics c =
  Tok.diagnostics c;
  Prod.diagnostics c

end (* Make *)
