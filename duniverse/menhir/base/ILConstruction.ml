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
open IL

(* -------------------------------------------------------------------------- *)

(* Types. *)

let tname name =
  TypApp (name, [])

let tunit =
  tname "unit"

let tint =
  tname "int"

let tstring =
  tname "string"

let tposition =
  tname "Lexing.position"

let tpair typ1 typ2 =
  TypTuple [typ1; typ2]

let tlocation =
  tpair tposition tposition

let tlexbuf =
  tname "Lexing.lexbuf"

let tobj =
  tname "Obj.t"

let toption ty =
  TypApp ("option", [ty])

let tlist ty =
  TypApp ("list", [ty])

let tvar x : typ =
  TypVar x

let arrow typ body : typ =
  TypArrow (typ, body)

let marrow typs body : typ =
  List.fold_right arrow typs body

let scheme quantifiers body =
  let locally_abstract = false in
  { quantifiers; body; locally_abstract }

let local_scheme quantifiers body =
  let locally_abstract = true in
  { quantifiers; body; locally_abstract }

let type2scheme t =
  scheme [] t

(* -------------------------------------------------------------------------- *)

(* Expressions. *)

let evar s =
  EVar s

let evars xs =
  List.map evar xs

let efun ps e =
  match ps with
  | [] ->
      e
  | _ ->
      EFun (ps, e)

let ematch e bs =
  EMatch (e, bs)

let emagic e =
  EMagic e

let eapp e es =
  EApp (e, es)

let etuple = function
  | [] ->
      EUnit
  | [ e ] ->
      e
  | es ->
      ETuple es

(* In [annotate], the transformations that we perform on the fly are
   intended to improve readability. *)

let rec annotate ty e =
  match e with
  | EComment (c, e) ->
      EComment (c, annotate ty e)
  | ELet (bs, e) ->
      ELet (bs, annotate ty e)
  | _ ->
      EAnnot (e, type2scheme ty)

let poly sigma e =
  match sigma.quantifiers with
  | [] ->
      (* If the type scheme [σ] has no quantifiers,
         produce an ordinary type annotation. *)
      annotate sigma.body e
  | _ ->
      (* If the type scheme [σ] has no quantifiers,
         encode [e : σ] as [let x : σ = e in x]. *)
      let x = "__menhir_poly" in
      ELet ([PVar x, EAnnot (e, sigma)], EVar x)

(* [simplify] removes bindings of the form [let v = v in ...] and
   [let _ = v in ...]. *)

let rec simplify = function
  | [] ->
      []
  | (PVar v1, EVar v2) :: bindings when v1 = v2 ->
      (* Avoid a useless let binding. *)
      simplify bindings
  | (PWildcard, EVar _) :: bindings ->
      (* Avoid a useless let binding. *)
      simplify bindings
  | binding :: bindings ->
      binding :: simplify bindings

let rec blet bindings body =
  let bindings = simplify bindings in
  match bindings, body with
  | [], _ ->
      body
  | [ PVar x1, e ], EVar x2 when x1 = x2 ->
      (* Reduce [let x = e in x] to just [e]. *)
      e
  | (PUnit, EUnit) :: bindings, _ ->
      (* Reduce [let () = () in e] to just [e]. *)
      blet bindings body
  | _, _ ->
      ELet (bindings, body)

let mlet formals actuals body =
  blet (List.combine formals actuals) body

let eletand (bindings, body) =
  let bindings = simplify bindings in
  match bindings, body with
  | [], _ ->
      (* special case: zero bindings *)
      body
  | [ PVar x1, e ], EVar x2 when x1 = x2 ->
      (* Reduce [let x = e in x] to just [e]. *)
      e
  | [ _ ], _ ->
      (* special case: one binding *)
      ELet (bindings, body)
  | _ :: _ :: _, _ ->
      (* general case: at least two bindings *)
      let pats, exprs = List.split bindings in
      ELet ([ PTuple pats, ETuple exprs ], body)

let eraisenotfound =
  ERaise (EData ("Not_found", []))

let eassert e =
  EApp (EVar "assert", [ e ])

let efalse : expr =
  EData ("false", [])

let eassertfalse =
  eassert efalse

let eprintf format args =
  EApp (
    EVar "Printf.eprintf",
    (EStringConst (format ^ "\n%!")) ::
    args
  )

let ecomment c e =
  EComment (c, e)

let bcast x ty =
  (PVar x, annotate ty (EMagic (EVar x)))

let non_exhaustive_let pat e1 e2 =
  let branch1 = { branchpat = pat; branchbody = e2 }
  and branch2 = { branchpat = PWildcard; branchbody = eassertfalse } in
  EMatch (e1, [ branch1; branch2 ])

(* -------------------------------------------------------------------------- *)

(* Patterns and branches. *)

let pvar x =
  PVar x

let pvars xs =
  List.map pvar xs

let pint k : pattern =
  PData (string_of_int k, [])

let pvarlocated id =
  let range = position id in
  if Range.is_dummy range then
    let x = value id in
    PVar x
  else
    PVarLocated id

let branch branchpat branchbody =
  { branchpat; branchbody }

(* -------------------------------------------------------------------------- *)

(* Interfaces and structures. *)

let rec interface_item_to_structure_item = function
  | IIExcDecls defs ->
      [ SIExcDefs defs ]
  | IITypeDecls (flag, defs) ->
      [ SITypeDefs (flag, defs) ]
  | IIModule (name, MTSigEnd intf) ->
      [ SIModuleDef (name, MStruct (interface_to_structure intf)) ]
  | IIFunctor (_, _)
  | IIValDecls _
  | IIInclude _
  | IIModule (_, _)
  | IIComment _
  | IIClass _ ->
      []

and interface_to_structure i =
  List.flatten (List.map interface_item_to_structure_item i)

let with_types wk name tys =
  List.fold_left (fun mt (params, name, ty) ->
    MTWithType (mt, params, name, wk, ty)
  ) (MTNamedModuleType name) tys

let mapp1 me1 me2 =
  MApp (me1, me2)

let mapp me1 mes2 =
  List.fold_left mapp1 me1 mes2

let idef valpat valval attribute =
  { valpat; valval; attribute }

let pdef p e =
  idef p e ""

let def x e =
  idef (PVar x) e ""

let def_inline x e =
  idef (PVar x) e "[@inline]"

let valdef def =
  SIValDefs (false, [ def ])

let valdefs defs =
  List.map valdef defs
