(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module defines the abstract syntax of an "intermediate language",
   named IL, which is really just a fragment of OCaml. This fragment is
   tailored for the needs of Menhir and is not very general or pretty. *)

include BaseTypes

(**A variable. *)
type var =
  string

(**An interface. *)
type interface =
  interface_item list

(**An interface item. *)
and interface_item =
    (* A functor, always named [Make]. If the list of functor parameters is
       empty then the functor is omitted entirely; so [IIFunctor ([], i)]
       is considered equivalent to just [i]. *)
  | IIFunctor of string located list * interface
    (* Exception declarations. *)
  | IIExcDecls of excdef list
    (* Algebraic data type declarations. *)
  | IITypeDecls of [`Rec | `NonRec] * typedef list
    (* Value declarations. *)
  | IIValDecls of (string * typescheme) list
    (* Include directive. *)
  | IIInclude of module_type
    (* Submodule. *)
  | IIModule of string * module_type
    (* Comment. *)
  | IIComment of string
    (* Class. *)
  | IIClass of
      (* virtual? *)              virtuality *
      (* type parameters: *)      typeparams *
      (* class name: *)           class_name *
      (* self type annotation: *) typ option *
      (* content: *)              class_field_specs

(**A module type. *)
and module_type =
  | MTNamedModuleType of string
  | MTWithType of module_type * string list * string * with_kind * typ
  | MTSigEnd of interface

(**Two variants of [with type] annotations. *)
and with_kind =
  | WKNonDestructive (* = *)
  | WKDestructive   (* := *)

(**An exception definition. *)
and excdef = {

    excname: string;
    (**The name of this exception. *)

    exceq: string option;
    (**An optional equality. *)

    excparams: typ list;
    (**Parameters. *)

  }

(**A type definition. *)
and typedef = {

    typename: string;
    (**The name of this type. *)

    typeparams: typeparams;
    (**Type parameters. *)

    typerhs: typedefrhs;
    (**Data constructors. *)

    typeconstraint: (typ * typ) option
    (**An optional type equality constraint. *)

  }

(**The type parameters in a type definition. *)
and typeparams =
  typeparam list

(**A type parameter is a type variable name, without a leading quote.
   It can also be ["_"]. *)
and typeparam =
  string

(**The right-hand side of a type definition. *)
and typedefrhs =
  | TDefRecord of fielddef list
  | TDefSum of datadef list
  | TAbbrev of typ
  | TAbstract

(**A record field definition. *)
and fielddef = {

  modifiable: bool;
  (**Whether this field is mutable. *)

  fieldname: string;
  (**The name of this field. *)

  fieldtype: typescheme
  (**The type of this field. *)

}

(**A data constructor definition. *)
and datadef = {

  dataname: string;
  (**The name of this data constructor. *)

  datavalparams: typ list;
  (**The types of the value parameters. *)

  datatypeparams: typ list option;
  (**The instantiated type parameters that appear in the result type of this
     constructor, if this is a GADT; [None] if this is an ordinary ADT. *)

  comment: string option;
  (**A comment about this data constructor. *)

  unboxed: bool;
  (**An optional [@@unboxed] attribute. This attribute can be used only
     if there is a single data constructor and it carries a single field.
     This attribute is recognized by OCaml 4.04 and ignored by earlier
     versions of OCaml. *)

}

(**A type. *)
and typ =

  (* Textual OCaml type. *)
  | TypTextual of ocamltype

  (* Type variable, without its leading quote. Can also be "_". *)
  | TypVar of string

  (* Application of an algebraic data type constructor. *)
  | TypApp of string * typ list

  (* Anonymous tuple. *)
  | TypTuple of typ list

  (* Arrow type. *)
  | TypArrow of typ * typ

  (* Type sharing construct. *)
  | TypAs of typ * string

(**A type scheme. *)
and typescheme = {

  quantifiers: string list;
  (**Universal quantifiers, without leading quotes. *)

  locally_abstract: bool;
  (**Whether the quantifiers are locally abstract. An OCaml locally abstract
     type is bound by [type a] and referred to as [a]. Such a reference is
     internally represented as a [TypApp] node. An ordinary type variable is
     bound by ['a] and referred to as ['a]. Such a reference is internally
     represented as a [TypVar] node. *)

  body: typ;
  (**Body. *)

}

(**A value definition. *)
and valdef = {

  attribute: string;
  (**An optional attribute, like "[@inline]". *)

  valpat: pattern;
  (**The left-hand side of the definition (a pattern). *)

  valval: expr
  (**The right-hand side of the definition (an expression). *)

}

and valdefs =
  valdef list

(**An expression. *)
and expr =

  (* Variable. *)
  | EVar of var

  (* Function. *)
  | EFun of pattern list * expr

  (* Function call. *)
  | EApp of expr * expr list

  (* Method call. *)
  | EMethodCall of expr * method_name

  (* Local definitions. This is a nested sequence of [let] definitions. *)
  | ELet of (pattern * expr) list * expr

  (* Case analysis. *)
  | EMatch of expr * branch list
  | EIfThen of expr * expr
  | EIfThenElse of expr * expr * expr

  (* An expression that claims to be statically dead code. *)
  | EDead

  (* A divergent expression, which has every type. *)
  | EBottom

  (* Raising an exception. *)
  | ERaise of expr

  (* Catching an exception. *)
  | ETry of expr * branch list

  (* Data construction. Tuples of length 1 are considered nonexistent,
     that is, [ETuple [e]] is considered the same expression as [e]. *)

  | EUnit
  | EIntConst of int
  | EStringConst of string
  | EData of string * expr list
  | ETuple of expr list

  (* Type annotation. *)
  | EAnnot of expr * typescheme
      (* We allow a type scheme, with quantifiers, but our printer supports
         this only at the top level of a definition: [let p = (e : σ)]. Such
         a definition is printed under the form [let p : σ = e]. To work
         around this limitation, use the smart constructor [poly]. *)
      (* Furthermore, due to a bug in OCaml 4.07-4.10, our printer supports
         this only in the case where [σ] has locally abstract quantifiers. *)

  (* Cheating on the typechecker. *)
  | EMagic of expr (* Obj.magic *)
  | ERepr of expr  (* Obj.repr *)

  (* Records. *)
  | ERecord of (string * expr) list
  | ERecordAccess of expr * string
  | ERecordWrite of expr * string * expr

  (* Textual OCaml code. *)
  | ETextual of string located

  (* Comments. *)
  | EComment of string * expr

  (* Arrays. *)
  | EArray of expr list
  | EArrayAccess of expr * expr

(**A branch in a case analysis. *)
and branch = {

  branchpat: pattern;
  (**The branch's pattern. *)

  branchbody: expr;
  (**The branch's body. *)

}

(**A pattern. *)
and pattern =

  (* Wildcard. *)
  | PWildcard

  (* Variable. *)
  | PVar of var
  | PVarLocated of var located
      (* The positions must not be dummies. Use [pvarlocated]. *)

  (* Data deconstruction. Tuples of length 1 are considered nonexistent,
     that is, [PTuple [p]] is considered the same pattern as [p]. *)
  | PUnit
  | PIntConst of int
  | PData of string * pattern list
  | PTuple of pattern list
  | PRecord of (string * pattern) list

  (* Disjunction. *)
  | POr of pattern list

  (* Type annotation. *)
  | PAnnot of pattern * typ

(**A module expression. *)
and modexpr =
  | MVar of string
  | MStruct of structure
  | MApp of modexpr * modexpr

(**A program is a structure. *)
and program =
    structure

(**A structure. *)
and structure =
    structure_item list

(**A structure item.*)
and structure_item =
    (* Functor. Called [Make]. No functor if no parameters. Very ad hoc! *)
  | SIFunctor of string located list * structure
    (* Exception definitions. *)
  | SIExcDefs of excdef list
    (* Algebraic data type definitions (mutually recursive). *)
  | SITypeDefs of [`Rec | `NonRec] * typedef list
    (* Value definitions (mutually recursive or not, as per the flag). *)
  | SIValDefs of bool * valdefs
    (* Raw OCaml code. *)
  | SIFragment of string located list
    (* Sub-module definition. *)
  | SIModuleDef of string * modexpr
    (* Module inclusion. *)
  | SIInclude of modexpr
    (* Comment. *)
  | SIComment of string
    (* Toplevel attribute. *)
  | SIAttribute of (* attribute: *) string * (* payload: *) string
    (* Class. *)
  | SIClass of
      (* virtual? *)              virtuality *
      (* type parameters: *)      typeparams *
      (* class name: *)           class_name *
      (* self type annotation: *) typ option *
      (* content: *)              class_fields

(**An optional [virtual] flag. *)
and virtuality =
  | NonVirtual
  | Virtual

(**An optional [private] flag. *)
and privacy =
  | Public
  | Private

(**Class fields in a class definition. *)
and class_fields =
  class_field list

(**A class field in a class definition. *)
and class_field =
    (* Method definition. *)
  | CFMethod of privacy * method_name * expr
    (* Virtual method declaration. *)
  | CFMethodVirtual of method_name * typescheme
    (* Comment. *)
  | CFComment of string

(**Class fields in a class type. *)
and class_field_specs =
  class_field_spec list

(**A class field in a class type. *)
and class_field_spec =
    (* Method declaration. *)
  | CFSMethod of virtuality * method_name * typescheme
    (* Comment. *)
  | CFSComment of string

(**A class name. *)
and class_name =
  string

(**A method name. *)
and method_name =
  string
