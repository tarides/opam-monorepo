(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module provides a number of constructor functions that help produce
   [IL] code. *)

open IL

(* -------------------------------------------------------------------------- *)

(* Types. *)

(**[tname] converts a type name to a type (applied to zero parameters). *)
val tname: string -> typ

(***The type [unit]. *)
val tunit: typ

(***The type [int]. *)
val tint: typ

(***The type [string]. *)
val tstring: typ

(***The type [position]. *)
val tposition: typ

(***The type [location] is the type of the keyword [$loc].
    It is currently defined as [position * position], but
    this might change in the future. *)
val tlocation: typ

(***The type [lexbuf]. *)
val tlexbuf: typ

(***The type [Obj.t]. *)
val tobj: typ

(***The type [option]. *)
val toption: typ -> typ

(***The type [list]. *)
val tlist: typ -> typ

(**A type variable. [tvar] is synonymous with [TypVar]. *)
val tvar: string -> typ

(**A function type. *)
val arrow: typ -> typ -> typ

(**A multi-argument function type. *)
val marrow: typ list -> typ -> typ

(**[scheme qs ty] builds a type scheme out of the quantifiers [qs] and
   body [ty]. *)
val scheme: string list -> typ -> typescheme

(**[scheme qs ty] builds a type scheme out of the quantifiers [qs] and
   body [ty]. The quantifiers are printed using the syntax [type a]. *)
val local_scheme: string list -> typ -> typescheme

(**[type2scheme] converts a type to a type scheme (with no quantifiers). *)
val type2scheme: typ -> typescheme

(* -------------------------------------------------------------------------- *)

(* Expressions. *)

(**A variable as an expression. *)
val evar: var -> expr

(**A list of variables as expressions. *)
val evars: var list -> expr list

(**A function of several parameters.
   If [ps] is empty then [efun ps e] is just [e]. *)
val efun: pattern list -> expr -> expr

(**A [match] construct.*)
val ematch: expr -> branch list -> expr

(**A use of [Obj.magic]. *)
val emagic: expr -> expr

(**A function application. *)
val eapp: expr -> expr list -> expr

(**This smart constructor constructs a tuple expression. A tuple of length
   zero is changed to [EUnit]. A tuple of length 1 is erased. *)
val etuple: expr list -> expr

(**[annotate ty e] annotates the expression [e] with the monomorphic
   type [ty]. Certain simplifications are performed on the fly. *)
val annotate: typ -> expr -> expr

(**[poly sigma e] annotates the expression [e] with the type scheme [sigma].
   Currently, due to a bug in OCaml 4.07-4.10, our printer supports this only
   in the case where [sigma] has locally abstract quantifiers. *)
val poly: typescheme -> expr -> expr

(**[blet pes e] constructs a nested sequence of [let] definitions.
   Certain simplifications are performed on the fly. *)
val blet: (pattern * expr) list -> expr -> expr

(**[mlet ps es e] constructs a nested sequence of [let] definitions.
   Certain simplifications are performed on the fly.
   It is like [blet], but takes separate lists of patterns and expressions. *)
val mlet: pattern list -> expr list -> expr -> expr

(**[eletand pes e] constructs a parallel [let] definition,
   that is, a [let/and] construct.
   This construct is not part of the syntax of IL,
   so it is simulated using tuples. *)
val eletand: (pattern * expr) list * expr -> expr

(**[non_exhaustive_let pat e1 e2] constructs a [let] definition where the
   pattern [pat] is not exhaustive. In order to avoid a warning, a [match]
   construct is used, whose second branch contains [assert false]. *)
val non_exhaustive_let: pattern -> expr -> expr -> expr

(**[eraisenotfound] is an expression that raises [Not_found]. *)
val eraisenotfound: expr

(**[eassert e] is the runtime assertion [assert e]. *)
val eassert: expr -> expr

(**[eassertfalse] is the runtime assertion [assert false]. *)
val eassertfalse: expr

(**[eprintf format args] constructs a call to [Printf.eprintf], which
   log a tracing message onto [stderr]. *)
val eprintf: string -> expr list -> expr

(**[ecomment c e] annotates the expression [e] with the comment [c]. *)
val ecomment: string -> expr -> expr

(**[bcast x ty] is a binding [x = (Obj.magic x : ty)].
   It imposes the type [ty] on the variable [x]. *)
val bcast: var -> typ -> pattern * expr

(* -------------------------------------------------------------------------- *)

(* Patterns and branches. *)

(** A variable as a pattern. *)
val pvar: var -> pattern

(** A list of variables as patterns. *)
val pvars: var list -> pattern list

(**An integer literal pattern. *)
val pint: int -> pattern

(**This smart constructor for [PVarLocated] deals with the situation where
   we have a dummy range. *)
val pvarlocated: var located -> pattern

(** Branches. *)
val branch: pattern -> expr -> branch

(* -------------------------------------------------------------------------- *)

(* Interfaces and structures. *)

(**[interface_to_structure] converts an interface to a structure. Only
   exception and type definitions go through. The rest is filtered away. *)
val interface_to_structure: interface -> structure

(**[with_types] constructs a named module type together with a list of
   [with type] constraints. *)
val with_types:
  with_kind -> string -> (string list * string * typ) list -> module_type

(**[mapp] constructs a multiple functor application. *)
val mapp: modexpr -> modexpr list -> modexpr

(**[pdef] constructs a value definition. The left-hand side is a pattern. *)
val pdef: pattern -> expr -> valdef

(**[def] constructs a value definition. *)
val def: var -> expr -> valdef

(**[def_inline] constructs a value definition, annotated with [@inline]. *)
val def_inline: var -> expr -> valdef

(**[valdef] turns a value definition into a structure item.
   This yields a single non-recursive value definition. *)
val valdef: valdef -> structure_item

(**[valdefs] turns a list of value definitions into a list of structure items.
   This yields a sequence of non-recursive value definitions. *)
val valdefs: valdef list -> structure_item list
