(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This is the abstract syntax for a plain grammar, that is, a grammar that
   does not have any parameterized nonterminal symbols. *)

(* A plain grammar is obtained as the result of an expansion phase, which
   takes place in several steps, implemented in [SelectiveExpansion] and
   [Drop]. *)

include FrontTypes

(* ------------------------------------------------------------------------ *)

(* In a plain grammar, %attribute declarations can be desugared away. This is
   also done by [Drop].

   Thus, in a plain grammar, attributes can be attached in the following
   places:

   - with the grammar:          field [gr_attributes] of [grammar]
   - with a terminal symbol:    field [tk_attributes] of [properties]
   - with a nonterminal symbol: field [attributes] of [rule]
   - with a producer:           field [prod_attributes] of [producer]
   - with a branch:             field [br_attributes] of [branch]           *)

(* ------------------------------------------------------------------------ *)

(**A producer is a pair of a (located) identifier and a symbol. In concrete
   syntax, it could be [e = expr], for instance. It carries a number of
   attributes. 2025/11/19: we allow a producer to be marked %inlined, which
   means that the definition of the symbol [prod_symbol] should be inlined
   into this particular use site. There is currently no surface syntax for
   this feature; it is used internally. *)
type producer =
  {
    prod_id         : identifier located;
    prod_symbol     : symbol;
    prod_attributes : attributes;
    prod_inlined    : bool;
  }

type producers =
  producer list

(* ------------------------------------------------------------------------ *)

(**A branch contains a series of producers and a semantic action. It is the
   same as in the surface syntax; see [Syntax]. *)
type branch =
  {
    branch_position  : range;
    producers        : producers;
    action           : action;
    prec_annotation  : prec_annotation;
    production_level : production_level;
    br_attributes    : attributes;
  }

type branches =
  branch list

(* ------------------------------------------------------------------------ *)

(**A rule consists mainly of several branches. In contrast with the surface
   syntax, it has no parameters.

   The [%inline] flag is no longer relevant after %inline symbols have been
   eliminated. *)
type rule =
  {
    branches    : branches;
    positions   : range list;
    inline_flag : bool;
    attributes  : attributes;
    merge       : merge_fun option;
  }

(* ------------------------------------------------------------------------ *)

(**A grammar is essentially the same as in the surface syntax; see [Syntax].
   The main difference is that [%attribute] declarations, represented by
   the field [p_symbol_attributes] in the surface syntax, have disappeared. *)
type grammar =
  {
    preludes        : string located list;
    postludes       : string located list;
    parameters      : string located list;
    start_symbols   : StringSet.t;
    types           : ocamltype StringMap.t;
    tokens          : properties StringMap.t;
    on_error_reduce : on_error_reduce_level StringMap.t;
    gr_attributes   : attributes;
    rules           : rule StringMap.t;
    default_merge   : merge_fun located option;
  }
