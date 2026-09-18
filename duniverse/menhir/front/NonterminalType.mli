(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module deals with the definition of the type [nonterminal], which
   describes the nonterminal symbols. It is a generalized algebraic data type
   (GADT). It has one type parameter. Its data constructors carry zero value
   arguments. *)

open PlainSyntax
open IL

(**[tcnonterminalgadt] is the name of the type [nonterminal]. *)
val tcnonterminalgadt: string

(**[tnonterminalgadt ty] constructs an application of the type constructor
   [nonterminal] to the type [ty]. *)
val tnonterminalgadt: typ -> typ

(**[tnonterminalgadtdata nt] is the name of the data constructor that
   represents the nonterminal symbol [nt]. *)
val tnonterminalgadtdata: string -> string

(**[nonterminalgadtdef grammar] is the definition of the type [nonterminal].
   This definition can be correctly constructed only if the OCaml type of
   every nonterminal symbol in the grammar [grammar] is known. If this is not
   the case then [nonterminalgadtdef grammar] produces a mock definition of
   the type [nonterminal] as an abstract type. *)
val nonterminalgadtdef: grammar -> interface

(* [nonterminalgadtdef] is normally called only by the table back-end and
   only when [Settings.inspection] is [true]. *)

(* As an exception, in [--(raw-)depend] mode, we need to produce a mock [.mli]
   file before [--infer] has run. Then [nonterminalgadtdef] is called in a
   situation where some nonterminal symbols have unknown OCaml types. In this
   case, a mock definition of the type [nonterminal] is produced, so the mock
   [.mli] file is a subset of the final [.mli] file. It is unclear whether
   this can create problems; possibly, it can. Anyway, the modern Dune-based
   workflow does not use [--(raw)-depend]. *)
