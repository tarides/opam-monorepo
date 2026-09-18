(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers a printer for grammars. *)

open PlainSyntax

type mode =

  | PrintNormal
    (**[PrintNormal] is the normal mode: the result is a grammar that obeys
       the concrete syntax of [.mly] files. *)

  | PrintForOCamlyacc
    (**[PrintForOCamlyacc] attempts to produce ocamlyacc-compatible output.
       This means, in particular, that we cannot bind identifiers to semantic
       values, but must use [$i] instead. *)

  | PrintUnitActions of bool
    (**[PrintUnitActions _] causes all OCaml code to be suppressed. All
       semantic actions are replaced with trivial semantic actions that
       contain just a unit value. All bindings of semantic values are
       suppressed. All preludes and postludes disappear. All %parameter
       declarations disappear. Every %type declaration carries the [unit]
       type.

       With [PrintUnitActions true], in addition, every token is declared to
       carry a semantic value of type [unit]. *)

(**[print mode f g] prints the grammar [g]. All output is sent to the output
   channel [f]. *)
val print: mode -> out_channel -> grammar -> unit
