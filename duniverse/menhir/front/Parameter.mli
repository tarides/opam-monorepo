(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers helper functions to deal with the type [parameter].

   Because the constructor [ParamAnonynous] is eliminated very early on, most
   of these functions cannot deal with this constructor. The only functions
   that can deal with it are {!apply}, {!position}, and {!locate}. *)

open Syntax

type t = parameter

(**[var] converts a symbol to a parameter. A dummy location is used. *)
val var : symbol -> parameter

(**[apply s ps] constructs an application of the symbol [s] to the
   parameters [ps]. The list [ps] can be empty. This smart constructor
   should be preferred to a direct use of the constructor [ParamApp],
   which does not accept an empty list. *)
val apply : symbol located -> parameters -> parameter

(**[destruct p] destructs the parameter [p] into an application of a symbol
   [s] to a list of parameters [ps]. *)
val destruct : parameter -> symbol located * parameters

(**[head p] destructs the parameter [p] into a symbol [s] applied to an empty
   list of parameters. If the list of parameters is not empty then it fails. *)
val head : parameter -> symbol located

(**[iter f p] applies the action [f] to every symbol that appears in the
   parameter [p]. *)
val iter : (symbol located -> unit) -> parameter -> unit

(**[map f p] applies the transformation [f] to every symbol that appears in
   the parameter [p]. *)
val map : (symbol located -> symbol located) -> parameter -> parameter

(**[subst f p] applies the substitution [f] to the parameter [p]. The
   substitution [f] must be sort-preserving; in particular, a variable of
   higher sort must be mapped to a variable of the same sort. *)
val subst : (symbol located -> parameter) -> parameter -> parameter

(**[occurs s p] tests whether the symbol [s] appears in the parameter [p]. *)
val occurs : symbol -> parameter -> bool

(**[equal] tests whether two parameters are equal.
   The position information is ignored. *)
val equal : parameter -> parameter -> bool

(**[hash] hashes a parameter.
   The position information is ignored.
   [hash] is compatible with {!equal}. *)
val hash : parameter -> int

(**[position p] returns the position of the parameter [p]. *)
val position : parameter -> range

(**[locate] transforms a parameter into a located parameter
   using the position [position p]. *)
val locate : parameter -> parameter located

(**[print sep p] converts the parameter [p] into a string. The separator
   [sep] is used to separate the actual parameters in an application. It
   should typically be a comma, possibly with a space. *)
val print : string -> parameter -> string
