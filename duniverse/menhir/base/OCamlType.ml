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

(**An OCaml type is a located string (that is, a string that was
   extracted out of a source file) or just a string (this is the
   case when this type has been inferred; see [Infer]). *)
type ocamltype =
  | Declared of string located
  | Inferred of string
