(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

type range =
  Range.range

type ranges =
  Range.ranges

type 'a located =
  'a Located.located

type ocamltype = OCamlType.ocamltype =
  | Declared of string located
  | Inferred of string
