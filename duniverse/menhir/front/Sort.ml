(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

type sort =
  | Arrow of sort list

let star =
  Arrow []

let[@inline] arrow sorts =
  Arrow sorts

let[@inline] domain (Arrow sorts) =
  sorts

type sorts =
  sort StringMap.t
