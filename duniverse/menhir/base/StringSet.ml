(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

include Set.Make(String)

let print s =
  MString.separated_iter (fun s -> s) ", " (fun f -> iter f s)

let big_union ss =
  List.fold_left union empty ss
