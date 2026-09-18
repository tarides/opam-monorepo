(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

include Map.Make(String)

(* [of_list] appears in OCaml 5.1. *)
let of_list bs =
  List.fold_left (fun m (k, v) -> add k v m) empty bs

let restrict domain m =
  filter (fun key _v -> StringSet.mem key domain) m

let domain m =
  fold (fun key _v accu -> StringSet.add key accu) m StringSet.empty

let support m =
  fold (fun key v accu ->
    StringSet.add key (StringSet.add v accu)
  ) m StringSet.empty

let multiple_add k v m =
  let vs =
    try
      find k m
    with Not_found ->
      []
  in
  add k (v :: vs) m
