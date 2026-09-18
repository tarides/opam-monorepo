(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Channels

(* Construct a rich view of the grammar. *)

(* In GLR mode, request a topological numbering of the productions,
   which is used to efficiently determine in what order reductions
   should take place. *)

module G =
  GrammarConstruction.Make
    (Front)
    (struct let topological_numbering = Settings.enabled_GLR end)
    ()

include G

(* At log level 1, print short information messages. *)

let () =
  G.info (getG 1)

(* Emit some error and warnings messages. *)

let () =
  monitor @@ fun c ->
  G.check_start_symbol c;
  G.warn_empty_symbol c

(* At log level 2, display the results of the analyses NULLABLE,
   FIRST, MINIMAL, MAXIMAL, and FOLLOW. *)

let () =
  let c = getG 2 in
  if Report.live c then begin
    G.dump_nullable c;
    G.dump_first c;
    G.dump_minimal c;
    G.dump_maximal c;
    G.dump_follow c
  end

(* At log level 3, display FOLLOW sets for terminal symbols
   and symbolic FOLLOW sets. *)

let () =
  let c = getG 3 in
  if Report.live c then begin
    G.dump_tfollow c;
    G.dump_sfollow c
  end
