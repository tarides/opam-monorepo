(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open PlainSyntax

let check c g =
  g.rules |> StringMap.iter @@ fun _ rule ->
  if rule.inline_flag then
    Report.error c rule.positions
      "%%inline is not supported by the Rocq back-end."
