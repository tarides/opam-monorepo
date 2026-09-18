(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf

type mode =
  [`Normal | `WarningIsSignal | `SignalIsWarning]

type channel =
| N
| C of {
    out_channel: out_channel;
    mode: mode;
    mutable count: int
  }

let warning_is_signal c =
  match c with C { mode = `WarningIsSignal; _ } -> true | _ -> false

let signal_is_warning c =
  match c with C { mode = `SignalIsWarning; _ } -> true | _ -> false

let null =
  N

let live c =
  match c with N -> false | C _ -> true

let create mode out_channel =
  let count = 0 in
  C { out_channel; mode; count }

let[@inline] count c =
  match c with N -> 0 | C c ->
  c.count

let flush c =
  match c with N -> () | C c ->
  flush c.out_channel

let exit_if c =
  flush c;
  if count c > 0 then exit 1

(* It might seem tempting to make [mode] an optional argument with the default
   value [`Normal], so as to let the caller omit this argument when it is
   [`Normal]. Unfortunately, then, the syntax [monitor @@ fun c -> ...] works
   only with OCaml 4.13 or newer, where [@@] is type-checked just like an
   ordinary function application. With older versions, a type error arises. *)

let monitor mode action =
  let c = create mode stderr in
  match action c with
  | result ->
      exit_if c;
      result
  | exception e ->
      exit_if c;
      raise e

let display c delta continuation header positions format =
  let format = header ^^ format ^^ "\n%!" in
  match c with
  | N ->
      let dummy = stderr in (* uncanny *)
      ikfprintf continuation dummy format
  | C c ->
      c.count <- c.count + delta;
      let f = c.out_channel in
      Range.fprint f positions;
      kfprintf continuation f format

let die _f =
  exit 1

let ret _f =
  ()

let error c positions format =
  let delta = 1 in
  display c delta die "Error: " positions format

let signal c positions format =
  let delta = 1 in
  display c delta ret "Error: " positions format

let warning c positions format =
  let delta = 0 in
  display c delta ret "Warning: " positions format

let signal c =
  if signal_is_warning c then warning c else signal c
and warning c =
  if warning_is_signal c then signal c else warning c

let note c positions format =
  let delta = 0 in
  display c delta ret "Note: " positions format

let log c format =
  let delta = 0
  and positions = [] in
  display c delta ret "" positions format

module Just = struct

  let error positions format =
    let c = create `Normal stderr in
    error c positions format

  let warning positions format =
    let c = create `Normal stderr in
    warning c positions format

end
