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

let b =
  Buffer.create 1024

let out =
  ref (None : out_channel option)

let set_output_channel oc =
  out := oc

let () =
  at_exit @@ fun () ->
  !out |> Option.iter @@ fun f ->
  fprintf f "%s" (Buffer.contents b);
  close_out f

let[@inline] now () =
  let open Unix in
  (times()).tms_utime

type start =
  float

let start () =
  match !out with None -> 0.0 | Some _ ->
  now()

let time msg (task : unit -> 'a) : 'a =
  match !out with None -> task() | Some _ ->
  let times1 = now() in
  let result = task() in
  let times2 = now() in
  let elapsed = times2 -. times1 in
  bprintf b "%s: %.02fs\n" msg elapsed;
  result

let stop times1 msg =
  match !out with None -> () | Some _ ->
  let times2 = now() in
  let elapsed = times2 -. times1 in
  bprintf b "%s: %.02fs\n" msg elapsed
