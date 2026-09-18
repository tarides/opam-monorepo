(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let[@inline] preincrement r =
  let x = !r in
  let x' = x + 1 in
  r := x';
  x'

let[@inline] postincrement r =
  let x = !r in
  r := x + 1;
  x

let[@inline] predecrement r =
  let x = !r in
  let x' = x - 1 in
  r := x';
  x'

let[@inline] postdecrement r =
  let x = !r in
  r := x - 1;
  x

let mkgensym () =
  let r = ref 0 in
  fun () ->
    postincrement r

let sum n (f : int -> int) : int =
  let sum = ref 0 in
  for x = 0 to n - 1 do
    sum := !sum + f x
  done;
  !sum

let[@inline] with_counter f =
  let c = ref 0 in
  f c;
  !c

let[@tail_mod_cons] rec interval i j =
  if i < j then i :: interval (i+1) j else []

let[@inline] iterij i j f =
  for x = i to j - 1 do
    f x
  done

let[@inline] iteri n f =
  iterij 0 n f

let[@inline] foldij i j f accu =
  let accu = ref accu in
  for x = i to j-1 do
    accu := f x !accu
  done;
  !accu

let[@inline] foldi j f accu =
  foldij 0 j f accu

let rec foldij_lazy i j f accu =
  if i < j then
    f i (fun () -> foldij_lazy (i + 1) j f accu)
  else
    accu

let[@inline] mapij i j f =
  foldij i j (fun x accu -> f x :: accu) []
  |> List.rev

let[@inline] mapi n f =
  mapij 0 n f

let[@inline] initij i j f =
  if j <= i then
    [||]
  else
    Array.init (j - i) (fun k -> f (i + k))

let[@inline] initi n f =
  if n <= 0 then
    [||]
  else
    Array.init n f
