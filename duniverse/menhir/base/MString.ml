(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* -------------------------------------------------------------------------- *)

(* Buffering. *)

let with_buffer n f =
  let b = Buffer.create n in
  f b;
  Buffer.contents b

(* -------------------------------------------------------------------------- *)

(* Printing. *)

type 'a iter = ('a -> unit) -> unit

let this_then_that x y =
  let s = ref x in
  fun () ->
    let result = !s in
    s := y;
    result

let separated_iter print separator iter =
  let separator = this_then_that "" separator in
  with_buffer 32 @@ fun b ->
  let emit s = Buffer.add_string b s in
  iter @@ fun x ->
  separator() |> emit;
  print x |> emit

let separated_list print separator xs =
  let iter f = List.iter f xs in
  separated_iter print separator iter

let preceded_iter print del iter =
  with_buffer 32 @@ fun b ->
  let emit s = Buffer.add_string b s in
  iter @@ fun x ->
  del |> emit;
  print x |> emit

let preceded_list print del xs =
  let iter f = List.iter f xs in
  preceded_iter print del iter

let subarray_bullet print separator start dot bullet xs =
  let separator = this_then_that "" separator in
  with_buffer 32 @@ fun b ->
  let emit s = Buffer.add_string b s in
  let n = Array.length xs in
  for i = start to n do
    if i = dot then (
      separator() |> emit;
      emit bullet
    );
    if i < n then (
      separator() |> emit;
      emit (print xs.(i))
    )
  done

let count = function
  | 1 -> "one"
  | 2 -> "two"
  | 3 -> "three"
  | i -> Printf.sprintf "%d" i

let nth = function
  | 1 -> "first"
  | 2 -> "second"
  | 3 -> "third"
  | i -> Printf.sprintf "%dth" i

(* [digits n] computes how many decimal digits are involved in the
   decimal representation of the integer [n]. *)

let digits n =
  let rec loop accu n =
    if n < 10 then
      accu + 1
    else
      loop (accu + 1) (n / 10)
  in
  loop 0 n

(* [pad n s] pads the string [s] with zeroes in front so that its
   length is [n]. *)

let pad n s =
  String.make (n - String.length s) '0' ^ s

let padded_index n i =
  pad (digits n) (Printf.sprintf "%d" i)

(* -------------------------------------------------------------------------- *)

(* Analysis. *)

let longest_valid_prefix valid s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && valid s.[!i] do incr i done;
  String.sub s 0 !i

(* [longest_run c s] computes the length of the longest run of consecutive
   characters equal to [c] in the string [s]. *)

(* In the auxiliary function, [i] is the index of the next character that
   must be read, [m] is a max-accumulator, and [k] is a sum-accumulator. *)

let rec longest_run c s n i m k =
  if i = n then
    max m k
  else if s.[i] = c then
    longest_run c s n (i+1) m (k+1)
  else
    longest_run c s n (i+1) (max m k) 0

let longest_run c s =
  longest_run c s (String.length s) 0 0 0

(* -------------------------------------------------------------------------- *)

(* Transformations. *)

let normalize s =
  let s = Bytes.of_string s in
  let n = Bytes.length s in
  for i = 0 to n - 1 do
    match Bytes.get s i with
    | '('
    | ')'
    | ',' ->
        Bytes.set s i '_'
    | _ ->
        ()
  done;
  Bytes.unsafe_to_string s

let unquote s =
  if String.length s = 0 || s.[0] <> '"' then
    invalid_arg "unquote"
  else
    (* Skip the opening quote and decode the remainder using the lexer
       [DecodeString.unescape], which stops at the closing quote. *)
    let s = String.sub s 1 (String.length s - 1) in
    let lexbuf = Lexing.from_string s in
    with_buffer 8 @@ fun b ->
    DecodeString.unescape b lexbuf
