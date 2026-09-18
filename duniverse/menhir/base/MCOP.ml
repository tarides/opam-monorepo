(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

type problem =
  int array

type 'a solution =
  | Matrix of 'a
  | Product of 'a solution * 'a solution

let rec map f = function
  | Matrix x ->
      Matrix (f x)
  | Product (t, u) ->
      let t' = map f t in
      let u' = map f u in
      Product (t', u')

(* -------------------------------------------------------------------------- *)

(* Handle trivial cases before falling back to a smarter algorithm. *)

exception Empty

let trivial f dims =
  match dims with
  | [||] | [|_|] ->
      raise Empty
  | [|_; _|] ->
      (* One matrix: there is just one solution. *)
      Matrix 0
  | [|_; _; _|] ->
      (* Two matrices: there is just one solution. *)
      Product (Matrix 0, Matrix 1)
  | [|a; b; c; d|] ->
      (* Three matrices: there are two solutions. One test suffices. *)
      let cost_AB_C = a * b * c + a * c * d in
      let cost_A_BC = a * b * d + b * c * d in
      if cost_AB_C < cost_A_BC
      then Product (Product (Matrix 0, Matrix 1), Matrix 2)
      else Product (Matrix 0, Product (Matrix 1, Matrix 2))
  | _ ->
      (* Not a trivial case. *)
      f dims

(* -------------------------------------------------------------------------- *)

(* Trivial solutions. *)

let left dims =
  let len = Array.length dims in
  if len <= 1 then raise Empty;
  let solution = ref (Matrix 0) in
  for i = 1 to len - 1 do
    solution := Product (!solution, Matrix i)
  done;
  !solution

let right dims =
  let len = Array.length dims in
  if len <= 1 then raise Empty;
  let solution = ref (Matrix (len - 1)) in
  for i = len - 2 downto 0 do
    solution := Product (Matrix i, !solution)
  done;
  !solution

(* -------------------------------------------------------------------------- *)

(* Francis Chin's algorithm. *)

(* [argmax] returns the index of the largest element of the array. *)

let argmax (a : float array) : int =
  let i = ref 0 in
  let m = ref a.(0) in
  for j = 1 to Array.length a - 1 do
    let m' = a.(j) in
    if m' > !m then begin
      i := j;
      m := m';
    end
  done;
  !i

(* [vector] returns a permutation vector corresponding to the problem [k]. *)

let vector (k : problem) =
  (* k.(0..n) *)
  let n = Array.length k - 1 in
  (* v.(0..n-2) *)
  let v = Array.make (n - 1) 0 in
  (* r.(0..n) *)
  let r = Array.map (fun k_i -> 1.0 /. float k_i) k in
  (* 0 <= m <= n, r_m = r.(m) *)
  let m = argmax r in
  let r_m = r.(m) in
  let c = ref 1 in
  let j = ref 0 in
  let s = Array.make (n + 1) 0 in
  for i = 1 to n - 1 do
    incr j;
    s.(!j) <- i;
    while !j > 0 && r.(s.(!j)) +. r_m < r.(s.(!j-1)) +. r.(i+1) do
      v.(s.(!j)-1) <- !c;
      incr c;
      decr j;
    done;
  done;
  let b = ref (n - 1) in
  incr j;
  s.(!j) <- n;
  let k = ref 0 in
  let stop = ref false in
  while not !stop do
    if r.(s.(!k)) < r.(s.(!j)) then
      begin
        if r.(s.(!j)) +. r_m < r.(s.(!j-1)) +. r.(s.(!k)) then
          begin decr j; v.(s.(!j) - 1) <- !b; decr b; end
        else if r.(s.(!k)) +. r_m < r.(s.(!k+1)) +. r.(s.(!j)) then
          begin incr k;
            if s.(!k) < n then v.(s.(!k)-1) <- !b;
            decr b;
          end
        else stop := true
      end
    else stop := true
  done;
  for i = m - 1 downto s.(!k) + 1
  do if v.(i-1) = 0 then begin v.(i-1) <- !c; incr c end done;
  for i = m + 1 to s.(!j) - 1
  do if v.(i-1) = 0 then begin v.(i-1) <- !c; incr c end done;
  if m > 0 && m < n && v.(m-1) = 0 then v.(m-1) <- !c;
  v

(* [parse] parses the permutation vector produced by [vector]
   using a variant of the shunting-yard algorithm. *)

let rec pop (n : int) x stack =
  match stack with
  | (n', x') :: xs when n' <= n ->
      pop n (Product (x', x)) xs
  | _ ->
      (n, x) :: stack

let push (i, stack) v =
  (i + 1, pop v (Matrix i) stack)

let close (i, stack) =
  snd (List.hd (pop max_int (Matrix i) stack))

let parse v =
  let stack = [] in
  let stack = Array.fold_left push (0, stack) v in
  close stack

let approx =
  trivial @@ fun dims ->
  parse (vector dims)

(* -------------------------------------------------------------------------- *)

(* The optimal solution. *)

let build_matrix (dims : problem) =
  let n = Array.length dims - 1 in
  let m = Array.make_matrix n n 0 in
  let s = Array.make_matrix n n 0 in
  for len = 1 to n - 1 do
    for i = 0 to n - len - 1 do
      let j = i + len in
      m.(i).(j) <- max_int;
      for k = i to j - 1 do
        let cost = m.(i).(k) + m.(k+1).(j) + dims.(i)*dims.(k+1)*dims.(j+1) in
        if cost < m.(i).(j) then begin
          m.(i).(j) <- cost;
          s.(i).(j) <- k;
        end
      done
    done
  done;
  s

let parse_matrix s =
  let rec loop i j =
    if i <> j then
      let i_s = loop i s.(i).(j) in
      let s_j = loop (s.(i).(j) + 1) j in
      Product (i_s, s_j)
    else
      Matrix i
  in
  loop 0 (Array.length s - 1)

let optimal =
  trivial @@ fun dims ->
  parse_matrix (build_matrix dims)
