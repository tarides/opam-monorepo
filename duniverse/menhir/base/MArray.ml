(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

include Array

let empty =
  [||]

let[@inline] last a =
  let n = length a in
  assert (n > 0);
  unsafe_get a (n - 1)

let pop a =
  let n = length a in
  assert (n > 0);
  sub a 0 (n - 1)

let push a x =
  let n = length a in
  init (n + 1) (fun i -> if i = n then x else a.(i))

let suffix a k =
  let n = length a in
  if n <= k then a else sub a (n-k) k

let rec equal_segments equal a1 i1 a2 i2 n =
  n = 0 ||
  equal a1.(i1) a2.(i2) && equal_segments equal a1 (i1 + 1) a2 (i2 + 1) (n - 1)

let is_suffix equal a1 a2 =
  let n1 = length a1
  and n2 = length a2 in
  n1 <= n2 && equal_segments equal a1 0 a2 (n2 - n1) n1

let rec greatest_suffix_forall p a n k =
  if k = n || not (p a.(n - 1 - k)) then
    k
  else
    greatest_suffix_forall p a n (k + 1)

let greatest_suffix_forall p a =
  let k = greatest_suffix_forall p a (length a) 0 in
  suffix a k

let rec lcs v1 v2 n1 n2 n k =
  assert (n = min n1 n2);
  if k = n || v1.(n1 - 1 - k) <> v2.(n2 - 1 - k) then k
  else lcs v1 v2 n1 n2 n (k + 1)

let leq_join_lcs v1 v2 =
  let n1 = Array.length v1
  and n2 = Array.length v2 in
  let n = min n1 n2 in
  let k = lcs v1 v2 n1 n2 n 0 in
  if k = n2 then v2
  else if k = n1 then v1
  else suffix v1 k

let rev a =
  let n = length a in
  if n = 0 then
    a
  else
    let r = make n a.(0) in
    for i = 0 to n - 2 do
      r.(i) <- a.(n - i - 1)
    done;
    r

let rev_of_list xs =
  match xs with
  | [] ->
      [||]
  | x :: xs ->
      let n = 1 + List.length xs in
      let r = make n x in
      List.iteri (fun i x -> r.(n - i - 2) <- x) xs ;
      r

let[@inline] rev_to_list a =
  fold_left (fun xs x -> x :: xs) [] a

let[@tail_mod_cons] rec segment_to_list a i j =
  if i = j then
    []
  else
    a.(i) :: segment_to_list a (i + 1) j

let[@inline] iter_rev f a =
  for i = length a - 1 downto 0 do
    f a.(i)
  done

let filter p a =
  a |> to_list |> List.filter p |> of_list

let existsi p a =
  let n = length a in
  let rec loop i =
    if i = n then false
    else if p i (unsafe_get a i) then true
    else loop (succ i) in
  loop 0

let count p a =
  let n = length a in
  let c = ref 0 in
  for i = 0 to n-1 do
    if p (unsafe_get a i) then c := !c + 1
  done;
  !c

(* [equal] and [compare] appear in OCaml 5.4. *)

let equal eq a b =
  if length a <> length b then false else
  let i = ref 0 in
  let len = length a in
  while !i < len && eq (unsafe_get a !i) (unsafe_get b !i) do incr i done;
  !i = len

let compare cmp a b =
  let len_a = length a and len_b = length b in
  let diff = len_a - len_b in
  if diff <> 0 then (if diff < 0 then -1 else 1) else
  let i = ref 0 and c = ref 0 in
  while !i < len_a && !c = 0
  do c := cmp (unsafe_get a !i) (unsafe_get b !i); incr i done;
  !c

(* To keep compatibility with OCaml 4.03, we copy [Array.for_all2],
   which appeared in 4.11. *)

let for_all2 p l1 l2 =
  let n1 = length l1
  and n2 = length l2 in
  if n1 <> n2 then invalid_arg "Array.for_all2"
  else let rec loop i =
    if i = n1 then true
    else if p (unsafe_get l1 i) (unsafe_get l2 i) then loop (succ i)
    else false in
  loop 0

let fold_left2 f accu a1 a2 =
  let n1 = length a1
  and n2 = length a2 in
  if n1 <> n2 then invalid_arg "Array.fold_left2";
  let accu = ref accu in
  for i = 0 to n1 - 1 do
    accu := f !accu (unsafe_get a1 i) (unsafe_get a2 i)
  done;
  !accu

let leq_join leq_join a1 a2 =
  let n = length a1 in
  assert (n = length a2);
  let a = init n (fun i -> leq_join (unsafe_get a1 i) (unsafe_get a2 i)) in
  if for_all2 (==) a2 a then a2 else a

let rec findi f i arr =
  if i >= Array.length arr then
    raise Not_found
  else if f i arr.(i) then
    i
  else
    findi f (i + 1) arr

let inverse (a : 'a array) : 'a -> int =
  let table = Hashtbl.create (Array.length a) in
  Array.iteri (fun i data ->
    assert (not (Hashtbl.mem table data));
    Hashtbl.add table data i
  ) a;
  fun data ->
    try
      Hashtbl.find table data
    with Not_found ->
      assert false

let tabulate n f =
  let a = Array.init n f in
  Array.get a

let test () =
  assert (pop [|1; 2; 3; 4|] = [|1; 2; 3|]) ;
  assert (push [|1; 2; 3|] 4 = [|1; 2; 3; 4|]) ;
  assert (suffix [|1; 2; 3; 4|] 2 = [|3; 4|]) ;
  assert (suffix [|1; 2|] 4 = [|1; 2|]) ;
  assert (is_suffix (=) [||] [||]) ;
  assert (is_suffix (=) [||] [|0;3;4|]) ;
  assert (is_suffix (=) [|2|] [|0;2|]) ;
  assert (is_suffix (=) [|3; 4|] [|0;3;4|]) ;
  assert (greatest_suffix_forall ((<) 4) [|1; 2; 3; 4|] = [||]) ;
  assert (greatest_suffix_forall ((<) 2) [|1; 2; 3; 4|] = [|3; 4|]) ;
  assert (greatest_suffix_forall ((<) 0) [|1; 2; 3; 4|] = [|1; 2; 3; 4|]) ;
  assert (greatest_suffix_forall ((<) 0) [|1; 2; 0; 4|] = [|4|]) ;
  assert (rev [|1; 2; 3; 4|] = [|4; 3; 2; 1|]) ;
  assert (rev_of_list [1; 2; 3; 4; 5] = [|5; 4; 3; 2; 1|]) ;
  assert (rev_to_list [|1; 2; 3; 4; 5|] = [5; 4; 3; 2; 1]) ;
  assert (count (fun x -> x mod 2 = 0) [| 1;2;3 |] = 1) ;
  assert (count (fun x -> x mod 2 = 0) [||] = 0) ;
  assert (suffix [|1; 2; 3; 4; 5|] 0 = [||]) ;
  assert (suffix [|1; 2; 3; 4; 5|] 3 = [|3; 4; 5|]) ;
  assert (suffix [|1; 2; 3; 4; 5|] 5 = [|1; 2; 3; 4; 5|]) ;
  assert (segment_to_list [|1; 2; 3; 4|] 1 3 = [2; 3]) ;
  ()
