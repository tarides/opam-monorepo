include ListLabels

let rev_partition_map =
  let rec loop l accl accr ~f =
    match l with
    | [] -> (accl, accr)
    | x :: l -> (
        match (f x : (_, _) Either.t) with
        | Left y -> loop l (y :: accl) accr ~f
        | Right y -> loop l accl (y :: accr) ~f)
  in
  fun l ~f -> loop l [] [] ~f

let partition_map l ~f =
  let l, r = rev_partition_map l ~f in
  (rev l, rev r)

let filter_opt l = filter_map ~f:(fun x -> x) l

let concat_map ~f l =
  let rec aux f acc = function
    | [] -> rev acc
    | x :: l ->
        let xs = f x in
        aux f (rev_append xs acc) l
  in
  aux f [] l

let max_exn ~compare l =
  match l with
  | [] -> invalid_arg "List.max_exn: empty list"
  | hd :: tl ->
      fold_left tl ~init:hd ~f:(fun acc elm ->
          match Ordering.of_int (compare acc elm) with
          | Gt | Eq -> acc
          | Lt -> elm)

let compare ~compare l l' =
  let rec aux l l' =
    match (l, l') with
    | hd :: tl, hd' :: tl' ->
        let c = compare hd hd' in
        if c = 0 then aux tl tl' else c
    | [], _ :: _ -> Ordering.to_int Lt
    | _ :: _, [] -> Ordering.to_int Gt
    | [], [] -> 0
  in
  aux l l'

let unzip l =
  let xs, ys =
    fold_left ~f:(fun (xs, ys) (x, y) -> (x :: xs, y :: ys)) ~init:([], []) l
  in
  (rev xs, rev ys)
