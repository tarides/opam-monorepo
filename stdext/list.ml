include ListLabels

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
