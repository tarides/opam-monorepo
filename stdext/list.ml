include ListLabels

let max_exn ~compare l =
  match l with
  | [] -> invalid_arg "List.max_exn: empty list"
  | hd :: tl ->
      fold_left tl ~init:hd ~f:(fun acc elm ->
          match Ordering.of_int (compare acc elm) with
          | Gt | Eq -> acc
          | Lt -> elm)

let unzip l =
  let xs, ys =
    fold_left ~f:(fun (xs, ys) (x, y) -> (x :: xs, y :: ys)) ~init:([], []) l
  in
  (rev xs, rev ys)
