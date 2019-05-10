type 'a t = 'a option

let map ~f = function
  | None -> None
  | Some a -> Some (f a)
