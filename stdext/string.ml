module String = Stdlib.String
include StringLabels

let extract_words s ~is_word_char =
  let rec skip_blanks i =
    if i = length s then []
    else if is_word_char s.[i] then parse_word i (i + 1)
    else skip_blanks (i + 1)
  and parse_word i j =
    if j = length s then [ sub s ~pos:i ~len:(j - i) ]
    else if is_word_char s.[j] then parse_word i (j + 1)
    else sub s ~pos:i ~len:(j - i) :: skip_blanks (j + 1)
  in
  skip_blanks 0

let extract_blank_separated_words s =
  extract_words s ~is_word_char:(function ' ' | '\t' -> false | _ -> true)

let index = index_opt
let rindex = rindex_opt

let lsplit2 s ~on =
  match index s on with
  | None -> None
  | Some i ->
      Some (sub s ~pos:0 ~len:i, sub s ~pos:(i + 1) ~len:(length s - i - 1))

let rsplit2 s ~on =
  match rindex s on with
  | None -> None
  | Some i ->
      Some (sub s ~pos:0 ~len:i, sub s ~pos:(i + 1) ~len:(length s - i - 1))

module Map = Map.Make (String)
