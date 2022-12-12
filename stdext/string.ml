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

module Map = Map.Make (String)
