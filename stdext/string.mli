include module type of struct
  include StringLabels
end

val index : string -> char -> int option
val rindex : string -> char -> int option
val lsplit2 : string -> on:char -> (string * string) option
val rsplit2 : string -> on:char -> (string * string) option
val extract_blank_separated_words : string -> string list

module Map : Map.S with type key = string
