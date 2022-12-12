include module type of struct
  include StringLabels
end

val extract_blank_separated_words : string -> string list

module Map : Map.S with type key = string
