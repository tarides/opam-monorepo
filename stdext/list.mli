include module type of struct
  include ListLabels
end

val max_exn : compare:('a -> 'a -> int) -> 'a list -> 'a
(** Returns the greatest element in the list according to [compare].
    Raises Invalig_argument on empty lists *)
