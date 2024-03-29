include module type of struct
  include Stdlib.Result
end

module O : sig
  val ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val ( >>| ) : ('a, 'err) t -> ('a -> 'b) -> ('b, 'err) t
  val ( let* ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val ( let+ ) : ('a, 'err) t -> ('a -> 'b) -> ('b, 'err) t
end

module List : sig
  val map : f:('a -> ('b, 'err) t) -> 'a list -> ('b list, 'err) t
  val iter : f:('a -> (unit, 'err) t) -> 'a list -> (unit, 'err) t

  val fold_left :
    'a list -> f:('acc -> 'a -> ('acc, 'c) t) -> init:'acc -> ('acc, 'c) t

  val exists : 'a list -> f:('a -> (bool, 'err) t) -> (bool, 'err) t
  (** Same as [List.exists] with a predicate that can return an error.
      Returns [Ok true] if there is at least one element in the list
      that satisfies the predicate [f].
      Returns [Ok false] on empty lists.
      Returns [Error _] immediatly if the predicate does. *)
end
