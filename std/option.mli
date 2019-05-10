type 'a t = 'a option

val map : f:('a -> 'b) -> 'a t -> 'b t
