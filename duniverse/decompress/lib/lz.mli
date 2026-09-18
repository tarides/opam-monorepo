type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type optint = Optint.t
type src = [ `Channel of in_channel | `String of string | `Manual ]
type decode = [ `Flush | `Await | `End ]
type state
type literals = De.literals
type distances = De.distances
type window

val literals : state -> literals
val distances : state -> distances
val checksum : state -> optint
val src : state -> bigstring -> int -> int -> unit
val src_rem : state -> int
val make_window : bits:int -> window
val compress : state -> decode
val state : ?level:int -> q:De.Queue.t -> w:window -> src -> state
