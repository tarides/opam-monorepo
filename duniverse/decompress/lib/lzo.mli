type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type error =
  [ `Malformed of string | `Invalid_argument of string | `Invalid_dictionary ]

val pp_error : Format.formatter -> error -> unit

val uncompress : bigstring -> bigstring -> (bigstring, [> error ]) result
(** [uncompress input output] returns a {i sub-layout} of [output] which is the
    inflated contents of [input]. Otherwise, it returns:

    - [`Malformed] if the [input] is not recognized as a LZO contents.
    - [`Invalid_argument] if [output] is not large enough to contain inflated
      contents.
    - [`Invalid_dictionary] if an {i op-code} of [input] refers to an unbound
      location. *)

val uncompress_with_buffer :
  ?chunk:int -> bigstring -> (string, [> error ]) result
(** [uncompress ?chunk input] returns a fresh-allocated [string] which is the
    inflated contents of [input]. An internal {!Buffer.t} is used and it can be
    initialized with [chunk] (default to [0x1000]). Otherwise, it returns same
    errors as [uncompress]. *)

type wrkmem
(** Type of an internal {i hash-table} used by {!compress}. *)

val make_wrkmem : unit -> wrkmem
(** [make_wrkmem ()] returns a fresh-allocated {!wrkmem}. *)

val compress : bigstring -> bigstring -> wrkmem -> int
(** [compress input output wrkmem] deflates [input] and produces a LZO contents
    into [output]. It uses [wrkmem] to do the deflation. It returns the number
    of bytes wrotes into [output] such as:

    {[
    let len = compress input output wrkmem in
    Bigarray.Array1.sub output 0 len
    ]}

    is the deflated contents of [input].

    @raise Invalid_argument
      if [output] is not large enough to contain the deflated contents. *)
