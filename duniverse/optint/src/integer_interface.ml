module type S = sig
  type t

  val zero : t
  (** Integer 0. *)

  val one : t
  (** Integer 1. *)

  val minus_one : t
  (** Integer (-1). *)

  val neg : t -> t
  (** Unary negation. *)

  val add : t -> t -> t
  (** Addition. *)

  val sub : t -> t -> t
  (** Subtraction. *)

  val mul : t -> t -> t
  (** Mulitplication. *)

  val div : t -> t -> t
  (** Integer division. Raise [Division_by_zero] if the second argument is zero.
      This division rounds the real quotient of its arguments towrds zero. *)

  val rem : t -> t -> t
  (** Integer remainder. If [y] is not zero, the result of [rem x y] satisfies
      the following property: [x = add (mul (div x y) y) (rem x y)]. if [y = 0],
      [rem x y] raises [Division_by_zero]. *)

  val succ : t -> t
  (** Successor. [succ x] is [add x one]. *)

  val pred : t -> t
  (** Predecessor. [pred x] is [sub x one]. *)

  val abs : t -> t
  (** Return the absolute value its argument. *)

  val max_int : t
  (** The greatest representable integer. *)

  val min_int : t
  (** The smallest representable integer. *)

  val logand : t -> t -> t
  (** Bitwise logical and. *)

  val logor : t -> t -> t
  (** Bitwise logical or. *)

  val logxor : t -> t -> t
  (** Bitwise logical exclusive or. *)

  val lognot : t -> t
  (** Bitwise logical negation. *)

  val shift_left : t -> int -> t
  (** [shift_left x y] shifts [x] to the left by [y] bits. The result is
      unspecified if [y < 0] or [y >= (32 || 63)]. *)

  val shift_right : t -> int -> t
  (** [shift_right x y] shifts [x] to the right by [y] bits. This is an
      arithmetic shift: the sign bit of [x] is replicated and inserted in the
      vacated bits. The result is unspecified if [y < 0] or [y >= (32 || 63)]. *)

  val shift_right_logical : t -> int -> t
  (** [shift_right_logical x y] shifts [x] to the right by [y] bits. This is a
      logical shift: zeroes are inserted in the vacated bits regardless of the
      sign of [x] / The result is unspecified if [y < 0] or [y >= (32 || 63)]. *)

  val of_int : int -> t
  (** Convert the given integer (type [int] ) to {!t}. It's an unsafe function
      whose semantic is different from architecture. *)

  val to_int : t -> int
  (** Convert the given {!t} integer to an integer (type [int] ). On 64-bit
      platforms, the conversion is exact. On 32-bit platforms, the 32-bit
      integer is taken modulo 2 {^ 31}, i.e. the high-order bit is lost during
      the conversion. *)

  val of_int32 : int32 -> t
  (** Convert the given 32-bit integer (type [int32]) to {!t} integer. It's an
      unsafe function whose semantic is different from architecture. *)

  val to_int32 : t -> int32
  (** Convert the given {!t} integer to a 32-bit integer. *)

  val of_int64 : int64 -> t
  (** Convert the given 64-bit integer (type [int64]) to {!t} integer. *)

  val to_int64 : t -> int64
  (** Covert the given {!t} integer to a 64-bit integer. *)

  val of_float : float -> t
  (** Convert the given floating-point number to a {!t} integer, discarding the
      fractional part (truncate towards 0). The result of the conversion is
      undefined if, after truncation, the number is outside the range
      {!min_int}, {!max_int}. *)

  val to_float : t -> float
  (** Convert the given {!t} integer to a floating-point number. *)

  val of_string : string -> t
  (** Convert the given string to a {!t} integer. The string is read in decimal
      (by default, or if the string begins with [0u]) or in hexadecimal, octal
      or binary if the string begins with [0x], [0o] or [0b] respectively.

      The [0u] prefix reads the input as an unsigned integer in the range
      [\[0, 2 * max_int + 1\]]. If the input exceeds {!max_int} it is converted
      to the signed integer [min_int + input - max_int - 1].

      The [_] (underscore) character can appear anywhere in the string is
      ignored. Raise [Failure _] if the given string is not a valid
      representation of an integer, or if the integer represented exceeds the
      range of integer, or if the integer represented exceeds the range of
      integers representable in type {!t}. *)

  val of_string_opt : string -> t option
  (** Same as [of_string], but return [None] instead of raising. *)

  val to_string : t -> string
  (** Return the string representation of its argument, in decimal. *)

  val compare : t -> t -> int
  (** The comparison function for {!t} integers, with the same specification as
      {!Stdlib.compare}. Along with the type [t], this function [compare] allows
      the module [Optint] to be passed as argument to the functors {!Set.Make}
      and {!Map.Make}. *)

  val equal : t -> t -> bool
  (** The equal function for {!t}. *)

  val pp : Format.formatter -> t -> unit
  (** The pretty-printer for {!t}. *)

  (** {2 Encoding functions}

      Efficient fixed-length big-endian encoding functions for {!t} integers: *)

  val encode : bytes -> off:int -> t -> unit
  val decode : string -> off:int -> t

  val encoded_size : int
  (** The number of bytes in the {{!encode} encoded} form of {!t}. *)

  val to_unsigned_int32 : t -> int32
  val of_unsigned_int32 : int32 -> t
  val to_unsigned_int : t -> int
  val of_unsigned_int : int -> t

  module Infix : sig
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( % ) : t -> t -> t
    val ( / ) : t -> t -> t
    val ( land ) : t -> t -> t
    val ( lor ) : t -> t -> t
    val ( lsr ) : t -> int -> t
    val ( lsl ) : t -> int -> t

    val ( && ) : t -> t -> t
    [@@ocaml.deprecated "Please use ( land )."]
    val ( || ) : t -> t -> t
    [@@ocaml.deprecated "Please use ( lor )."]
    val ( >> ) : t -> int -> t
    [@@ocaml.deprecated "Please use ( lsr )."]
    val ( << ) : t -> int -> t
    [@@ocaml.deprecated "Please use ( lsl )."]
  end
end
