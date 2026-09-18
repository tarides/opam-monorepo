type t [@@immediate64]
(** The type of integers with {i at least} 32 bits.
    For 63-bit integers, see {!Int63}. *)

include Integer_interface.S with type t := t
(** @inline *)

(** {1 Other modules} *)

(** A conditional type equality, used for revealing that a type [t] has one of
    two possible implementation types [u] and [v]. *)
module Conditional : sig
  type ('t, 'u, 'v) t =
    | True : ('t, 't, _) t (** therefore ['t] = ['u] *)
    | False : ('t, _, 't) t (** therefore ['t] = ['v] *)
end

val is_immediate : (t, int, int32) Conditional.t

(** 63-bit integers. *)
module Int63 : sig
  type t [@@immediate64]
  (** The type of integers with exactly 63-bits. *)

  include Integer_interface.S with type t := t
  (** @inline *)

  module Boxed : Integer_interface.S
  (** An implementation of 63-bit integers that always uses a boxed
      representation regardless of word size. *)

    (** [is_immediate] reveals the implementation of {!t} on the current
        platform, and can be used to build [Int63] operations that behave
        differently depending on the underlying representation, such as FFIs. *)
  val is_immediate : (t, int, Boxed.t) Conditional.t
end
