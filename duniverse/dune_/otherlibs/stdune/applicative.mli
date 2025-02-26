module type Basic = Applicative_intf.Basic

module Make (A : Basic) : Applicative_intf.S with type 'a t := 'a A.t
[@@inlined always]

module Id : Applicative_intf.S with type 'a t = 'a
