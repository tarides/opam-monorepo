(** One way normalization of URIs.
    Not meant to expose the normalized value of the URI again *)
module Normalized : sig
  type t
  (** Abstracts away the actual value which is not to be used directly *)

  val of_uri : Uri.t -> t
  (** Returns a canonical representation of the URI *)

  val equal : t -> t -> bool
  (** Determines whether two normalized URIs are equal *)

  val pp : t Fmt.t
  (** Pretty printer for normalized URLs. *)

  (**/**)

  module Private : sig
    val unescaped : Uri.t -> t
    (** Convert a [Uri.t] into a [t] without escaping. Only used for testing
        purposes, as it breaks the security guarantees. *)
  end

  (**/**)
end
