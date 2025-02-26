module type S = sig
  (** The signature of the modules present in the module generated by the stanza
      generate_sites_module *)

  val paths : string list

  val list : unit -> string list

  val load_all : unit -> unit

  val load : string -> unit
end

(** The functor applied in the generated module *)
module Make (_ : sig
  val paths : string list
end) : S

(** Load a library *)
val load : string -> unit

(** Indicates if a library exists in the search path *)
val available : string -> bool
