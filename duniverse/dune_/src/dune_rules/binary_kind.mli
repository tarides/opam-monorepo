(** Linking modes for binaries *)

open Import

type t =
  | C
  | Exe
  | Object
  | Shared_object
  | Plugin
  | Js

val compare : t -> t -> Ordering.t

include Dune_lang.Conv.S with type t := t

val all : t list

val to_dyn : t -> Dyn.t
