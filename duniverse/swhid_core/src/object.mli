(** This module contains the various types used to represent a Software Heritage
    persistent identifier (swhid). These identifiers are documented on
    {{:https://docs.softwareheritage.org/devel/swh-model/persistent-identifiers.html}
    Software Heritage}.

    In short, a swhid is made of a mandatory core identifier (a scheme version,
    a kind and a hash) and an optional list of qualifiers. *)

module Scheme_version : sig
  (** Module to work with the scheme version of a swhid.*)

  (** The type of scheme versions. *)
  type t

  (** [of_string s] is [Ok v] if [s] is a valid scheme version, otherwise it is
      [Error e]. *)
  val of_string : string -> (t, string) result

  (** [of_int n] is [Ok v] if [n] is a valid scheme version, otherwise it is
      [Error e]. *)
  val of_int : int -> (t, string) result

  (** [to_int v] is the representation of [v] as an integer. *)
  val to_int : t -> int

  (** [pp fmt v] prints [v] on formatter [fmt]. *)
  val pp : Format.formatter -> t -> unit

  (** [default] is the default scheme version. *)
  val default : t
end

module Kind : sig
  (** Module to work with the different kinds of software artifacts a swhid can
      points to. They're documented
      {{:https://docs.softwareheritage.org/devel/swh-model/data-model.html#software-artifacts}
      here}. *)

  (** The type of the different kinds of software artifacts. *)
  type t =
    | Content of string
        (** Contents (AKA "blobs"). the string parameter is the name of the hash
            function used for the computation, defaults to ["sha1_git"] and in
            most use cases you don't care about it. *)
    | Directory  (** Directories. *)
    | Revision  (** Revisions. *)
    | Release  (** Releases. *)
    | Snapshot  (** Snapshots. *)

  (** [compare x y] returns [0] if [x] is equal to [y], a negative integer if
      [x] is less than [y], and a positive integer if [x] is greater than [y]. *)
  val compare : t -> t -> int

  (** [equal x y] returns [true] iff [x] is equal to [y]. *)
  val equal : t -> t -> bool

  (** [of_string s] is [Ok v] if [s] is a valid kind of object, otherwise it is
      [Error e]. The valid kinds are ["cnt"], ["dir"], ["rel"], ["rev"] and
      ["snp"]. *)
  val of_string : string -> (t, string) result

  (** [pp fmt v] prints [v] on formatter [fmt]. *)
  val pp : Format.formatter -> t -> unit

  (** [to_string v] is the representation of [v] as a string. *)
  val to_string : t -> string
end

module Hash : sig
  (** Module to work with the hash component of a swhid. *)

  (** The type of hashes. *)
  type t

  (** [compare x y] returns [0] if [x] is equal to [y], a negative integer if
      [x] is less than [y], and a positive integer if [x] is greater than [y]. *)
  val compare : t -> t -> int

  (** [equal x y] returns [true] iff [x] is equal to [y]. *)
  val equal : t -> t -> bool

  (** [of_string s] is [Ok v] if [s] is a valid hash, otherwise it is [Error e].
      A hash is valid if it's made of 40 hexadecimal characters. *)
  val of_string : string -> (t, string) result

  (** [pp fmt v] prints [v] on formatter [fmt]. *)
  val pp : Format.formatter -> t -> unit

  (** [to_string v] is the representation of [v] as a string. *)
  val to_string : t -> string
end

module Core_identifier : sig
  (** Module to work with the core identifier of a swhid. The core identifier of
      a swhid is made of its scheme version, its kind and its hash. They're
      documented
      {{:https://docs.softwareheritage.org/devel/swh-model/persistent-identifiers.html#core-identifiers}
      here}. *)

  (** The type of core identifiers. *)
  type t

  (** [compare x y] returns [0] if [x] is equal to [y], a negative integer if
      [x] is less than [y], and a positive integer if [x] is greater than [y]. *)
  val compare : t -> t -> int

  (** [equal x y] returns [true] iff [x] is equal to [y]. *)
  val equal : t -> t -> bool

  (** [of_string s] is [Ok v] if [s] is a valid core identifier, otherwise it is
      [Error e]. *)
  val of_string : string -> (t, string) result

  (** [mk scheme kind hash] is the core identifier made of the scheme version
      [scheme], the kind [kind] and the hash [hash]. *)
  val mk : Scheme_version.t -> Kind.t -> Hash.t -> t

  (** [pp fmt v] prints [v] on formatter [fmt]. *)
  val pp : Format.formatter -> t -> unit

  (** [to_string v] is the representation of [v] as a string. *)
  val to_string : t -> string

  (** [get_scheme v] is the scheme version of [v]. *)
  val get_scheme : t -> Scheme_version.t

  (** [get_kind v] is the kind of [v] *)
  val get_kind : t -> Kind.t

  (** [get_hash v] is the hash of [v]. *)
  val get_hash : t -> Hash.t
end

module Qualifier : sig
  (** Module to work with qualifiers. They are documented
      {{:https://docs.softwareheritage.org/devel/swh-model/persistent-identifiers.html#qualifiers}
      here}. *)

  (** The type of qualifiers. *)
  type t =
    | Anchor of Core_identifier.t
    | Origin of string
    | Path of string
    | Visit of Core_identifier.t
    | Fragment of (int * int option)

  (** [of_string s] is [Ok v] if [s] is a valid qualifier, otherwise it is
      [Error e]. *)
  val of_string : string -> (t, string) result

  (** [pp fmt v] prints [v] on formatter [fmt]. *)
  val pp : Format.formatter -> t -> unit

  (** [to_string v] is the representation of [v] as a string. *)
  val to_string : t -> string
end

(** The type of swhids. *)
type t

(** [of_string s] is [Ok v] if [s] is a valid swhid, otherwise it is [Error e]. *)
val of_string : string -> (t, string) result

(** [mk c q] is the swhid made of the core identifier [c] and the list of
    qualifiers [q]. *)
val mk : Core_identifier.t -> Qualifier.t list -> t

(** [get_core v] is the core identifier of [v]. *)
val get_core : t -> Core_identifier.t

(** [get_scheme v] is the scheme of [v]. *)
val get_scheme : t -> Scheme_version.t

(** [get_kind v] is the kind of [v]. *)
val get_kind : t -> Kind.t

(** [get_hash v] is the hash of [v]. *)
val get_hash : t -> Hash.t

(** [get_qualifiers v] is the list of qualifiers of [v]*)
val get_qualifiers : t -> Qualifier.t list

(** [pp fmt v] prints [v] on formatter [fmt]. *)
val pp : Format.formatter -> t -> unit

(** [to_string v] is the representation of [v] as a string. *)
val to_string : t -> string
