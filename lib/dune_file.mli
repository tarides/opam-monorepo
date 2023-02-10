module Raw : sig
  val as_sexps : Fpath.t -> (Sexplib0.Sexp.t list, [> `Msg of string ]) result
  (** Parses a dune file as a list of S-expressions. *)

  val comment : string -> string

  val vendored_dirs : string -> string
  (** [vendored_dirs glob] returns a stanza marking directories matching [glob] as vendored *)

  val duniverse_dune_content : string list
  (** The content of the duniverse/dune file as a list of lines *)
end

module Lang : sig
  type version = int * int

  val compare_version : version -> version -> int
  val pp_version : version Fmt.t

  val duniverse_minimum_version : version
  (** The minimum dune lang version required by duniverse *)

  val from_content : string -> (version, [> `Msg of string ]) result
  (** Extract the lang version from the content of the entire dune-project *)

  val update : version:version -> string -> string
  (** Update the content of the entire dune-project, setting the lang version
      to [version].
      Return the string unmodified if there was previously no lang stanza. *)
end

module Packages : sig
  module Map : Stdext.Map.S

  type t

  val init : string -> t

  type new_name = {
    public_name : string;
    private_name : string option;
    dune_project : string;
  }

  type 'a change = { changed : bool; data : 'a }
  type 'a rename = { stanzas : 'a; renames : new_name Map.t }

  val rename :
    t ->
    dune_project:string ->
    keep:string list ->
    new_name Map.t ->
    Sexplib0.Sexp.t list ->
    Sexplib0.Sexp.t list rename change

  val update_references :
    dune_project:string ->
    new_name Map.t ->
    Sexplib0.Sexp.t list ->
    Sexplib0.Sexp.t list change

  val renamed_opam_file : new_name Map.t -> Fpath.t -> Fpath.t

  val update_dune_project_references :
    new_name Map.t -> Sexplib0.Sexp.t list -> Sexplib0.Sexp.t list change
end

module Project : sig
  val name : Sexplib0.Sexp.t list -> (string, [> `Msg of string ]) result
  (** Returns the dune-project's name given the content of the file as a list of S-expressions,
      if any. *)
end
