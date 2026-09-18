(** Module to compute a swhid for all kinds of objects. *)

(** The type of directory entries. *)
type directory_entry_kind =
  | File
  | Dir

module Make (SHA1 : sig
  (** A module containing the needed hash functions. *)

  (** [digest_string_to_hex s] computes the SHA1 hash of [s] and returns its
      hexadecimal representation. *)
  val digest_string_to_hex : string -> string
end) (OS : sig
  (** A module containing the needed OS-related functions. *)

  (** [contents dir] returns the list of files in the directory [dir]. *)
  val contents : string -> string list option

  (** [type file] returns [Dir] if [file] is a directory and [File] otherwise. *)
  val typ : string -> directory_entry_kind option

  (** [read_file f] returns the content of the file [f]. *)
  val read_file : string -> string option

  (** [permissions f] returns the 16-bit file mode (as stored by Git) of the
      file [f]. That is:

      - [0o120000] if [f] is a symlink
      - [0o040000] if [f] is a directory
      - [0o100755] if [f] is an executable file
      - [0o100644] if [f] is a regular file *)
  val permissions : string -> int option

  (** [base f] is the basename of file [f]. *)
  val base : string -> string
end) : sig
  (** A functor that, given a SHA1 module and a OS module, provides various
      functions to compute the swhid of a given object. Supported objects are
      [content], [directory], [release], [revision] and [snapshot]. The origins
      and visits objects are not supported. *)

  (** The type for directory entries list, needed to compute directories
      identifiers. *)
  type directory_entry =
    { typ : directory_entry_kind
    ; permissions : int
    ; name : string
    ; target : Object.Core_identifier.t
    }

  (** The type for dates, needed to compute releases and revisions identifiers. *)
  type date =
    { timestamp : Int64.t
    ; tz_offset : int
    ; negative_utc : bool
    }

  (** [content_identifier s] computes the swhid for the [s] content. [s] is the
      raw content of a file as a [string].

      E.g. [content_identifier "_build\n"] is the swhid of this library's
      [.gitignore] file. *)
  val content_identifier : string -> (Object.t, string) result

  (** [directory_identifier entries] compute the swhid for the [entries]
      directory. [entries] is a list of [Kinds.directory_entry] where each
      element points to another object (usually a file content or a
      sub-directory).

      E.g.
      [directory_identifier \[ { typ = "file"
                        ; permissions = 33188
                        ; name = "README"
                        ; target = "37ec8ea2110c0b7a32fbb0e872f6e7debbf95e21"
                        }\]]
      is the swhid of a directory which has a single file [README] with
      permissions 33188 and whose core identifier from [content_identifier] is
      [37ec8ea2110c0b7a32fbb0e872f6e7debbf95e21]. *)
  val directory_identifier : directory_entry list -> (Object.t, string) result

  (** [directory_identifier_deep] compute the swhid for a given directory name,
      it uses the various functions provided in the [OS] module parameter to
      list directory contents, get file permissions and read file contents.*)
  val directory_identifier_deep : string -> (Object.t, string) result

  (** [release_identifier target target_kind name ~author date ~message]
      computes the swhid for a release object pointing to an object of type
      [target_kind] whose identifier is [target], the release having [~name],
      [~author] and has been published on [date] with the release [~message]. *)
  val release_identifier :
       Object.Hash.t
    -> Object.Kind.t
    -> name:string
    -> author:string option
    -> date option
    -> message:string option
    -> (Object.t, string) result

  (** [revision dir parents ~author ~author_date ~committer ~committer_date extra_headers message]
      computes the swhid for a revision object whose directory has id [dir] and
      whose parents has ids [parents] which was authored by [~author] on
      [~author_date] and committed by [~committer] on [~committer_date] with
      extra headers [extra_headers] and message [message]. *)
  val revision_identifier :
       Object.Hash.t
    -> Object.Hash.t list
    -> author:string
    -> author_date:date option
    -> committer:string
    -> committer_date:date option
    -> (string * string) array
    -> message:string option
    -> (Object.t, string) result

  (** [snapshot_identifier branches] computes the swhid of the snapshot made of
      branches [branches] where [branches] is a list of branch elements. Each
      branch is of the form [name, target] where [name] is the name of the
      branch and where [target] is a pair made of the identifier of the branch
      and its type. *)
  val snapshot_identifier :
    (string * (string * string) option) list -> (Object.t, string) result
end
