(* TODO: remove once we have >= 4.08 *)
let result_is_error = function Ok _v -> false | Error _v -> true
let result_get_ok = function Ok v -> v | Error _v -> invalid_arg "Result.get_ok"


module Scheme_version = struct
  type t = int

  let of_string = function
    | "1" -> Ok 1
    | invalid -> Error (Format.sprintf "invalid scheme version `%s`" invalid)

  let of_int = function
    | 1 -> Ok 1
    | invalid -> Error (Format.sprintf "invalid scheme version `%d`" invalid)

  let to_int x = x

  let pp fmt v = Format.fprintf fmt "%d" v

  let default = 1
end

module Kind = struct
  type t =
    | Content of string
    | Directory
    | Revision
    | Release
    | Snapshot

  let compare t t' =
    match (t, t') with
    | Directory, Directory
    | Release, Release
    | Revision, Revision
    | Snapshot, Snapshot ->
      0
    | Content c, Content c' -> String.compare c c'
    | Content _, _ -> 1
    | _, Content _ -> -1
    | Directory, _ -> 1
    | _, Directory -> -1
    | Revision, _ -> 1
    | _, Revision -> -1
    | Release, _ -> 1
    | _, Release -> -1

  let equal t t' = compare t t' = 0

  let of_string = function
    | "cnt" -> Ok (Content "sha1_git")
    | "dir" -> Ok Directory
    | "rel" -> Ok Release
    | "rev" -> Ok Revision
    | "snp" -> Ok Snapshot
    | invalid -> Error (Format.sprintf "invalid object kind `%s`" invalid)

  let to_string = function
    | Content _f -> "cnt"
    | Directory -> "dir"
    | Release -> "rel"
    | Revision -> "rev"
    | Snapshot -> "snp"

  let pp fmt v = Format.fprintf fmt "%s" (to_string v)
end

module Hash = struct
  type t = string

  let compare = String.compare

  let equal = String.equal

  let of_string s =
    let len = ref 0 in
    try
      String.iter
        (function
          | 'a' .. 'f' | '0' .. '9' -> incr len | _invalid_char -> raise Exit )
        s;
      if !len = 40 then Ok s else raise Exit
    with Exit -> Error (Format.sprintf "invalid object hash `%s`" s)

  let to_string v = v

  let pp fmt v = Format.fprintf fmt "%s" v
end

(* TODO: remove once we have > 4.03 *)
let string_split_on_char sep s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if String.unsafe_get s i = sep then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  String.sub s 0 !j :: !r

module Core_identifier = struct
  type t = Scheme_version.t * Kind.t * Hash.t

  let compare (sch_version, object_type, hash)
      (sch_version', object_type', hash') =
    let scheme_version = sch_version - sch_version' in
    if scheme_version <> 0 then scheme_version
    else
      let object_type = Kind.compare object_type object_type' in
      if object_type <> 0 then object_type else Hash.compare hash hash'

  let equal t t' = compare t t' = 0

  let of_string s =
    match string_split_on_char ':' s with
    | [ "swh"; "1"; t; hash ] -> begin
      match Kind.of_string t with
      | Error _msg as e -> e
      | Ok t -> begin
        match Hash.of_string hash with
        | Error _msg as e -> e
        | Ok hash ->
          let scheme = Scheme_version.default in
          Ok (scheme, t, hash)
      end
    end
    | _whatever -> Error "invalid core identifier"

  let mk scheme typ hash = (scheme, typ, hash)

  let pp fmt (scheme, typ, hash) =
    Format.fprintf fmt "swh:%a:%a:%a" Scheme_version.pp scheme Kind.pp typ
      Hash.pp hash

  let to_string v = Format.asprintf "%a" pp v

  let get_scheme (scheme, _kind, _hash) = scheme

  let get_kind (_scheme, kind, _hash) = kind

  let get_hash (_scheme, _kind, hash) = hash
end

module Qualifier = struct
  type t =
    | Anchor of Core_identifier.t
    | Origin of string
    | Path of string
    | Visit of Core_identifier.t
    | Fragment of (int * int option)

  let int_of_string_opt s = try Some (int_of_string s) with Failure _ -> None

  let of_string s =
    match string_split_on_char '=' s with
    | "lines" :: lines -> begin
      match string_split_on_char '-' (String.concat "" lines) with
      | [ l1 ] -> begin
        match int_of_string_opt l1 with
        | None -> Error "invalid qualifier"
        | Some i -> Ok (Fragment (i, None))
      end
      | [ l1; l2 ] -> begin
        match (int_of_string_opt l1, int_of_string_opt l2) with
        | Some i1, Some i2 -> Ok (Fragment (i1, Some i2))
        | _, _ -> Error "invalid qualifier"
      end
      | _whatever -> Error "invalid qualifier"
    end
    | "path" :: path ->
      (* TODO: check RFC 3987 IRI compliance *)
      let path = String.concat "" path in
      Ok (Path path)
    | "origin" :: url ->
      (* TODO: check RFC 3987 absolute path compliance *)
      let url = String.concat "" url in
      Ok (Origin url)
    | "visit" :: id -> (
      let id = String.concat "" id in
      match Core_identifier.of_string id with
      | Error _msg as e -> e
      | Ok id -> Ok (Visit id) )
    | "anchor" :: id -> (
      let id = String.concat "" id in
      match Core_identifier.of_string id with
      | Error _msg as e -> e
      | Ok id -> Ok (Anchor id) )
    | _whatever -> Error "invalid qualifier"

  let pp fmt = function
    | Anchor id -> Format.fprintf fmt "anchor=%a" Core_identifier.pp id
    | Origin uri -> Format.fprintf fmt "origin=%s" uri
    | Path path -> Format.fprintf fmt "path=%s" path
    | Visit id -> Format.fprintf fmt "visit=%a" Core_identifier.pp id
    | Fragment (l1, l2) -> (
      Format.fprintf fmt "lines=%d" l1;
      match l2 with None -> () | Some l2 -> Format.fprintf fmt "-%d" l2 )

  let to_string q = Format.asprintf "%a" pp q
end

type t = Core_identifier.t * Qualifier.t list

(* TODO: remove once we have >= 4.05 *)
let rec list_find_opt p = function
  | [] -> None
  | x :: l -> if p x then Some x else list_find_opt p l

let of_string s =
  match string_split_on_char ';' s with
  | id :: qualifiers -> begin
    match Core_identifier.of_string id with
    | Error _msg as e -> e
    | Ok object_core_identifier -> begin
      let qualifiers = List.map Qualifier.of_string qualifiers in
      match list_find_opt result_is_error qualifiers with
      | Some (Error _msg as e) -> e
      | Some _ -> assert false
      | None ->
        let qualifiers = List.map result_get_ok qualifiers in
        Ok (object_core_identifier, qualifiers)
    end
  end
  | _whatever -> Error "invalid swhid"

let mk object_core_identifier qualifiers = (object_core_identifier, qualifiers)

let get_core (core, _qualifiers) = core

let get_scheme (core, _qualifiers) = Core_identifier.get_scheme core

let get_kind (core, _qualifiers) = Core_identifier.get_kind core

let get_hash (core, _qualifiers) = Core_identifier.get_hash core

let get_qualifiers (_core, qualifiers) = qualifiers

let pp_qualifiers fmt q = List.iter (Format.fprintf fmt ";%a" Qualifier.pp) q

let pp fmt id =
  let i = get_core id in
  let q = get_qualifiers id in
  Format.fprintf fmt "%a%a" Core_identifier.pp i pp_qualifiers q

let to_string id = Format.asprintf "%a" pp id
