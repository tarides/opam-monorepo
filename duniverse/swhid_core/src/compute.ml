(* TODO: remove once we get >= 4.08 *)
let result_is_error = function Ok _v -> false | Error _v -> true
let result_get_ok = function Ok v -> v | Error _v -> invalid_arg "Result.get_ok"

type directory_entry_kind =
  | File
  | Dir

module Make (SHA1 : sig
  val digest_string_to_hex : string -> string
end) (OS : sig
  val contents : string -> string list option

  val typ : string -> directory_entry_kind option

  val read_file : string -> string option

  val permissions : string -> int option

  val base : string -> string
end) =
struct
  module Git = struct
    let target_kind_to_git = function
      | Object.Kind.Content _hash_type -> "blob"
      | Directory -> "tree"
      | Release -> "tag"
      | Revision -> "commit"
      | Snapshot -> "refs"

    let id_to_bytes id =
      String.init
        (String.length id / 2)
        (fun i ->
          let s = String.sub id (2 * i) 2 in
          Char.chr @@ int_of_string @@ "0x" ^ s )

    let object_to_swhid (obj : string) object_type =
      let scheme = Object.Scheme_version.default in
      let hash = SHA1.digest_string_to_hex obj in
      match Object.Hash.of_string hash with
      | Error _msg as e -> e
      | Ok hash ->
        let core_identifier =
          Object.Core_identifier.mk scheme object_type hash
        in
        Ok (Object.mk core_identifier [])

    let object_header fmt (git_type, len) =
      match git_type with
      | "blob" | "commit" | "extid" | "raw_extrinsic_metadata" | "snapshot"
      | "tag" | "tree" ->
        Format.fprintf fmt "%s %d\x00" git_type len
      | git_type ->
        invalid_arg
          (Format.sprintf "invalid git object type `%s` (Git.object_header)"
             git_type )

    let object_from_contents_strtarget target_kind contents =
      let len = String.length contents in
      Format.asprintf "%a%s" object_header (target_kind, len) contents

    let object_from_contents target_kind contents =
      object_from_contents_strtarget (target_kind_to_git target_kind) contents

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

    let escape_newlines snippet =
      String.concat "\n " (string_split_on_char '\n' snippet)

    (* TODO: replace with Int.abs when we have >= 4.08 *)
    let abs x = if x >= 0 then x else -x

    let format_offset fmt (offset, negative_utc) =
      let sign =
        if offset < 0 || (offset = 0 && negative_utc) then "-" else "+"
      in
      let offset = abs offset in
      let hours = offset / 60 in
      let minutes = offset mod 60 in
      Format.fprintf fmt "%s%02d%02d" sign hours minutes

    let format_author_data fmt (author, date) =
      Format.fprintf fmt "%s" author;
      match date with
      | None -> ()
      | Some (timestamp, tz_offset, negative_utc) ->
        Format.fprintf fmt " %Ld %a" timestamp format_offset
          (tz_offset, negative_utc)
  end

  type directory_entry =
    { typ : directory_entry_kind
    ; permissions : int
    ; name : string
    ; target : Object.Core_identifier.t
    }

  type date =
    { timestamp : Int64.t
    ; tz_offset : int
    ; negative_utc : bool
    }

  let content_identifier content =
    let typ = Object.Kind.Content "sha1_git" in
    let git_object = Git.object_from_contents typ content in
    Git.object_to_swhid git_object typ

  let directory_identifier entries =
    let entries =
      List.sort
        (fun entry1 entry2 ->
          String.compare
            (if entry1.typ = Dir then entry1.name ^ "/" else entry1.name)
            (if entry2.typ = Dir then entry2.name ^ "/" else entry2.name) )
        entries
    in
    let content =
      Format.asprintf "%a"
        (Format.pp_print_list
           ~pp_sep:(fun _fmt () -> ())
           (fun fmt entry ->
             Format.fprintf fmt "%o %s%c%s" entry.permissions entry.name '\x00'
               (Git.id_to_bytes
                  ( Object.Hash.to_string
                  @@ Object.Core_identifier.get_hash entry.target ) ) ) )
        entries
    in
    let typ = Object.Kind.Directory in
    let git_object = Git.object_from_contents typ content in
    Git.object_to_swhid git_object typ

  (* TODO: remove once we have >= 4.05 *)
  let rec list_find_opt p = function
    | [] -> None
    | x :: l -> if p x then Some x else list_find_opt p l

  let rec directory_identifier_deep name =
    match OS.contents name with
    | None -> Error (Format.sprintf "can't get contents of `%s`" name)
    | Some contents -> (
      let entries =
        List.map
          (fun name ->
            let typ = OS.typ name in
            let target =
              match typ with
              | Some File -> begin
                match OS.read_file name with
                | None -> Error (Format.sprintf "can't read file `%s`" name)
                | Some content -> content_identifier content
              end
              | Some Dir -> directory_identifier_deep name
              | None ->
                Error (Format.sprintf "can't get type of file `%s`" name)
            in
            let permissions = OS.permissions name in
            match (typ, permissions, target) with
            | Some typ, Some permissions, Ok target ->
              let name = OS.base name in
              let target = Object.get_core target in
              Ok { typ; permissions; target; name }
            | _ -> Error "can't compute directory deep identifier" )
          contents
      in
      match list_find_opt result_is_error entries with
      | Some (Error _ as e) -> e
      | Some _ -> assert false
      | None -> directory_identifier (List.map result_get_ok entries) )

  (* TODO: remove once we have >= 4.08 *)
  let option_map f = function None -> None | Some v -> Some (f v)

  let release_identifier target target_kind ~name ~author date ~message =
    let buff = Buffer.create 512 in
    let fmt = Format.formatter_of_buffer buff in

    Format.fprintf fmt "object %a%ctype %s%ctag %s%c" Object.Hash.pp target '\n'
      (Git.target_kind_to_git target_kind)
      '\n' (Git.escape_newlines name) '\n';

    begin
      match author with
      | None -> ()
      | Some author ->
        Format.fprintf fmt "tagger %a%c" Git.format_author_data
          ( Git.escape_newlines author
          , option_map
              (fun o -> (o.timestamp, o.tz_offset, o.negative_utc))
              date )
          '\n'
    end;

    begin
      match message with
      | None -> ()
      | Some message -> Format.fprintf fmt "%c%s" '\n' message
    end;

    Format.pp_print_flush fmt ();

    let content = Buffer.contents buff in

    let typ = Object.Kind.Release in
    let git_object = Git.object_from_contents typ content in
    Git.object_to_swhid git_object typ

  let revision_identifier directory parents ~author ~author_date ~committer
      ~committer_date extra_headers ~message =
    let buff = Buffer.create 512 in
    let fmt = Format.formatter_of_buffer buff in

    Format.fprintf fmt "tree %a%c" Object.Hash.pp directory '\n';

    List.iter
      (fun parent -> Format.fprintf fmt "parent %a%c" Object.Hash.pp parent '\n')
      parents;

    Format.fprintf fmt "author %a%c" Git.format_author_data
      ( Git.escape_newlines author
      , option_map
          (fun o -> (o.timestamp, o.tz_offset, o.negative_utc))
          author_date )
      '\n';

    Format.fprintf fmt "committer %a%c" Git.format_author_data
      ( Git.escape_newlines committer
      , option_map
          (fun o -> (o.timestamp, o.tz_offset, o.negative_utc))
          committer_date )
      '\n';

    Array.iter
      (fun (k, v) -> Format.fprintf fmt "%s %s%c" k (Git.escape_newlines v) '\n')
      extra_headers;

    begin
      match message with
      | None -> ()
      | Some message -> Format.fprintf fmt "%c%s" '\n' message
    end;

    Format.pp_print_flush fmt ();

    let content = Buffer.contents buff in

    let typ = Object.Kind.Revision in
    let git_object = Git.object_from_contents typ content in
    Git.object_to_swhid git_object typ

  let snapshot_identifier (branches : (string * (string * string) option) list)
      =
    let branches =
      List.sort
        (fun (name1, _target) (name2, _target) -> String.compare name1 name2)
        branches
    in
    let buff = Buffer.create 512 in
    let fmt = Format.formatter_of_buffer buff in
    List.iter
      (fun (branch_name, target) ->
        let target, target_kind, target_id_len =
          match target with
          | None -> ("", "dangling", 0)
          | Some (target, target_kind) -> (
            match target_kind with
            | "content" | "directory" | "revision" | "release" | "snapshot" ->
              (Git.id_to_bytes target, target_kind, 20)
            | "alias" -> (target, "alias", String.length target)
            | target_kind ->
              invalid_arg
                (Format.sprintf
                   "invalid target type: `%s` (Compute.snapshot_identifier)"
                   target_kind ) )
        in
        Format.fprintf fmt "%s %s%c%d:%s" target_kind branch_name '\x00'
          target_id_len target )
      branches;
    Format.pp_print_flush fmt ();
    let content = Buffer.contents buff in

    let git_object = Git.object_from_contents_strtarget "snapshot" content in
    Git.object_to_swhid git_object Object.Kind.Snapshot
end
