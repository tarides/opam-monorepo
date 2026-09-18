(*---------------------------------------------------------------------------
   Copyright (c) 2015 The bos programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Astring

let uerror = Unix.error_message

(* Existence, creation, deletion, contents *)

let exists = Bos_os_path.dir_exists
let must_exist = Bos_os_path.dir_must_exist
let delete = Bos_os_path.delete_dir

let create ?(path = true) ?(mode = 0o755) dir =
  let rec mkdir d mode = try Ok (Unix.mkdir (Fpath.to_string d) mode) with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok ()
  | Unix.Unix_error (e, _, _) ->
      if d = dir
      then Fmt.error_msg "create directory %a: %s" Fpath.pp d (uerror e)
      else Fmt.error_msg "create directory %a: %a: %s"
             Fpath.pp dir Fpath.pp d (uerror e)
  in
  Result.bind (Bos_os_path.exists dir) @@ function
  | true -> Result.bind (must_exist dir) @@ fun _ -> Ok false
  | false ->
      match path with
      | false -> Result.bind (mkdir dir mode) @@ fun () -> Ok true
      | true ->
          let rec dirs_to_create p acc = Result.bind (exists p) @@ function
          | true -> Ok acc
          | false -> dirs_to_create (Fpath.parent p) (p :: acc)
          in
          let rec create_them dirs () = match dirs with
          | dir :: dirs -> Result.bind (mkdir dir mode) @@ create_them dirs
          | [] -> Ok ()
          in
          Result.bind (dirs_to_create dir []) @@ fun dirs ->
          Result.bind (create_them dirs ()) @@ fun () ->
          Ok true

let rec contents ?(dotfiles = false) ?(rel = false) dir =
  let rec readdir dh acc =
    match (try Some (Unix.readdir dh) with End_of_file -> None) with
    | None -> Ok acc
    | Some (".." | ".") -> readdir dh acc
    | Some f when dotfiles || not (String.is_prefix ~affix:"." f) ->
        begin match Fpath.of_string f with
        | Ok f ->
            readdir dh ((if rel then f else Fpath.(dir // f)) :: acc)
        | Error (`Msg m) ->
            Fmt.error_msg
              "directory contents %a: cannot parse element to a path (%a)"
              Fpath.pp dir String.dump f
        end
    | Some _ -> readdir dh acc
  in
  try
    let dh = Unix.opendir (Fpath.to_string dir) in
    Bos_base.apply (readdir dh) [] ~finally:Unix.closedir dh
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> contents ~rel dir
  | Unix.Unix_error (e, _, _) ->
      Fmt.error_msg "directory contents %a: %s" Fpath.pp dir (uerror e)

let fold_contents ?err ?dotfiles ?elements ?traverse f acc d =
  Result.bind (contents d) @@
  Bos_os_path.fold ?err ?dotfiles ?elements ?traverse f acc

(* Current working directory *)

let rec current () =
  try
    let p = Unix.getcwd () in
    match Fpath.of_string p with
    | Ok dir ->
        if Fpath.is_abs dir then Ok dir else
        Fmt.error_msg "getcwd(3) returned a relative path: (%a)" Fpath.pp dir
    | Error _ ->
        Fmt.error_msg
          "get current working directory: cannot parse it to a path (%a)"
          String.dump p
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> current ()
  | Unix.Unix_error (e, _, _) ->
      Fmt.error_msg "get current working directory: %s" (uerror e)

let rec set_current dir = try Ok (Unix.chdir (Fpath.to_string dir)) with
| Unix.Unix_error (Unix.EINTR, _, _) -> set_current dir
| Unix.Unix_error (e, _, _) ->
    Fmt.error_msg "set current working directory to %a: %s"
      Fpath.pp dir (uerror e)

let with_current dir f v =
  Result.bind (current ()) @@ fun old ->
  try
    Result.bind (set_current dir) @@ fun () ->
    let ret = f v in
    Result.bind (set_current old) @@ fun () -> Ok ret
  with
  | exn -> ignore (set_current old); raise exn

(* Temporary directories *)

type tmp_name_pat = (string -> string, Format.formatter, unit, string) format4

let delete_tmp dir = ignore (delete ~recurse:true dir)
let tmps = ref Fpath.Set.empty
let tmps_add file = tmps := Fpath.Set.add file !tmps
let tmps_rem file = delete_tmp file; tmps := Fpath.Set.remove file !tmps
let delete_tmps () = Fpath.Set.iter delete_tmp !tmps
let () = at_exit delete_tmps

let default_tmp_mode = 0o700

let tmp ?(mode = default_tmp_mode) ?dir pat =
  let dir = match dir with None -> Bos_os_tmp.default_dir () | Some d -> d in
  let err () =
    Fmt.error_msg "create temporary directory %s in %a: \
                  too many failing attempts"
      (strf pat "XXXXXX") Fpath.pp dir
  in
  let rec loop count =
    if count < 0 then err () else
    let dir = Bos_os_tmp.rand_path dir pat in
    try Ok (Unix.mkdir (Fpath.to_string dir) mode; dir) with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (count - 1)
    | Unix.Unix_error (Unix.EINTR, _, _) -> loop count
    | Unix.Unix_error (e, _, _) ->
        Fmt.error_msg "create temporary directory %s in %a: %s"
          (strf pat "XXXXXX") Fpath.pp dir (uerror e)
  in
  match loop 10000 with
  | Ok dir as r -> tmps_add dir; r
  | Error _ as e -> e

let with_tmp ?mode ?dir pat f v =
  Result.bind (tmp ?mode ?dir pat) @@ fun dir ->
  try
    let ret = f dir v in
    tmps_rem dir;
    Ok ret
  with e -> tmps_rem dir; raise e

(* Default temporary directory *)

let default_tmp = Bos_os_tmp.default_dir
let set_default_tmp = Bos_os_tmp.set_default_dir

(* Base directories *)

let user_from_passwd entry =
  let home = entry.Unix.pw_dir in
  match Fpath.of_string home with
  | Ok p -> Ok p
  | Error _ ->
      Fmt.error_msg "could not parse path (%a) from passwd entry"
        String.dump home

let user () =
  let debug err = Bos_log.debug (fun m -> m "OS.Dir.user: %s" err) in
  let env_var var_name =
    Result.bind (Bos_os_env.(parse var_name (some path) ~absent:None)) @@
    function
    | Some p -> Ok p
    | None -> Fmt.error_msg "cannot determine user home directory: \
                            %s environment variable is undefined"
                            var_name
  in
  if Sys.os_type = "Win32" then
    (* We should check USERPROFILE exclusively (without HOME) because
       otherwise it could break a Windows setup with MSVC++ or MinGW.
       See the discussion at https://github.com/dbuenzli/bos/pull/96.

       On Windows with Cygwin, [Sys.os_type] would be ["Cygwin"],
       triggering the logic below.

       XXX One could use FOLDERID_Profile (or CSIDL_PROFILE for XP) to
       query the directory instead of (only) relying on the environment
       variables. *)
    env_var "USERPROFILE"
  else match env_var "HOME" with
  | Ok p -> Ok p
  | Error _ as err ->
      try
        let uid = Unix.getuid () in
        match user_from_passwd (Unix.getpwuid uid) with
        | Ok p -> Ok p
        | Error (`Msg msg) -> debug msg; err
      with
      | Unix.Unix_error (e, _, _) -> (* should not happen *)
          debug (uerror e); err
      | Not_found ->
          err

let expand_tilde p =
  let p_str = Fpath.to_string p in
  if String.head p_str <> Some '~' then Ok p else
  (* the first character is ['~'] *)
  match Fpath.segs p with
  | [] -> assert false
  | first_seg :: _ ->
      let login_name = String.with_index_range ~first:1 first_seg in
      Result.bind
        begin
          if login_name = "" then user ()
          else
          try
            (* XXX The currently code just gives up on Windows. *)
            if Sys.os_type = "Win32" then raise Not_found
            else user_from_passwd (Unix.getpwnam login_name)
          with
          | Unix.Unix_error (e, _, _) -> (* should not happen *)
              Fmt.error_msg "get passwd entry by name %s: %s"
                login_name (uerror e)
          | Not_found ->
              Fmt.error_msg "cannot determine user home directory of %s"
                login_name
        end @@ fun home ->
      let remaining_p =
        String.with_index_range ~first:(String.length first_seg) p_str
      in
      let expanded = Fpath.to_string home ^ remaining_p in
      match Fpath.of_string expanded with
      | Ok p -> Ok p
      | Error _ ->
          Fmt.error_msg "could not parse the expanded path (%a)"
            String.dump expanded

let err_dir dir fmt = Fmt.error_msg ("%s directory: " ^^ fmt) dir
let fpath_of_env_var dir var = match Bos_os_env.var var with
| None | Some "" -> None
| Some p ->
    match Fpath.of_string p with
    | Error (`Msg e) -> Some (err_dir dir "%s environment variable: %s" var e)
    | Ok _ as v -> Some v

let base_dir dir var var_alt fallback = match fpath_of_env_var dir var with
| Some r -> r
| None ->
    match Option.bind var_alt (fpath_of_env_var dir) with
    | Some r -> r
    | None -> fallback ()

let home_fallback dir sub = match user () with
| Error (`Msg e) -> err_dir dir "%s" e
| Ok home -> Ok Fpath.(home // sub)

let config_dir = "configuration"
let config_var = "XDG_CONFIG_HOME"
let config_var_alt = if Sys.win32 then Some "%APPDATA%" else None
let config_fallback () = home_fallback config_dir (Fpath.v ".config")
let config () =
  base_dir config_dir config_var config_var_alt config_fallback

let data_dir = "data"
let data_var = "XDG_DATA_HOME"
let data_var_alt = if Sys.win32 then Some "%APPDATA%" else None
let data_fallback () = home_fallback data_dir (Fpath.v ".local/share")
let data () =
  base_dir data_dir data_var data_var_alt data_fallback

let cache_dir = "cache"
let cache_var = "XDG_CACHE_HOME"
let cache_var_alt = if Sys.win32 then Some "%TEMP%" else None
let cache_fallback () = home_fallback cache_dir (Fpath.v ".cache")
let cache () =
  base_dir cache_dir cache_var cache_var_alt cache_fallback

let runtime_dir = "runtime"
let runtime_var = "XDG_RUNTIME_DIR"
let runtime_var_alt = None
let runtime_fallback () = Ok (default_tmp ())
let runtime () =
  base_dir runtime_dir runtime_var runtime_var_alt runtime_fallback

let state_dir = "state"
let state_var = "XDG_STATE_DIR"
let state_var_alt = None
let state_fallback () = home_fallback state_dir (Fpath.v ".local/state")
let state () = base_dir state_dir state_var state_var_alt state_fallback

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The bos programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
