(* Copyright 2024-2025 Kate Deplaix   *)
(* Copyright 2025 Samuel Hym, Tarides *)
(* SPDX-License-Identifier: ISC       *)

let errf fmt = Printf.ksprintf invalid_arg fmt
let read path = In_channel.(with_open_bin path input_all)

let write ~create path content =
  let flags =
    Open_wronly :: Open_binary
    :: (if create then [ Open_creat; Open_excl ] else [ Open_trunc ])
  in
  Out_channel.(
    with_open_gen flags 0o666 path (fun oc -> output_string oc content))

let remove ~bound path =
  let bound = Filename.concat bound "" in
  let rec rec_rmdir p =
    try
      let p = Filename.dirname p in
      if String.starts_with ~prefix:bound p then (
        Sys.rmdir p;
        rec_rmdir p)
    with _ -> ()
  in
  Unix.unlink path;
  (* FIXME? Opam's original code makes extra efforts to unlink on Windows if the
     first attempt fails (namely it retries after enabling read&write access);
     is that necessary here? *)
  rec_rmdir path

let patch ~force ~patchname content diff =
  (* NOTE: The None case returned by [Patch.patch] is only returned
       if [diff = Patch.Delete _]. This sub-function is not called in
       this case so we [assert false] instead. *)
  match Patch.patch ~cleanly:true content diff with
  | Some x -> x
  | None -> assert false (* See NOTE above *)
  | exception _ when not force ->
      errf "Patch %S does not apply cleanly" patchname
  | exception _ -> (
      match Patch.patch ~cleanly:false content diff with
      | Some x -> x
      | None -> assert false (* See NOTE above *)
      | exception _ -> errf "Patch %S does not apply" patchname)

let apply ~force ~dir ~patchname diffs =
  let ( / ) = Filename.concat in
  (* NOTE: It is important to keep this `concat dir ""` to ensure the
     is_prefix_of below doesn't match another similarly named directory *)
  let basedir = Unix.realpath dir in
  let dir = basedir / "" in
  let in_scope orig_path path =
    if (not (String.starts_with ~prefix:dir path)) && basedir <> path then
      errf "Patch tried to escape its scope to reach %S, out of %S (%S)"
        orig_path dir path
  in
  let fullpath path =
    if not (Filename.is_relative path) then errf "Path %S is not relative" path;
    dir / path
  and check_and_mkdir path =
    let rec aux d =
      if Sys.file_exists d then in_scope path (Unix.realpath d)
      else (
        aux (Filename.dirname d);
        if Sys.file_exists d then
          (* Note d could already exists if the path is abc/.. and the recursive
             call just created abc *)
          in_scope path (Unix.realpath d)
        else Sys.mkdir d 0o777)
    in
    aux path
  in
  let get_src path =
    let fpath = fullpath path in
    if not (Sys.file_exists fpath) then errf "File %S doesn't exist" path;
    let fpath = Unix.realpath fpath in
    in_scope path fpath;
    fpath
  and get_dst ~create path =
    let fpath = fullpath path in
    if create then (
      if Sys.file_exists fpath then errf "File %S exists" path
      else
        let d = Filename.dirname fpath in
        check_and_mkdir d;
        Unix.realpath d / Filename.basename fpath)
    else Unix.realpath fpath
  in
  let apply diff =
    match diff.Patch.operation with
    | Patch.Edit (src, dst) ->
        let create = src <> dst in
        let src = get_src src in
        (* see note about [Edit] operations below *)
        let content = read src in
        let content = patch ~force ~patchname (Some content) diff in
        let dst = get_dst ~create dst in
        write ~create dst content;
        if create then remove ~bound:dir src
    | Patch.Delete file | Patch.Git_ext (file, _, Patch.Delete_only) ->
        let file = get_src file in
        remove ~bound:dir file
    | Patch.Create file | Patch.Git_ext (_, file, Patch.Create_only) ->
        let content = patch ~force ~patchname None diff in
        let file = get_dst ~create:true file in
        write ~create:true file content
    | Patch.Git_ext (_, _, Patch.Rename_only (src, dst)) ->
        assert (src <> dst);
        let src = get_src src in
        let dst = get_dst ~create:true dst in
        (* see note about [Rename_only] operations below *)
        Unix.rename src dst;
        remove ~bound:dir src
  in
  List.iter apply diffs

(* NOTE: About [Edit] operations
   Opam's original code to apply patches can deal with [Edit] operations where
   the source doesn't exist but the destination does: in that case it patches
   the destination directly, mimicking GNU patch. That behaviour is not accepted
   by [get_src], which errors out on non-existing files. My intuition here is
   that such a patch is erroneous in the first place. opatch is a different
   position compared to opam: where opam replaced GNU patch with internal
   patching code, opatch could depart from GNU patch on such weird cases. *)

(* NOTE: About [Rename_only] operations
   The way [Rename_only] operations are handled will fail on a patch that moves
   [x] into [x/y], aka when the original file name is becoming a directory on
   the fly ([get_dst] will try to create the directory while the original file
   is still in place). While [git apply] can handle such patches, GNU patch
   rejects them, so we probably can fail too. *)

let apply ~dir ~strip patch =
  let patchname, content =
    let open In_channel in
    match patch with
    | None -> ("-", input_all stdin)
    | Some path -> (path, with_open_bin path input_all)
  in
  let diffs = Patch.parse ~p:strip content in
  apply ~force:false ~dir ~patchname diffs

let parse_argv version argv =
  let open Arg in
  let strip = ref 1
  and dir = ref "."
  and patches = ref []
  and verbose = ref false in
  let add_patch special = function
    | "-" when special -> patches := None :: !patches
    | x -> patches := Some x :: !patches
  and show_version () =
    Printf.printf "opatch %s\n" version;
    exit 0
  in
  let specs =
    [
      ( "-p",
        Arg.Set_int strip,
        "<NUM>  Strip <NUM> directories from the diff paths (default: 1)" );
      ( "-C",
        Arg.Set_string dir,
        "<DIR>  Locate files to patch as if launched in <DIR> instead of ." );
      ( "-v",
        Arg.Set verbose,
        " Set verbose mode, where applied patches are logged" );
      ("-version", Arg.Unit show_version, " Print version and exit");
      ("--version", Arg.Unit show_version, " Print version and exit");
      ( "--",
        Arg.Rest (add_patch false),
        " Process all remaining arguments as patches" );
    ]
  and usage = "opatch [-C <DIR>] [-p <NUM>] [PATCH...]: apply a diff file" in
  try
    parse_argv ~current:(ref 0) argv specs (add_patch true) usage;
    ( !strip,
      !dir,
      (match !patches with [] -> [ None ] | p -> List.rev p),
      !verbose )
  with
  | Help msg ->
      Printf.printf "%s" msg;
      exit 0
  | Bad msg ->
      Printf.eprintf "%s" msg;
      exit 1

let main version =
  let strip, dir, patches, verbose = parse_argv version Sys.argv in
  try
    List.iter
      (fun patch ->
        apply ~dir ~strip patch;
        if verbose then
          Printf.printf "%S applied.\n%!" (Option.value ~default:"-" patch))
      patches
  with Invalid_argument msg ->
    Printf.eprintf "Fatal error: %s\n" msg ; exit 1

let () = main Version.v
