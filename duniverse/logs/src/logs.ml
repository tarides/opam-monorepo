(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let rec atomic_list_cons v atomic =
  let l = Atomic.get atomic in
  if Atomic.compare_and_set atomic l (v :: l) then () else
  atomic_list_cons v atomic

(* Reporting levels *)

type level = App | Error | Warning | Info | Debug
let level' = Atomic.make (Some Warning)
let level () = Atomic.get level'
let pp_level ppf = function
| App -> ()
| Error -> Format.pp_print_string ppf "ERROR"
| Warning -> Format.pp_print_string ppf "WARNING"
| Info -> Format.pp_print_string ppf "INFO"
| Debug -> Format.pp_print_string ppf "DEBUG"

let level_to_string = function
| None -> "quiet" | Some App -> "app" | Some Error -> "error"
| Some Warning -> "warning" | Some Info -> "info" | Some Debug -> "debug"

let level_of_string = function
| "quiet" -> Ok None
| "app" -> Ok (Some App)
| "error" -> Ok (Some Error)
| "warning" -> Ok (Some Warning)
| "info" -> Ok (Some Info)
| "debug" -> Ok (Some Debug)
| l -> Error (`Msg (Printf.sprintf "%S: unknown log level" l))

(* Sources *)

module Src = struct
  type t =
    { uid : int;
      name : string;
      doc : string;
      level : level option Atomic.t }

  let uid =
    let id = Atomic.make 0 in
    fun () -> Atomic.fetch_and_add id 1

  let list = Atomic.make []

  let create ?(doc = "undocumented") name =
    let level = Atomic.make (Atomic.get level') in
    let src = { uid = uid (); name; doc; level } in
    atomic_list_cons src list; src

  let name s = s.name
  let doc s = s.doc
  let level s = Atomic.get (s.level)
  let set_level s l = Atomic.set s.level l
  let equal src0 src1 = src0.uid = src1.uid
  let compare src0 src1 = (compare : int -> int -> int) src0.uid src1.uid

  let pp ppf src = Format.fprintf ppf
      "@[<1>(src@ @[<1>(name %S)@]@ @[<1>(uid %d)@] @[<1>(doc %S)@])@]"
      src.name src.uid src.doc

  let list () = Atomic.get list
end

type src = Src.t

let default = Src.create "application" ~doc:"The application log"

let set_level ?(all = true) l =
  Atomic.set level' l;
  if all then List.iter (fun s -> Src.set_level s l) (Src.list ())

(* Message tags *)

module Tag = struct

  (* Universal type, see http://mlton.org/UniversalType.
     Note: we can get rid of that once we have OCaml >= 5.1 *)

  type univ = exn
  let univ (type s) () =
    let module M = struct exception E of s option end in
    (fun x -> M.E (Some x)), (function M.E x -> x | _ -> None)

  (* Tag definitions *)

  type 'a def =
    { uid : int;
      to_univ : 'a -> univ;
      of_univ : univ -> 'a option;
      name : string;
      doc : string;
      pp : Format.formatter -> 'a -> unit; }

  type def_e = Def : 'a def -> def_e

  let list = Atomic.make ([] : def_e list)
  let uid =
    let id = Atomic.make 0 in
    fun () -> Atomic.fetch_and_add id 1

  let def ?(doc = "undocumented") name pp =
    let to_univ, of_univ = univ () in
    let tag = { uid = uid (); to_univ; of_univ; name; doc; pp } in
    atomic_list_cons (Def tag) list;
    tag

  let name d = d.name
  let doc d = d.doc
  let printer d = d.pp
  let pp_def ppf d = Format.fprintf ppf "tag:%s" d.name
  let list () = Atomic.get list

  (* Tag values *)

  type t = V : 'a def * 'a -> t

  let pp ppf (V (d, v)) =
    Format.fprintf ppf "@[<1>(%a@ @[%a@])@]" pp_def d d.pp v

  (* Tag sets *)

  module Key = struct
    type t = V : 'a def -> t
    let compare (V k0) (V k1) = (compare : int -> int -> int) k0.uid k1.uid
  end

  module M = Map.Make (Key)

  type set = t M.t

  let empty = M.empty
  let is_empty = M.is_empty
  let mem k s = M.mem (Key.V k) s
  let add k v s = M.add (Key.V k) (V (k, v)) s
  let rem k s = M.remove (Key.V k) s
  let find : type a. a def -> set -> a option =
  fun k s ->
    try match M.find (Key.V k) s with
    | V (k', v) -> k.of_univ (k'.to_univ v)
    with Not_found -> None

  let get k s = match find k s with
  | None -> invalid_arg (Printf.sprintf "tag named %s not found in set" k.name)
  | Some v -> v

  let fold f s acc = M.fold (fun _ t acc -> f t acc) s acc
  let pp_set ppf s =
    let pp_tag tag is_first =
      if is_first then () else Format.fprintf ppf "@,";
      Format.fprintf ppf "%a" pp tag;
      false
    in
    Format.fprintf ppf "@[<1>{";
    ignore (fold pp_tag s true);
    Format.fprintf ppf "}@]";
    ()
end

(* Reporters *)

type ('a, 'b) msgf =
  (?header:string -> ?tags:Tag.set ->
   ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

type reporter_mutex = { lock : unit -> unit; unlock : unit -> unit }
let reporter_mutex' =
  Atomic.make { lock = (fun () -> ()); unlock = (fun () -> ()) }

let set_reporter_mutex ~lock ~unlock =
  Atomic.set reporter_mutex' { lock; unlock }

type reporter =
  { report :
      'a 'b. src -> level -> over:(unit -> unit) -> (unit -> 'b) ->
      ('a, 'b) msgf -> 'b }

let nop_reporter = { report = fun _ _ ~over k _ -> over (); k () }
let reporter' = Atomic.make nop_reporter
let set_reporter r = Atomic.set reporter' r
let reporter () = Atomic.get reporter'
let report src level ~over k msgf =
  let mutex = Atomic.get reporter_mutex' in
  let over () = over (); mutex.unlock () in
  mutex.lock ();
  try (Atomic.get reporter').report src level ~over k msgf with
  | exn ->
      let bt = Printexc.get_raw_backtrace ()  in
      over ();
      Printexc.raise_with_backtrace exn bt

let pp_brackets pp_v ppf v =
  Format.pp_print_char ppf '['; pp_v ppf v; Format.pp_print_char ppf ']'

let pp_header ppf (l, h) = match h with
| None -> if l = App then () else pp_brackets pp_level ppf l
| Some h -> pp_brackets Format.pp_print_string ppf h

let pp_exec_header =
  let exec = match Array.length Sys.argv with
  | 0 -> Filename.basename Sys.executable_name
  | n -> Filename.basename Sys.argv.(0)
  in
  fun ppf (l, h) ->
    if l = App then match h with
    | None -> ()
    | Some h ->
        pp_brackets Format.pp_print_string ppf h;
        Format.pp_print_char ppf ' '
    else match h with
    | None ->
        Format.pp_print_string ppf exec;
        Format.pp_print_string ppf ": ";
        pp_brackets pp_level ppf l;
        Format.pp_print_char ppf ' '
    | Some h ->
        Format.pp_print_string ppf exec;
        Format.pp_print_string ppf ": ";
        pp_brackets Format.pp_print_string ppf h;
        Format.pp_print_char ppf ' '

let format_reporter
    ?(pp_header = pp_exec_header)
    ?(app = Format.std_formatter)
    ?(dst = Format.err_formatter) ()
  =
  let report src level ~over k msgf =
    let k ppf =
      Format.pp_close_box ppf ();
      Format.pp_print_newline ppf ();
      over (); k ()
    in
    msgf @@ fun ?header ?tags fmt ->
    let ppf = if level = App then app else dst in
    pp_header ppf (level, header);
    Format.pp_open_box ppf 0;
    Format.kfprintf k ppf fmt
  in
  { report }

(* Log functions *)

let err_count' = Atomic.make 0
let err_count () = Atomic.get err_count'
let incr_err_count () = Atomic.incr err_count'

let warn_count' = Atomic.make 0
let warn_count () = Atomic.get warn_count'
let incr_warn_count () = Atomic.incr warn_count'

type 'a log = ('a, unit) msgf -> unit

let over () = ()
let kmsg k ?(src = default) level msgf =
  begin match level with
  | Error -> Atomic.incr err_count'
  | Warning -> Atomic.incr warn_count'
  | _ -> ()
  end;
  match Src.level src with
  | None -> k ()
  | Some current_level when level > current_level -> k ()
  | Some _ -> report src level ~over k msgf

let kunit _ = ()
let msg ?src level msgf = kmsg kunit ?src level msgf
let app ?src msgf = kmsg kunit ?src App msgf
let err ?src msgf = kmsg kunit ?src Error msgf
let warn ?src msgf = kmsg kunit ?src Warning msgf
let info ?src msgf = kmsg kunit ?src Info msgf
let debug ?src msgf = kmsg kunit ?src Debug msgf

(* Log result errors *)

let on_error ?src ?(level = Error) ?header ?tags ~pp ~use = function
| Ok v -> v
| Error e ->
    kmsg (fun () -> use e) ?src level @@ fun m ->
    m ?header ?tags "@[%a@]" pp e

let on_error_msg ?src ?(level = Error) ?header ?tags ~use = function
| Ok v -> v
| Error (`Msg msg) ->
    kmsg use ?src level @@ fun m ->
    m ?header ?tags "@[%a@]" Format.pp_print_text msg

(* Source specific logging functions *)

module type LOG = sig
  val msg : level -> 'a log
  val app : 'a log
  val err : 'a log
  val warn : 'a log
  val info : 'a log
  val debug : 'a log
  val kmsg : (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b
  val on_error :
    ?level:level -> ?header:string -> ?tags:Tag.set ->
    pp:(Format.formatter -> 'b -> unit) -> use:('b -> 'a) -> ('a, 'b) result ->
    'a

  val on_error_msg :
    ?level:level -> ?header:string -> ?tags:Tag.set ->
    use:(unit -> 'a) -> ('a, [`Msg of string]) result -> 'a
end

let src_log src =
  let module Log = struct
    let msg level msgf = msg ~src level msgf
    let kmsg k level msgf = kmsg k ~src level msgf
    let app msgf = msg App msgf
    let err msgf = msg Error msgf
    let warn msgf = msg Warning msgf
    let info msgf = msg Info msgf
    let debug msgf = msg Debug msgf
    let on_error ?level ?header ?tags ~pp ~use =
      on_error ~src ?level ?header ?tags ~pp ~use

    let on_error_msg ?level ?header ?tags ~use =
      on_error_msg ~src ?level ?header ?tags ~use
  end
  in
  (module Log : LOG)
