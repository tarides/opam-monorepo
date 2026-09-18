open Core
open Poly

module Unix = Caml_unix

let data_multi =
  let data_fn = "data.sexp" in
  if Stdlib.Sys.file_exists data_fn
  then sprintf "(%s)" (In_channel.read_all data_fn)
  else (
    (* Collect all the jbuilds in $ROOT/lib *)
    let ic = Unix.open_process_in "find ../../../lib -name jbuild -exec cat {} \\;" in
    let s = In_channel.input_all ic in
    assert (Unix.close_process_in ic = WEXITED 0);
    sprintf "%s" s)
;;

let data = sprintf "(%s)" data_multi

(* To make sure the input is valid before starting the bench *)
let (_ : Sexp.t) = Sexplib.Sexp.of_string data
let _, positions = Parsexp.Single_and_positions.parse_string_exn data

let () =
  let pos_mem_kb = Parsexp.Positions.memory_footprint_in_bytes positions / 1024 in
  printf
    "input size: %d KB\nposition set size: %d KB (%d KW)\n%!"
    (String.length data / 1024)
    pos_mem_kb
    (pos_mem_kb
     /
     match Word_size.word_size with
     | W32 -> 4
     | W64 -> 8)
;;

(* Obj.tag is a C call so the compiler can't consider the value as dead-code *)
let don't_optimize_out x = ignore (Obj.tag (Obj.repr x) : int)
let%bench "sexplib" = don't_optimize_out (Sexplib.Sexp.of_string data : Sexp.t)

let%bench "sexplib.annotated" =
  don't_optimize_out (Sexplib.Sexp.Annotated.of_string data : Sexp.Annotated.t)
;;

let%bench "parsexp" = don't_optimize_out (Parsexp.Single.parse_string_exn data : Sexp.t)

let%bench "parsexp+positions" =
  don't_optimize_out (Parsexp.Single_and_positions.parse_string_exn data : _ * _)
;;

let eager_parse_sexps str ~f =
  let module P = Parsexp.Eager in
  let results = Queue.create () in
  let got_sexp _state sexp = Core.Queue.enqueue results (f sexp) in
  let state = P.State.create got_sexp ~no_sexp_is_error:false in
  let stack = P.feed_string state str P.Stack.empty in
  P.feed_eoi state stack;
  Queue.to_list results
;;

let%bench "parsexp eager" =
  don't_optimize_out
    (eager_parse_sexps data_multi ~f:(fun (_ : Sexp.t) -> ()) : unit list)
;;
