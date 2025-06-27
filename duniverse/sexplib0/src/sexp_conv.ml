(* Utility Module for S-expression Conversions *)

open StdLabels
open MoreLabels
open Printf
open Sexp

(* Conversion of OCaml-values to S-expressions *)
external format_float : string -> float -> string = "caml_format_float"

(* '%.17g' is guaranteed to be round-trippable.

   '%.15g' will be round-trippable and not have noise at the last digit or two for a float
   which was converted from a decimal (string) with <= 15 significant digits.  So it's
   worth trying first to avoid things like "3.1400000000000001".

   See comment above [to_string_round_trippable] in {!Core.Float} for
   detailed explanation and examples. *)
let default_string_of_float =
  ref (fun x ->
    let y = format_float "%.15G" x in
    if float_of_string y = x then y else format_float "%.17G" x)
;;

let read_old_option_format = ref true
let write_old_option_format = ref true
let list_map f l = List.rev (List.rev_map l ~f)
let sexp_of_unit () = List []
let sexp_of_bool b = Atom (string_of_bool b)
let sexp_of_string str = Atom str
let sexp_of_bytes bytes = Atom (Bytes.to_string bytes)
let sexp_of_char c = Atom (String.make 1 c)
let sexp_of_int n = Atom (string_of_int n)
let sexp_of_float n = Atom (!default_string_of_float n)
let sexp_of_int32 n = Atom (Int32.to_string n)
let sexp_of_int64 n = Atom (Int64.to_string n)
let sexp_of_nativeint n = Atom (Nativeint.to_string n)
let sexp_of_ref sexp_of__a rf = sexp_of__a !rf
let sexp_of_lazy_t sexp_of__a lv = sexp_of__a (Lazy.force lv)

let sexp_of_option sexp_of__a = function
  | Some x when !write_old_option_format -> List [ sexp_of__a x ]
  | Some x -> List [ Atom "some"; sexp_of__a x ]
  | None when !write_old_option_format -> List []
  | None -> Atom "none"
;;

let sexp_of_pair sexp_of__a sexp_of__b (a, b) = List [ sexp_of__a a; sexp_of__b b ]

let sexp_of_triple sexp_of__a sexp_of__b sexp_of__c (a, b, c) =
  List [ sexp_of__a a; sexp_of__b b; sexp_of__c c ]
;;

(* List.rev (List.rev_map ...) is tail recursive, the OCaml standard
   library List.map is NOT. *)
let sexp_of_list sexp_of__a lst = List (List.rev (List.rev_map lst ~f:sexp_of__a))

let sexp_of_array sexp_of__a ar =
  let lst_ref = ref [] in
  for i = Array.length ar - 1 downto 0 do
    lst_ref := sexp_of__a ar.(i) :: !lst_ref
  done;
  List !lst_ref
;;

let sexp_of_hashtbl sexp_of_key sexp_of_val htbl =
  let coll ~key:k ~data:v acc = List [ sexp_of_key k; sexp_of_val v ] :: acc in
  List (Hashtbl.fold htbl ~init:[] ~f:coll)
;;

let sexp_of_opaque _ = Atom "<opaque>"
let sexp_of_fun _ = Atom "<fun>"

(* Exception converter registration and lookup *)

module Exn_converter = struct
  (* These exception registration functions assume that context-switches
     cannot happen unless there is an allocation.  It is reasonable to expect
     that this will remain true for the foreseeable future.  That way we
     avoid using mutexes and thus a dependency on the threads library. *)

  (* Fast and automatic exception registration *)

  module Registration = struct
    type t =
      { sexp_of_exn : exn -> Sexp.t
      ; (* If [printexc = true] then this sexp converter is used for Printexc.to_string *)
        printexc : bool
      }
  end

  module Exn_table = Ephemeron.K1.Make (struct
      type t = extension_constructor

      let equal = ( == )
      let hash = Obj.Extension_constructor.id
    end)

  let the_exn_table : Registration.t Exn_table.t = Exn_table.create 17

  (* Ephemerons are used so that [sexp_of_exn] closure don't keep the
     extension_constructor live. *)
  let add ?(printexc = true) ?finalise:_ extension_constructor sexp_of_exn =
    Exn_table.add the_exn_table extension_constructor { sexp_of_exn; printexc }
  ;;

  let find_auto ~for_printexc exn =
    let extension_constructor = Obj.Extension_constructor.of_val exn in
    match Exn_table.find_opt the_exn_table extension_constructor with
    | None -> None
    | Some { sexp_of_exn; printexc } ->
      (match for_printexc, printexc with
       | false, _ | _, true -> Some (sexp_of_exn exn)
       | true, false -> None)
  ;;

  module For_unit_tests_only = struct
    let size () = (Exn_table.stats_alive the_exn_table).num_bindings
  end
end

let sexp_of_exn_opt_for_printexc exn = Exn_converter.find_auto ~for_printexc:true exn
let sexp_of_exn_opt exn = Exn_converter.find_auto ~for_printexc:false exn

let sexp_of_exn exn =
  match sexp_of_exn_opt exn with
  | None -> List [ Atom (Printexc.to_string exn) ]
  | Some sexp -> sexp
;;

let exn_to_string e = Sexp.to_string_hum (sexp_of_exn e)

(* {[exception Blah [@@deriving sexp]]} generates a call to the function
   [Exn_converter.add] defined in this file.  So we are guaranted that as soon as we
   mark an exception as sexpable, this module will be linked in and this printer will be
   registered, which is what we want. *)
let () =
  Printexc.register_printer (fun exn ->
    match sexp_of_exn_opt_for_printexc exn with
    | None -> None
    | Some sexp -> Some (Sexp.to_string_hum ~indent:2 sexp))
;;

let printexc_prefer_sexp exn =
  match sexp_of_exn_opt exn with
  | None -> Printexc.to_string exn
  | Some sexp -> Sexp.to_string_hum ~indent:2 sexp
;;

(* Conversion of S-expressions to OCaml-values *)

exception Of_sexp_error = Sexp.Of_sexp_error

let record_check_extra_fields = ref true
let of_sexp_error_exn exc sexp = raise (Of_sexp_error (exc, sexp))
let of_sexp_error what sexp = raise (Of_sexp_error (Failure what, sexp))

let unit_of_sexp sexp =
  match sexp with
  | List [] -> ()
  | Atom _ | List _ -> of_sexp_error "unit_of_sexp: empty list needed" sexp
;;

let bool_of_sexp sexp =
  match sexp with
  | Atom ("true" | "True") -> true
  | Atom ("false" | "False") -> false
  | Atom _ -> of_sexp_error "bool_of_sexp: unknown string" sexp
  | List _ -> of_sexp_error "bool_of_sexp: atom needed" sexp
;;

let string_of_sexp sexp =
  match sexp with
  | Atom str -> str
  | List _ -> of_sexp_error "string_of_sexp: atom needed" sexp
;;

let bytes_of_sexp sexp =
  match sexp with
  | Atom str -> Bytes.of_string str
  | List _ -> of_sexp_error "bytes_of_sexp: atom needed" sexp
;;

let char_of_sexp sexp =
  match sexp with
  | Atom str ->
    if String.length str <> 1
    then of_sexp_error "char_of_sexp: atom string must contain one character only" sexp;
    str.[0]
  | List _ -> of_sexp_error "char_of_sexp: atom needed" sexp
;;

let int_of_sexp sexp =
  match sexp with
  | Atom str ->
    (try int_of_string str with
     | exc -> of_sexp_error ("int_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "int_of_sexp: atom needed" sexp
;;

let float_of_sexp sexp =
  match sexp with
  | Atom str ->
    (try float_of_string str with
     | exc -> of_sexp_error ("float_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "float_of_sexp: atom needed" sexp
;;

let int32_of_sexp sexp =
  match sexp with
  | Atom str ->
    (try Int32.of_string str with
     | exc -> of_sexp_error ("int32_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "int32_of_sexp: atom needed" sexp
;;

let int64_of_sexp sexp =
  match sexp with
  | Atom str ->
    (try Int64.of_string str with
     | exc -> of_sexp_error ("int64_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "int64_of_sexp: atom needed" sexp
;;

let nativeint_of_sexp sexp =
  match sexp with
  | Atom str ->
    (try Nativeint.of_string str with
     | exc -> of_sexp_error ("nativeint_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "nativeint_of_sexp: atom needed" sexp
;;

let ref_of_sexp a__of_sexp sexp = ref (a__of_sexp sexp)
let lazy_t_of_sexp a__of_sexp sexp = Lazy.from_val (a__of_sexp sexp)

let option_of_sexp a__of_sexp sexp =
  if !read_old_option_format
  then (
    match sexp with
    | List [] | Atom ("none" | "None") -> None
    | List [ el ] | List [ Atom ("some" | "Some"); el ] -> Some (a__of_sexp el)
    | List _ -> of_sexp_error "option_of_sexp: list must represent optional value" sexp
    | Atom _ -> of_sexp_error "option_of_sexp: only none can be atom" sexp)
  else (
    match sexp with
    | Atom ("none" | "None") -> None
    | List [ Atom ("some" | "Some"); el ] -> Some (a__of_sexp el)
    | Atom _ -> of_sexp_error "option_of_sexp: only none can be atom" sexp
    | List _ -> of_sexp_error "option_of_sexp: list must be (some el)" sexp)
;;

let pair_of_sexp a__of_sexp b__of_sexp sexp =
  match sexp with
  | List [ a_sexp; b_sexp ] ->
    let a = a__of_sexp a_sexp in
    let b = b__of_sexp b_sexp in
    a, b
  | List _ ->
    of_sexp_error "pair_of_sexp: list must contain exactly two elements only" sexp
  | Atom _ -> of_sexp_error "pair_of_sexp: list needed" sexp
;;

let triple_of_sexp a__of_sexp b__of_sexp c__of_sexp sexp =
  match sexp with
  | List [ a_sexp; b_sexp; c_sexp ] ->
    let a = a__of_sexp a_sexp in
    let b = b__of_sexp b_sexp in
    let c = c__of_sexp c_sexp in
    a, b, c
  | List _ ->
    of_sexp_error "triple_of_sexp: list must contain exactly three elements only" sexp
  | Atom _ -> of_sexp_error "triple_of_sexp: list needed" sexp
;;

let list_of_sexp a__of_sexp sexp =
  match sexp with
  | List lst ->
    let rev_lst = List.rev_map lst ~f:a__of_sexp in
    List.rev rev_lst
  | Atom _ -> of_sexp_error "list_of_sexp: list needed" sexp
;;

let array_of_sexp a__of_sexp sexp =
  match sexp with
  | List [] -> [||]
  | List (h :: t) ->
    let len = List.length t + 1 in
    let res = Array.make len (a__of_sexp h) in
    let rec loop i = function
      | [] -> res
      | h :: t ->
        res.(i) <- a__of_sexp h;
        loop (i + 1) t
    in
    loop 1 t
  | Atom _ -> of_sexp_error "array_of_sexp: list needed" sexp
;;

let hashtbl_of_sexp key_of_sexp val_of_sexp sexp =
  match sexp with
  | List lst ->
    let htbl = Hashtbl.create 0 in
    let act = function
      | List [ k_sexp; v_sexp ] ->
        Hashtbl.add htbl ~key:(key_of_sexp k_sexp) ~data:(val_of_sexp v_sexp)
      | List _ | Atom _ -> of_sexp_error "hashtbl_of_sexp: tuple list needed" sexp
    in
    List.iter lst ~f:act;
    htbl
  | Atom _ -> of_sexp_error "hashtbl_of_sexp: list needed" sexp
;;

let opaque_of_sexp sexp =
  of_sexp_error "opaque_of_sexp: cannot convert opaque values" sexp
;;

let fun_of_sexp sexp = of_sexp_error "fun_of_sexp: cannot convert function values" sexp

(* Sexp Grammars *)

include Sexp_conv_grammar

(* Registering default exception printers *)

let get_flc_error name (file, line, chr) = Atom (sprintf "%s %s:%d:%d" name file line chr)

let () =
  List.iter
    ~f:(fun (extension_constructor, handler) ->
      Exn_converter.add ~printexc:false ~finalise:false extension_constructor handler)
    [ ( [%extension_constructor Assert_failure]
      , function
        | Assert_failure arg -> get_flc_error "Assert_failure" arg
        | _ -> assert false )
    ; ( [%extension_constructor Exit]
      , function
        | Exit -> Atom "Exit"
        | _ -> assert false )
    ; ( [%extension_constructor End_of_file]
      , function
        | End_of_file -> Atom "End_of_file"
        | _ -> assert false )
    ; ( [%extension_constructor Failure]
      , function
        | Failure arg -> List [ Atom "Failure"; Atom arg ]
        | _ -> assert false )
    ; ( [%extension_constructor Not_found]
      , function
        | Not_found -> Atom "Not_found"
        | _ -> assert false )
    ; ( [%extension_constructor Invalid_argument]
      , function
        | Invalid_argument arg -> List [ Atom "Invalid_argument"; Atom arg ]
        | _ -> assert false )
    ; ( [%extension_constructor Match_failure]
      , function
        | Match_failure arg -> get_flc_error "Match_failure" arg
        | _ -> assert false )
    ; ( [%extension_constructor Not_found_s]
      , function
        | Not_found_s arg -> List [ Atom "Not_found_s"; arg ]
        | _ -> assert false )
    ; ( [%extension_constructor Sys_error]
      , function
        | Sys_error arg -> List [ Atom "Sys_error"; Atom arg ]
        | _ -> assert false )
    ; ( [%extension_constructor Arg.Help]
      , function
        | Arg.Help arg -> List [ Atom "Arg.Help"; Atom arg ]
        | _ -> assert false )
    ; ( [%extension_constructor Arg.Bad]
      , function
        | Arg.Bad arg -> List [ Atom "Arg.Bad"; Atom arg ]
        | _ -> assert false )
    ; ( [%extension_constructor Lazy.Undefined]
      , function
        | Lazy.Undefined -> Atom "Lazy.Undefined"
        | _ -> assert false )
    ; ( [%extension_constructor Parsing.Parse_error]
      , function
        | Parsing.Parse_error -> Atom "Parsing.Parse_error"
        | _ -> assert false )
    ; ( [%extension_constructor Queue.Empty]
      , function
        | Queue.Empty -> Atom "Queue.Empty"
        | _ -> assert false )
    ; ( [%extension_constructor Scanf.Scan_failure]
      , function
        | Scanf.Scan_failure arg -> List [ Atom "Scanf.Scan_failure"; Atom arg ]
        | _ -> assert false )
    ; ( [%extension_constructor Stack.Empty]
      , function
        | Stack.Empty -> Atom "Stack.Empty"
        | _ -> assert false )
    ; ( [%extension_constructor Sys.Break]
      , function
        | Sys.Break -> Atom "Sys.Break"
        | _ -> assert false )
    ]
;;

let () =
  List.iter
    ~f:(fun (extension_constructor, handler) ->
      Exn_converter.add ~printexc:true ~finalise:false extension_constructor handler)
    [ ( [%extension_constructor Of_sexp_error]
      , function
        | Of_sexp_error (exc, sexp) ->
          List [ Atom "Sexplib.Conv.Of_sexp_error"; sexp_of_exn exc; sexp ]
        | _ -> assert false )
    ]
;;

external ignore : _ -> unit = "%ignore"
external ( = ) : 'a -> 'a -> bool = "%equal"
