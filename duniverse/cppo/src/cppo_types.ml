open Printf
open Lexing

module String_set = Set.Make (String)
module String_map = Map.Make (String)

type loc = position * position

(* The name of a macro. *)
type macro =
  string

(* The shape of a macro.

   The abstract syntax of shapes is τ ::= [τ, ..., τ].
   That is, a macro takes a tuple of parameters, each
   of which has a shape. The length of of this tuple
   can be zero: this is the base case. *)
type shape =
  | Shape of shape list

(* Printing a shape. This code must be consistent with the shape
   parser in [Cppo_lexer]. *)

let rec print_shape (Shape shs) =
  match shs with
  | [] ->
      (* As a special case, the base shape is ".". *)
      "."
  | _ ->
      "[" ^ String.concat "" (List.map print_shape shs) ^ "]"

(* Testing two shapes for equality. *)

let same_shape : shape -> shape -> bool =
  (=)

(* The base shape. This is the shape of a basic macro,
   which takes no parameters, and produces text. *)
let base = Shape []

type bool_expr =
    [ `True
    | `False
    | `Defined of macro
    | `Not of bool_expr (* not *)
    | `And of (bool_expr * bool_expr) (* && *)
    | `Or of (bool_expr * bool_expr) (* || *)
    | `Eq of (arith_expr * arith_expr) (* = *)
    | `Lt of (arith_expr * arith_expr) (* < *)
    | `Gt of (arith_expr * arith_expr) (* > *)
        (* syntax for additional operators: <>, <=, >= *)
    ]

and arith_expr = (* signed int64 *)
    [ `Int of int64
    | `Ident of (loc * string)
        (* must be bound to a valid int literal.
           Expansion of macro functions is not supported. *)

    | `Tuple of (loc * arith_expr list)
        (* tuple of 2 or more elements guaranteed by the syntax *)

    | `Neg of arith_expr (* - *)
    | `Add of (arith_expr * arith_expr) (* + *)
    | `Sub of (arith_expr * arith_expr) (* - *)
    | `Mul of (arith_expr * arith_expr) (* * *)
    | `Div of (loc * arith_expr * arith_expr) (* / *)
    | `Mod of (loc * arith_expr * arith_expr) (* mod *)

    (* Bitwise operations on 64 bits *)
    | `Lnot of arith_expr (* lnot *)
    | `Lsl of (arith_expr * arith_expr) (* lsl *)
    | `Lsr of (arith_expr * arith_expr) (* lsr *)
    | `Asr of (arith_expr * arith_expr) (* asr *)
    | `Land of (arith_expr * arith_expr) (* land *)
    | `Lor of (arith_expr * arith_expr) (* lor *)
    | `Lxor of (arith_expr * arith_expr) (* lxor *)
    ]

type node =
    [ `Ident of (loc * string * actuals)
         (* the list [actuals] is empty if and only if no parentheses
            are used at this macro invocation site. *)
    | `Def of (loc * macro * formals * body)
         (* the list [formals] is empty if and only if no parentheses
            are used at this macro definition site. *)
    | `Scope of body
    | `Undef of (loc * macro)
    | `Include of (loc * string)
    | `Ext of (loc * string * string)
    | `Cond of (loc * bool_expr * node list * node list)
    | `Error of (loc * string)
    | `Warning of (loc * string)
    | `Text of (loc * bool * string) (* bool is true for space tokens *)
    | `Seq of (loc * node list)
    | `Stringify of node
    | `Capitalize of node
    | `Concat of (node * node)
    | `Line of (loc * string option * int)
    | `Current_line of loc
    | `Current_file of loc ]

(* A formal macro parameter consists of an identifier (the name of this
   parameter) and a shape (the shape of this parameter). In the concrete
   syntax, if the shape is omitted, then the base shape is assumed. *)
and formal =
  string * shape

(* A tuple of formal macro parameters. *)
and formals =
  formal list

(* One actual macro argument. *)
and actual =
  node

(* A tuple of actual macro arguments. *)
and actuals =
  actual list

(* The body of a macro definition. *)
and body =
  node


let string_of_loc (pos1, pos2) =
  let line1 = pos1.pos_lnum
  and start1 = pos1.pos_bol in
  Printf.sprintf "File %S, line %i, characters %i-%i"
    pos1.pos_fname line1
    (pos1.pos_cnum - start1)
    (pos2.pos_cnum - start1)


exception Cppo_error of string

let error loc s =
  let msg =
    sprintf "%s\nError: %s" (string_of_loc loc) s in
  raise (Cppo_error msg)

let warning loc s =
  let msg =
    sprintf "%s\nWarning: %s" (string_of_loc loc) s in
  eprintf "%s\n%!" msg

let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

let rec node_loc (node : node) : loc =
  match node with
  | `Ident (loc, _, _)
  | `Def (loc, _, _, _)
  | `Undef (loc, _)
  | `Include (loc, _)
  | `Ext (loc, _, _)
  | `Cond (loc, _, _, _)
  | `Error (loc, _)
  | `Warning (loc, _)
  | `Text (loc, _, _)
  | `Seq (loc, _)
  | `Line (loc, _, _)
  | `Current_line loc
  | `Current_file loc
      -> loc
  | `Scope node ->
      node_loc node
  | `Stringify _
  | `Capitalize _
  | `Concat (_, _)
      -> dummy_loc
           (* These cases are never produced by the parser. *)

let rec is_whitespace_node node =
  match node with
  | `Text (_, is_whitespace, _) ->
      is_whitespace
  | `Seq (_loc, nodes) ->
      is_whitespace_nodes nodes
  | _ ->
      false

and is_whitespace_nodes nodes =
  List.for_all is_whitespace_node nodes

let is_not_whitespace_node node =
  not (is_whitespace_node node)

let dissolve (node : node) : node list =
  match node with
  | `Seq (_loc, nodes) ->
      nodes
  | _ ->
      [node]

let nodes_are_ident (nodes : node list) : (loc * string) option =
  match List.filter is_not_whitespace_node nodes with
  | [`Ident (loc, x, [])] ->
      Some (loc, x)
  | _ ->
      None

let node_is_ident (node : node) : (loc * string) option =
  nodes_are_ident (dissolve node)
