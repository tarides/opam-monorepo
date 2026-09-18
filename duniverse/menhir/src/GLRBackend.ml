(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf
open ILConstruction
open Grammar
let grammar = Front.grammar
open IL
open Channels
open PlainSyntax

module TokenType =
  TokenType.Make(Settings)
open TokenType

module Basics =
  Interface.Basics(Settings)
open Basics

module Conventions =
  Conventions.Make(Grammar)(Settings)
open Conventions

let runtimelib =
  "MenhirGLR"

module TableGeneration = TableGeneration.Make(struct
  let runtimelib = runtimelib
  let info = getC 1
end)
open TableGeneration

module Run () = struct

let start_time =
  Time.start()

(* -------------------------------------------------------------------------- *)

(* [fold_entry] enumerates the start states of the LR(1) automaton.
   The user function [f] is applied to the start production [prod],
   the start state [state], the (user) start nonterminal symbol [nt],
   and the OCaml type of this symbol [ty]. *)

let fold_entry f accu =
  ProductionMap.fold (fun prod state accu ->
    let nt = Production.get_start prod in
    let ty = Nonterminal.ocamltype_of_start_symbol nt in
    f prod state nt ty accu
  ) Lr1.entry accu

(* -------------------------------------------------------------------------- *)

(* Conventional names for modules, exceptions, record fields, functions. *)

let runtimelib_module m =
  runtimelib ^ "." ^ m

let qualified m x =
  runtimelib_module m ^ "." ^ x

let table_interpreter_make =
  qualified "TableInterpreter" "Make"

let glr_make =
  qualified "GLR" "Make"

let input_create =
  qualified "Input" "create"

let fsemv =
  qualified "GSS" "semv"

let fnode =
  qualified "GSS" "node"

let fdate =
  qualified "GSS" "date"

let c_path_cons =
  qualified "Path" "Edge"

let c_path_nil =
  qualified "Path" "Empty"

let start_position =
  qualified "Input" "start_position"

let end_position =
  qualified "Input" "end_position"

let tinput ty =
  TypApp (qualified "Input" "input", [ty])

let entry =
  interpreter_submodule_name ^ ".start"

let staticVersion =
  runtimelib ^ ".StaticVersion"

(* The following are names of internal sub-modules. *)

let tables =
  "Tables"

(* Date variables and position variables. *)

let date_var i =
  prefix (sprintf "date_%d" i)

(* The following function calls invoke [MenhirGLR.Input.start_position] or
   [MenhirGLR.Input.end_position] to convert the date variable [date_var i]
   into a position. *)

let convert_startp (input : var) (i : int) : expr =
  eapp (EVar start_position) [ EVar input; EVar (date_var i)]

let convert_endp (input : var) (i : int) : expr =
  eapp (EVar end_position) [ EVar input; EVar (date_var i)]

(* -------------------------------------------------------------------------- *)

(* Code generation for semantic actions. *)

(* [edge_pattern prod i] constructs a pattern that matches a GSS edge.
   This edge corresponds to the [i]-th element in the right-hand side
   of the production [prod]. The pattern binds the semantic-value
   variable [ids.(i)] and the date variable [date_var i]. *)

let edge_pattern prod i : pattern =
  let ids = Production.identifiers prod in
  PRecord [ (* edge *)
    fsemv, PVar ids.(i);
    fnode, PRecord [ (* node *)
      fdate, PVar (date_var i)
    ]
  ]

(* [path_cons_pattern prod i tail] constructs a pattern that matches a
   subpath. This subpath corresponds to the production suffix defined by
   [prod] and [i]. It is assumed that [tail] is the pattern that corresponds
   to the suffix defined by [prod] and [i+1]. The pattern that is returned
   binds the semantic-value variables [ids.(j)] and the date variables
   [date_var j] for all [j] such that [i <= j]. *)

let path_cons_pattern prod i (tail : pattern) : pattern =
  PData (c_path_cons, [
    edge_pattern prod i;
    tail
  ])

(* [path_nil_pattern n] constructs a pattern that matches an empty subpath.
   This subpath corresponds to an empty production suffix. The pattern that
   is returned binds just the date variable [date_var n]. *)

let path_nil_pattern n : pattern =
  PData (c_path_nil, [
    PRecord [ (* node *)
      fdate, PVar (date_var n)
    ]
  ])

(* [filter action posbindings] filters the list [posbindings] by keeping only
   the bindings (of position variables) that the semantic action [action]
   actually needs. *)

(* This lets us save the expensive function calls that convert dates to
   positions, and save also the cost of reading the dates from memory,
   as the OCaml compiler will see that the date variables are unused. *)

let filter action (posbindings : (var * expr) list) : (pattern * expr) list =
  let needed = Action.posvars action in
  let needed (x, _e) = StringSet.mem x needed in
  posbindings
  |> List.filter needed
  |> List.map (fun (x, e) -> (PVar x, e))

(* [semantic_action prod] is the semantic action function for the production
   [prod]. Its parameters are [input], which offers access to the input
   stream, and [path], which offers access to the reduction path, a sequence
   of GSS edges. See [MenhirGLR.GLRAPI]. *)

let semantic_action prod : expr =
  assert (not (Production.is_start prod));
  let nt, rhs = Production.def prod
  and ids = Production.identifiers prod
  and n = Production.length prod
  and action = Production.action prod in

  (* [input] and [path] are the parameters of the semantic action function. *)
  let input = prefix "input"
  and path = prefix "path" in

  (* Build a pattern that extracts the desired data out of a GLR path. A path
     is a list of edges, where the head of the list represents the deepest
     edge, that is, the edge that corresponds to the leftmost symbol in the
     right-hand side of the production [prod]. *)

  (* Because the pattern must be built bottom up, we want a [fold] that begins
     with the deepest cells in the path, that is, the rightmost end of the
     array [rhs]. We must fold from right to left on [rhs] is appropriate. *)

  let (_ : int), pat, casts =
    Array.fold_right (fun symbol (i, pat, casts) ->
      i - 1,
      path_cons_pattern prod i pat,
      bcast ids.(i) (semvtype1 symbol) :: casts
    ) rhs (n - 1, path_nil_pattern n, [])
  in

  (* Bind all of the conventional position variables, namely [beforeendp],
     [$startpos(i)], [$endpos(i)]. We compute a suitable value for each such
     variable by relying on the date variables [date_var i], which have been
     bound by the pattern [pat] above. *)

  let posbindings : (var * expr) list =
    (beforeendp, convert_endp input 0) ::
    List.init n (fun i -> startpos ids i, convert_startp input i) @
    List.init n (fun i -> endpos ids i,   convert_endp input (i+1))
  in

  (* This is the complete semantic action function. *)
  efun [ PVar input; PVar path ] @@
  ecomment (Production.print prod) @@
  (* Destructure the path. *)
  non_exhaustive_let pat (EVar path) @@
  blet (
    (* Perform the type casts planned above. *)
    casts @
    (* Bind the position variables. *)
    filter action posbindings @
    (* Execute the semantic action, annotated with its expected type. *)
    [ PVar semv, annotate (semvtype nt) (Action.expr action) ]
  ) @@
  (* Cast the result to the type [Obj.t]. *)
  ERepr (EVar semv)

(* -------------------------------------------------------------------------- *)

(* Table encodings. *)

(* Encodings of entries in the default reduction table. *)

let dr_DefRed prod =            (* 1 + prod *)
  1 + Production.encode prod

let dr_NoDefRed =               (* 0 *)
  0

(* Encodings of entries in the unique action table. *)

let ua_Reduce prod =            (* prod | 01 *)
  ((Production.encode prod) lsl 2) lor 0b01

let ua_Shift s =                (*    s | 10 *)
  ((Lr1.encode s) lsl 2) lor 0b10

let ua_Fork =                   (*        11 *)
  0b11

let ua_Fail =                   (*        00 *)
  0b00

(* Encodings of entries in the unique action bitmap. *)

let uab_DoNotFail = 0
let uab_Fail = 1

(* Encodings of entries in the shift table. *)

let s_Shift s =
  1 + Lr1.encode s

let s_Fail =
  0

(* Encodings of entries in the shift bitmap. *)

let sb_DoNotFail = 0
let sb_Fail = 1

(* Encodings of entries in the goto table. *)

let encode_Goto node =              (* 1 + node *)
  1 + Lr1.encode node

let encode_NoGoto =                 (* 0 *)
  0

(* -------------------------------------------------------------------------- *)

(* Table generation. *)

(* The unique action table. *)

(* This table is used by the GLR parser when it runs in deterministic mode.
   It proposes a (shift or reduce) action only when there is a unique such
   action. Otherwise, it proposes nothing; this is encoded by [Fail]. *)

(* This table is essentially identical to the [action] table of the (LR)
   table back-end. *)

let transitions node t : Lr1.node option =
  SymbolMap.find_opt (Symbol.T t) (Lr1.transitions node)

let reductions node t : Production.t list =
  try TerminalMap.find t (Lr1.reductions node) with Not_found -> []

let unique_action node t =
  (* If there is a default reduction, then this table entry is irrelevant. *)
  if Lr1.has_default_reduction node then
    ua_Fail
  else
    match transitions node t, reductions node t with
    | Some target, [] ->
        (* There is a transition, and no reduction. *)
        ua_Shift target
    | None, [prod] ->
        (* There is no transition, and one reduction. *)
        ua_Reduce prod
    | None, [] ->
        (* There is no action at all. *)
        ua_Fail
    | _, _ ->
        (* There are several actions. *)
        ua_Fork

(* The unique action bitmap reflects which entries in the [unique_action]
   table are [ua_Fail]. This justifies that the value [ua_Fail] in
   the unique action table can then be considered insignificant. *)

let unique_action_bitmap node t =
  if unique_action node t = ua_Fail then
    uab_Fail
  else
    uab_DoNotFail

(* The shift table. *)

(* This table proposes a shift action, if there is one; otherwise, it proposes
   nothing. *)

let shift node t =
  (* If there is a default reduction, then this table entry is irrelevant. *)
  if Lr1.has_default_reduction node then s_Fail else
  match transitions node t with
  | Some target ->
      (* There is a transition. *)
      s_Shift target
  | None ->
      (* There is no transition. *)
      s_Fail

(* The shift bitmap reflects which entries in the [shift] table are
   [ua_Fail]. *)

let shift_bitmap node t =
  if shift node t = s_Fail then
    sb_Fail
  else
    sb_DoNotFail

(* The reductions table. *)

(* Each entry in this table is an array of reduction actions. *)

(* If there is a default reduction, then this table entry is irrelevant.
   We do not take advantage of this. We just consider this table entry
   to contain an empty list in this case, so it occupies zero space in
   the linearized table. *)

let reductions node t : int array =
  if Lr1.has_default_reduction node then [||] else
  reductions node t |> Array.of_list |> Array.map Production.encode

(* In the error bitmap and in the action table, the row that corresponds to
   the special terminal symbol [#] is never accessed. Thus, we do not create
   this row. This does not create a gap in the table, because this is the
   right-most row. For sanity, we check this fact here. *)

let () =
  assert (Terminal.encode Terminal.sharp = Terminal.n - 1)

(* The goto table. *)

let goto node nt =
  try
    let target = SymbolMap.find (Symbol.N nt) (Lr1.transitions node) in
    encode_Goto target
  with Not_found ->
    encode_NoGoto

(* The default reductions table. *)

let default_reduction node =
  match Lr1.test_default_reduction node with
  | Some (prod, _) ->
      dr_DefRed prod
  | None ->
      dr_NoDefRed

(* Generate the table definitions. *)

let unique_action_bitmap =
  marshal_2D_matrix "unique action bit" "unique_action_bitmap" @@
  Lr1.init @@ fun node ->
  Terminal.initx @@ fun t ->
  unique_action_bitmap node t

let unique_action =
  let insignificant entry = (entry = ua_Fail) in
  marshal_2D_sparse_matrix "unique action" "unique_action" insignificant @@
  Lr1.init @@ fun node ->
  Terminal.initx @@ fun t ->
  unique_action node t

let shift_bitmap =
  marshal_2D_matrix "shift bit" "shift_bitmap" @@
  Lr1.init @@ fun node ->
  Terminal.initx @@ fun t ->
  shift_bitmap node t

let shift =
  let insignificant entry = (entry = s_Fail) in
  marshal_2D_sparse_matrix "shift" "shift" insignificant @@
  Lr1.init @@ fun node ->
  Terminal.initx @@ fun t ->
  shift node t

let reductions =
  marshal_irregular_3D_array "reductions" "reductions" @@
  Lr1.init @@ fun node ->
  Terminal.initx @@ fun t ->
  reductions node t

let goto =
  let insignificant entry = (entry = encode_NoGoto) in
  marshal_2D_sparse_matrix "goto" "goto" insignificant @@
  Lr1.init @@ fun node ->
  Nonterminal.init @@ fun nt ->
  goto node nt

let default_reduction =
  marshal_1D_array "default reduction" "default_reduction" @@
  Lr1.init @@ fun node ->
  default_reduction node

let lhs =
  marshal_1D_array "left-hand side" "lhs" @@
  Production.init @@ fun prod ->
  Nonterminal.encode (Production.nt prod)

let length =
  marshal_1D_array "length" "length" @@
  Production.init @@ fun prod ->
  Production.length prod

let semantic_action =
  def "semantic_action" @@
  (* Non-start productions only. *)
  EArray (Production.mapx semantic_action)

(* -------------------------------------------------------------------------- *)

(* Generate the two functions that map a token to its integer code and to
   its semantic value, respectively. *)

let token2terminal =
  destruct_token_def
    "token2terminal"
    tint
    false
    (fun tok -> EIntConst (Terminal.encode tok))

let token2value =
  destruct_token_def
    "token2value"
    tobj
    true
    (fun tok ->
      ERepr (
        match Terminal.ocamltype tok with
        | None ->
            EUnit
        | Some typ ->
            (* 2025/09/12: a type annotation is required for safety when the
               user uses an external [token] type (via --external-tokens).
               We might have two inconsistent views of the type of the
               semantic value: one in our .mly file, and one in the external
               definition of the [token] type. See MR 38. *)
            EAnnot (EVar semv, type2scheme (TypTextual typ))
      )
    )

(* -------------------------------------------------------------------------- *)

(* Generate a mapping of nonterminal symbols to their names. *)

(* This mapping is looked up when the default merge function is invoked. *)

let nt_name =
  prefix "nt_name"

let nt_name_def =
  def nt_name @@
  EArray (
    Nonterminal.map @@ fun nt ->
    EStringConst (Nonterminal.print false nt)
  )

let nt_name_lookup nt =
  EArrayAccess (EVar nt_name, EVar nt)

(* -------------------------------------------------------------------------- *)

(* Generate the main [merge] function. *)

(* The function [merge] has type
   [int -> Obj.t -> Obj.t -> token input -> int -> int Obj.t].

   Its first parameter is (the integer number of) a nonterminal symbol [nt].

   The next two parameters are two semantic values [_v1] and [_v2]
   whose type depends on [nt].

   The next three parameters are the input stream [input] and the start date
   and end date of the input fragment associated with this merge event.

   The result is a semantic value of the same type as [_v1] and [_v2]. *)

module Merge = struct

  (* The parameters of the function [merge]. *)

  let nt, v1, v2, input, date0, date1 =
    prefix "nt", prefix "_v1", prefix "_v2",
    prefix "input", date_var 0, date_var 1

  (* Inside each branch, [date0] and [date1] are used to define [$startpos]
     and [$endpos], as follows. These bindings are emitted only if the merge
     function actually uses the keywords [$startpos] and [$endpos]. *)

  (* In a semantic action, KeywordExpansion expands away [$startpos] and
     [$endpos]. However, KeywordExpansion does not affect %merge functions. *)

  let posbindings : (var * expr) list =
    let open Keyword in
    [ posvar Left WhereStart FlavorPosition, convert_startp input 0;
      posvar Left WhereEnd   FlavorPosition, convert_endp   input 1 ]

  let rec merge () =
    def "merge" @@
    annotate (marrow [ tint; tobj; tobj; tinput ttoken; tint; tint ] tobj) @@
    efun [ PVar nt; PVar v1; PVar v2; PVar input; PVar date0; PVar date1 ] @@
    ematch (EVar nt) @@
    Nonterminal.foldx (fun nt branches ->
      match Nonterminal.merge_function nt with
      | Some action ->
          let branch =
            (* If the symbol [nt] has a %merge function, then generate a
               branch for this symbol. *)
            branch (PIntConst (Nonterminal.encode nt)) @@
            ecomment (Nonterminal.print false nt) @@
            let v = "_v" and ty = semvtype nt in
            blet (
              filter action posbindings @ [
              (* Cast [v1] and [v2] to the type [semvtype nt]. *)
              bcast v1 ty;
              bcast v2 ty;
              (* Apply the user-provided %merge function to [v1] and [v2]. *)
              PVar v, eapp (Action.expr action) [ EVar v1; EVar v2 ]
            ]) @@
            (* Cast the result back from [semvtype nt] to [Obj.t]. *)
            ERepr (annotate (semvtype nt) (EVar v))
          in
          branch :: branches
      | None ->
          branches
    )
    (* In the default branch, call the default merge function. *)
    [ branch PWildcard @@ default_branch() ]

  (* The default branch of the [merge] function. *)

  and default_branch () : expr =
    if CheckMergeFunctions.every_rule_has_merge_fun grammar then
      (* The default branch is dead. *)
      eassertfalse
    else
      match grammar.default_merge with
      | None ->
          (* The default branch is not dead, but the user has not provided
             a default merge function. We warn against this situation, but
             the user can ignore this warning, so we must generate code. *)
          let format =
            "Error: the symbol %s is ambiguous,\n\
             yet no merge function for this symbol has been defined."
          in
          blet [ PUnit, eprintf format [nt_name_lookup nt]] @@
          eassertfalse
      | Some mfl ->
          let action = Located.value mfl in
          (* Apply the user-provided default %merge function to [nt],
             [v1] and [v2]. *)
          blet (filter action posbindings) @@
          eapp (Action.expr action) [ nt_name_lookup nt; EVar v1; EVar v2 ]

end (* Merge *)

(* -------------------------------------------------------------------------- *)

(* The client APIs invoke the interpreter with an appropriate start state. The
   monolithic API uses the function [entry], which performs the entire parsing
   process. *)

(* An entry point to the monolithic API. *)

let monolithic_entry_point state nt ty =
  def (Nonterminal.print true nt) @@
  let lexer, lexbuf, input = "lexer", "lexbuf", "input" in
  efun [ PVar lexer; PVar lexbuf ] @@
  blet [ PVar input, EApp (EVar input_create, [ EVar lexer; EVar lexbuf ]) ] @@
  annotate (TypTextual ty) @@
  emagic @@
  eapp (EVar entry) [
    EIntConst (Lr1.encode state);
    EVar input
  ]

(* The whole monolithic API. *)

let monolithic_api : IL.valdef list =
  fold_entry (fun _prod state nt t api ->
    monolithic_entry_point state nt t ::
    api
  ) []

(* -------------------------------------------------------------------------- *)

(* A reference to [MenhirGLR.StaticVersion.require_XXXXXXXX]. *)

let versiondef =
  pdef PUnit @@
  EVar (staticVersion ^ ".require_" ^ Version.version)

(* -------------------------------------------------------------------------- *)

(* The generated code. *)

let program =

  [ SIFunctor (grammar.parameters,

    SIComment "This generated code requires the following version of MenhirGLR:" ::
    valdef versiondef ::

    (* Define the internal sub-module [Basics], which contains the definitions
       of the exception [Error] and of the type [token]. Then, include this
       sub-module. This sub-module is used again below, as part of the
       application of the functor [TableInterpreter.Make]. *)

    basics_submodule_def grammar @

    (* In order to avoid hiding user-defined identifiers, only the type
       [token] should be defined above this line. *)

    SIFragment grammar.preludes ::

    (* Define the tables. *)

    SIModuleDef (tables, (* see [TableFormat] *)
      MStruct (

        (* The internal sub-module [Basics] contains the definitions of the
           exception [Error] and of the type [token]. *)
        SIInclude (MVar basics_submodule_name) ::

        (* This is a cascade of non-recursive definitions. An accessor
           function may need to access the table that is defined above it. The
           semantic actions come first so as to avoid a name collision. *)
        List.map valdef (
          semantic_action ::
          nt_name_def ::
          Merge.merge() ::
          token2terminal ::
          token2value ::
          def "start" (EIntConst Production.start) ::
          lhs @
          length @
          def "n" (EIntConst Lr1.n) ::
          default_reduction @
          unique_action_bitmap @
          unique_action @
          shift_bitmap @
          shift @
          reductions @
          goto @
          []
        )
      )
    ) ::

    SIModuleDef (interpreter_submodule_name,
      (* Apply the functor [TableInterpreter.Make] to the tables. *)
      (* Apply the functor [GLR.Make] to obtain an engine. *)
      MApp (MVar glr_make, MApp (MVar table_interpreter_make, MVar tables))
    ) ::

    SIValDefs (false, monolithic_api) ::

    SIFragment grammar.postludes ::

  [])]

let () =
  Time.stop start_time "Producing tables and abstract syntax"

end (* Run *)
