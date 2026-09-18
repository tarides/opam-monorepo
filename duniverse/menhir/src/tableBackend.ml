(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let provided = MList.provided
open ILConstruction
open Grammar
open IL
open Channels
open Printf
open NonterminalType
open PlainSyntax

module TokenType =
  TokenType.Make(Settings)
open TokenType

module Basics =
  Interface.Basics(Settings)
open Basics

module Interface =
  Interface.Make(Settings)
open Interface

module Conventions =
  Conventions.Make(Grammar)(Settings)
open Conventions

let runtimelib =
  "MenhirLib"

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

let make_engine_table =
  runtimelib ^ ".TableInterpreter.MakeEngineTable"

let make_engine =
  runtimelib ^ ".Engine.Make"

let make_symbol =
  runtimelib ^ ".InspectionTableInterpreter.Symbols"

let make_inspection =
  runtimelib ^ ".InspectionTableInterpreter.Make"

let engineTypes =
  runtimelib ^ ".EngineTypes"

let field x =
  engineTypes ^ "." ^ x

let fstate =
  field "state"

let fsemv =
  field "semv"

let fstartp =
  field "startp"

let fendp =
  field "endp"

let fnext =
  field "next"

let fstack =
  field "stack"

let fcurrent =
  field "current"

let entry =
  interpreter_submodule_name ^ ".entry"

let start =
  interpreter_submodule_name ^ ".start"

let staticVersion =
  runtimelib ^ ".StaticVersion"

(* The following are names of internal sub-modules. *)

let tables =
  "Tables"

let symbols =
  "Symbols"

let et =
  "ET"

let ti =
  "TI"

(* -------------------------------------------------------------------------- *)

(* Code generation for semantic actions. *)

(* Things are slightly more regular here than in the code back-end, since
   there is no optimization: every stack cell has the same structure and
   holds a state, a semantic value, and a pair of positions. *)

(* [reducecellparams] constructs a pattern that describes the contents
   of a stack cell. If this is the bottom cell, the variable [state]
   is bound to the state found in the cell. If [ids.(i)] is used in
   the semantic action, then it is bound to the semantic value. The
   position variables are always bound. *)

let reducecellparams prod i _symbol (next : pattern) : pattern =

  let ids = Production.identifiers prod in

  PRecord [
    fstate, (if i = 0 then PVar state else PWildcard);
    fsemv, PVar ids.(i);
    fstartp, PVar (sprintf "_startpos_%s_" ids.(i));
    fendp, PVar (sprintf "_endpos_%s_" ids.(i));
    fnext, next;
  ]

(* 2015/11/04. The start and end positions of an epsilon production are obtained
   by taking the end position stored in the top stack cell (whatever it is). *)

let endpos_of_top_stack_cell =
  ERecordAccess(EVar stack, fendp)

(* This is the body of the [reduce] function associated with production
   [prod]. It assumes that the variable [stack] is bound. *)

let reducebody prod =

  let nt, rhs = Production.def prod
  and ids = Production.identifiers prod
  and length = Production.length prod in

  (* Build a pattern that represents the shape of the stack. Out of
     the stack, we extract a state (except when the production is an
     epsilon production) and a number of semantic values. *)

  (* At the same time, build a series of casts. The semantic value
     variables bound by the pattern [pat] have type [Obj.t]. They
     must be cast to the correct type. *)

  (* We want a [fold] that begins with the deepest cells in the stack.
     Folding from left to right on [rhs] is appropriate. *)

  let (_ : int), pat, casts =
    Array.fold_left (fun (i, pat, casts) symbol ->
      i + 1,
      reducecellparams prod i symbol pat,
      bcast ids.(i) (semvtype1 symbol) :: casts
    ) (0, PVar stack, []) rhs
  in

  (* Bind the conventional variable [beforeendp], for use by the semantic
     action. If the semantic action does not use this variable, then this
     binding is dead code and can be ignored by the OCaml compiler. Bind the
     variables [startp] and [endp] for our own use below; they are used in the
     construction of a new stack cell. The semantic action cannot use them,
     because the keywords [$startpos] and [$endpos] are expanded away. *)

  let posbindings =
    ( PVar beforeendp,
      endpos_of_top_stack_cell
    ) ::
    ( PVar startp,
      if length > 0 then
        EVar (sprintf "_startpos_%s_" ids.(0))
      else
        endpos_of_top_stack_cell
    ) ::
    ( PVar endp,
      if length > 0 then
        EVar (sprintf "_endpos_%s_" ids.(length - 1))
      else
        EVar startp
    ) :: []
  in

  (* This cannot be one of the start productions. *)
  assert (not (Production.is_start prod));

  (* This is a regular production. Perform a reduction. *)

  let act =
    annotate (semvtype nt) @@
    Action.expr (Production.action prod)
  in

  ecomment (Production.print prod) @@
  blet (
    (pat, EVar stack) ::                (* destructure the stack *)
    casts @                             (* perform type casts *)
    posbindings @                       (* bind [startp] and [endp] *)
    [ PVar semv, act ]                  (* run the user's code and bind [semv] *)
  ) @@

  (* Return a new stack, onto which we have pushed a new stack cell. *)
  ERecord [                             (* the new stack cell *)
    fstate, EVar state;                 (* the current state after popping; it will be updated by [goto] *)
    fsemv, ERepr (EVar semv);           (* the newly computed semantic value *)
    fstartp, EVar startp;               (* the newly computed start and end positions *)
    fendp, EVar endp;
    fnext, EVar stack;                  (* this is the stack after popping *)
  ]

(* This is the body of the semantic action associated with production
   [prod]. It takes just one parameter, namely the environment [env]. *)

let semantic_action prod =
  let env = prefix "env" in
  EFun (
    [ PVar env ],

    (* Access the stack and current state via the environment. *)

    (* In fact, the current state needs be bound here only if this is
       an epsilon production. Otherwise, the variable [state] will be
       bound by the pattern produced by [reducecellparams] above. *)

    ELet (

      [ PVar stack, ERecordAccess (EVar env, fstack) ] @
        (if Production.length prod = 0 then [ PVar state, ERecordAccess (EVar env, fcurrent) ] else []),

      reducebody prod

    )
  )

(* Export the number of start productions. *)

let start_def =
  def "start" @@
  EIntConst Production.start

(* -------------------------------------------------------------------------- *)

(* Table encodings. *)

(* Encodings of entries in the default reduction table. *)

let encode_DefRed prod =            (* 1 + prod *)
  1 + Production.encode prod

let encode_NoDefRed =               (* 0 *)
  0

(* Encodings of entries in the action table. *)

let encode_Reduce prod =            (* prod | 01 *)
  (Production.encode prod lsl 2) lor 1

let encode_ShiftDiscard s =         (*    s | 10 *)
  ((Lr1.encode s) lsl 2) lor 0b10

let encode_ShiftNoDiscard s =       (*    s | 11 *)
  ((Lr1.encode s) lsl 2) lor 0b11

(* In the encoding of a shift transition, if the target state has a default
   reduction on [#], we use [ShiftNoDiscard], otherwise [ShiftDiscard]. *)
let encode_Shift s =
  if Lr1.has_default_reduction_on_sharp s then
    encode_ShiftNoDiscard s
  else
    encode_ShiftDiscard s

let encode_Fail =                   (*        00 *)
  0

(* Encodings of entries in the goto table. *)

let encode_Goto node =              (* 1 + node *)
  1 + Lr1.encode node

let encode_NoGoto =                 (* 0 *)
  0

(* Encodings of entries in the error bitmap. *)

let encode_Error =                  (* 0 *)
  0

let encode_NoError =                (* 1 *)
  1

(* Encodings of terminal and nonterminal symbols in the production table. *)

let encode_no_symbol =
  0                                          (* 0 | 0 *)

let encode_terminal tok =
  (Terminal.encode tok + 1) lsl 1          (*  t + 1 | 0 *)

let encode_nonterminal nt =
  ((Nonterminal.encode nt) lsl 1) lor 1        (* nt | 1 *)

let encode_symbol = function
  | Symbol.T tok ->
      encode_terminal tok
  | Symbol.N nt ->
      encode_nonterminal nt

let encode_symbol_option = function
  | None ->
      encode_no_symbol
  | Some symbol ->
      encode_symbol symbol

(* Encoding a Boolean as an integer value. *)

let encode_bool b =
  if b then 1 else 0

(* -------------------------------------------------------------------------- *)

(* Table generation. *)

(* The action table. *)

let action node t =

  (* If this state has a default reduction, then the action table is
     never looked up; this table entry is irrelevant. *)
  if Lr1.has_default_reduction node then encode_Fail else

  match SymbolMap.find (Symbol.T t) (Lr1.transitions node) with
  | target ->
      (* [node] has a transition to [target]. *)
      encode_Shift target

  | exception Not_found ->

      match TerminalMap.find t (Lr1.reductions node) with
      | prods ->
          (* [node] has a reduction. *)
          let prod = MList.single prods in
          encode_Reduce prod

      | exception Not_found ->
          (* [node] has no action. *)
          encode_Fail

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

(* The error bitmap reflects which entries in the action table are
   [Fail]. Like the action table, it is not accessed when [node] has a
   default reduction. *)

let error node t =
  if action node t = encode_Fail then
    encode_Error
  else
    encode_NoError

(* The default reductions table. *)

let default_reduction node =
  match Lr1.test_default_reduction node with
  | Some (prod, _) ->
      encode_DefRed prod
  | None ->
      encode_NoDefRed

(* Generate the table definitions. *)

let action =
  let insignificant entry = (entry = encode_Fail) in
  marshal_2D_sparse_matrix "action" "action" insignificant @@
  Lr1.init @@ fun node ->
  Terminal.initx @@ fun t ->
  action node t

let goto =
  let insignificant entry = (entry = encode_NoGoto) in
  marshal_2D_sparse_matrix "goto" "goto" insignificant @@
  Lr1.init @@ fun node ->
  Nonterminal.init @@ fun nt ->
  goto node nt

let error =
  marshal_2D_matrix "error" "error" @@
  Lr1.init @@ fun node ->
  Terminal.initx @@ fun t ->
  error node t

let default_reduction =
  marshal_1D_array "default reduction" "default_reduction" @@
  Lr1.init @@ fun node ->
  default_reduction node

let lhs =
  marshal_1D_array "left-hand side" "lhs" @@
  Production.init @@ fun prod ->
  Nonterminal.encode (Production.nt prod)

let semantic_action =
  def "semantic_action" @@
  (* Non-start productions only. *)
  EArray (Production.mapx semantic_action)

(* -------------------------------------------------------------------------- *)

(* When [--trace] is enabled, we need tables that map terminals and
   productions to strings. *)

let stringwrap f x =
  EStringConst (f x)

let reduce_or_accept prod =
  match Production.test_start prod with
  | Some _ ->
      "Accepting"
  | None ->
      "Reducing production " ^ (Production.print prod)

let trace =
  define_and_measure "trace" @@
  if Settings.trace then
    EData ("Some", [
      ETuple [
        EArray (Terminal.map (stringwrap Terminal.print));
        EArray (Production.map (stringwrap reduce_or_accept));
      ]
    ])
  else
    EData ("None", [])

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

(* The client APIs invoke the interpreter with an appropriate start state. The
   monolithic API uses the function [entry], which performs the entire parsing
   process, while the incremental API relies on the function [start], which
   returns just an initial checkpoint. Both functions are defined in
   [lib/Engine.ml]. *)

(* The function [entry] takes a [strategy] parameter, whose value is fixed at
   compile time, based on [Settings.strategy]. For users of the incremental
   API, the value of [Settings.strategy] is irrelevant; the functions [resume]
   and [loop] offered by the incremental API take a [strategy] parameter at
   runtime. *)

let strategy =
  match Settings.strategy with
  | `Legacy ->
      EData ("`Legacy", [])
  | `Simplified ->
      EData ("`Simplified", [])

(* An entry point to the monolithic API. *)

let monolithic_entry_point state nt t =
  def (Nonterminal.print true nt) @@
  let lexer = "lexer"
  and lexbuf = "lexbuf" in
  efun [ PVar lexer; PVar lexbuf ] @@
  annotate (TypTextual t) @@
  emagic @@
  eapp (EVar entry) [
    strategy;
    EIntConst (Lr1.encode state);
    EVar lexer;
    EVar lexbuf
  ]

(* The whole monolithic API. *)

let monolithic_api : IL.valdef list =
  fold_entry (fun _prod state nt t api ->
    monolithic_entry_point state nt t ::
    api
  ) []

(* An entry point to the incremental API. *)

let incremental_entry_point state nt t =
  let initial = "initial_position" in
  def (Nonterminal.print true nt) @@
  (* In principle the eta-expansion [fun initpos -> start s initpos] should
     not be necessary, since [start] is a pure function. However, if [--trace]
     is enabled, [start] logs messages to [stderr]. *)
  efun [ PVar initial ] @@
  annotate (tcheckpoint (TypTextual t)) @@
  emagic @@
  eapp (EVar start) [
    EIntConst (Lr1.encode state);
    EVar initial;
  ]

(* The whole incremental API. *)

let incremental_api : IL.valdef list =
  fold_entry (fun _prod state nt t api ->
    incremental_entry_point state nt t ::
    api
  ) []

(* -------------------------------------------------------------------------- *)

(* Constructing representations of symbols. *)

(* [eterminal t] is a value of type ['a terminal] (for some ['a]) that
   encodes the terminal symbol [t]. It is just a data constructor of
   the terminal GADT. *)

let eterminal (t : Terminal.t) : expr =
  EData (tokengadtdata (Terminal.print t), [])

(* [enonterminal nt] is a value of type ['a nonterminal] (for some
   ['a]) that encodes the nonterminal symbol [nt]. It is just a data
   constructor of the nonterminal GADT. *)

let enonterminal (nt : Nonterminal.t) : expr =
  EData (tnonterminalgadtdata (Nonterminal.print false nt), [])

(* [esymbol symbol] is a value of type ['a symbol] (for some ['a])
   that encodes the symbol [symbol]. It is built by applying the
   injection [T] or [N] to the terminal or nonterminal encoding. *)

let dataT =
  "T"

let dataN =
  "N"

let esymbol (symbol : Symbol.t) : expr =
  match symbol with
  | Symbol.T t ->
      EData (dataT, [ eterminal t ])
  | Symbol.N nt ->
      EData (dataN, [ enonterminal nt ])

(* [xsymbol symbol] is a value of type [xsymbol] that encodes the
   symbol [symbol]. It is built by applying the injection [X] (an
   existential quantifier) to [esymbol symbol]. *)

let dataX =
  "X"

let xsymbol (symbol : Symbol.t) : expr =
  EData (dataX, [ esymbol symbol ])

(* -------------------------------------------------------------------------- *)

(* Produce a function that maps a terminal symbol (represented as an integer
   code) to its representation as an [xsymbol]. Include [error] but not [#],
   i.e., include all of the symbols which can appear in a production. *)

(* Note that, instead of generating a function, we could (a) use an array
   or (b) use an unsafe conversion of an integer to a data constructor,
   then wrap it using [X] and [T/N]. Approach (b) is unsafe and causes
   memory allocation (due to the wrapping) at each call. *)

let terminal () =
  assert Settings.inspection;
  let t = "t" in
  def "terminal" @@
  EFun ([ PVar t ],
    EMatch (EVar t,
      Terminal.mapx (fun tok ->
        branch
          (pint (Terminal.encode tok))
          (xsymbol (Symbol.T tok))
      ) @ [
        branch
          PWildcard
          (EComment ("This terminal symbol does not exist.",
                     eassertfalse))
      ]
    )
  )

(* -------------------------------------------------------------------------- *)

(* Produce a function that maps a (non-start) nonterminal symbol (represented
   as an integer code) to its representation as an [xsymbol]. *)

let nonterminal () =
  assert Settings.inspection;
  let nt = "nt" in
  def "nonterminal" @@
  EFun ([ PVar nt ],
    EMatch (EVar nt,
      Nonterminal.foldx (fun nt branches ->
        branch
          (pint (Nonterminal.encode nt))
          (xsymbol (Symbol.N nt))
        :: branches
      ) [
        branch
          PWildcard
          (EComment ("This nonterminal symbol does not exist.",
                     eassertfalse))
      ]
    )
  )

(* -------------------------------------------------------------------------- *)

(* Produce a mapping of every LR(0) state to its incoming symbol (encoded as
   an integer value). (Note that the initial states do not have one.) *)

let lr0_incoming () =
  assert Settings.inspection;
  marshal_1D_array "incoming symbol" "lr0_incoming" @@
  Lr0.init @@ fun node ->
  encode_symbol_option (Lr0.incoming_symbol node)

(* -------------------------------------------------------------------------- *)

(* A table that maps a production (i.e., an integer index) to the production's
   right-hand side. In principle, we use this table for ordinary productions
   only, as opposed to the start productions, whose existence is not exposed
   to the user. However, it is simpler (and not really costly) to include all
   productions in this table. *)

let rhs () =
  assert Settings.inspection;
  marshal_irregular_2D_array "right-hand side" "rhs" @@
  Production.init @@ fun prod ->
  Array.map encode_symbol (Production.rhs prod)

(* -------------------------------------------------------------------------- *)

(* A table that maps an LR(1) state to its LR(0) core. *)

let lr0_core () =
  assert Settings.inspection;
  marshal_1D_array "LR(0) core" "lr0_core" @@
  Lr1.init @@ fun node ->
  Lr0.encode (Lr0.ALR1.core (Lr1.state node))

(* A table that maps an LR(0) state to a set of LR(0) items. *)

let lr0_items () =
  assert Settings.inspection;
  marshal_irregular_2D_array "LR(0) items" "lr0_items" @@
  Lr0.init @@ fun node ->
  Array.map Item.marshal (Array.of_list (Item.Set.elements (Lr0.items node)))

(* -------------------------------------------------------------------------- *)

(* A table that tells which nonterminal symbols are nullable.
   (For simplicity, this table includes the start symbols.) *)

let nullable () =
  assert Settings.inspection;
  marshal_1D_array "nullable" "nullable" @@
  Nonterminal.init @@ fun nt ->
  encode_bool (Analysis.nullable nt)

(* -------------------------------------------------------------------------- *)

(* A two-dimensional bitmap, indexed first by nonterminal symbols, then by
   terminal symbols, encodes the FIRST sets. *)

let first () =
  assert Settings.inspection;
  marshal_2D_matrix "first" "first" @@
  Nonterminal.init @@ fun nt ->
  Terminal.initx @@ fun t ->
  encode_bool (TerminalSet.mem t (Analysis.first nt))

(* -------------------------------------------------------------------------- *)

(* A reference to [MenhirLib.StaticVersion.require_XXXXXXXX], where [XXXXXXXX]
   is our 8-digit version number. This ensures that the generated code can be
   linked only with an appropriate version of MenhirLib. This is important
   because we use unsafe casts, and a version mismatch could cause a crash. *)

let versiondef =
  pdef PUnit @@
  EVar (staticVersion ^ ".require_" ^ Version.version)

(* -------------------------------------------------------------------------- *)

(* Let's put everything together. *)

let grammar =
  Front.grammar

(* -------------------------------------------------------------------------- *)

(* Generated code for the inspection API. *)

let inspection_API () =

  (* Check that the type of every nonterminal symbol is known. *)

  Nonterminal.check_every_symbol_has_ocaml_type "inspection API";

  (* Define the internal sub-module [symbols], which contains type
     definitions. Then, include this sub-module. This sub-module is used
     again below, as part of the application of the functor
     [TableInterpreter.MakeInspection]. *)

  SIModuleDef (symbols, MStruct (
    interface_to_structure (
      tokengadtdef grammar @
      nonterminalgadtdef grammar
    )
  )) ::

  SIInclude (MVar symbols) ::

  (* Apply the functor [InspectionTableInterpreter.Make], which expects
     four arguments. *)
  SIInclude (mapp (MVar make_inspection) [
    (* Argument 1, of type [TableFormat.TABLES]. *)
    MVar tables;
    (* Argument 2, of type [InspectionTableFormat.TABLES]. *)
    MStruct (
      (* [lr1state] *)
      SIInclude (MVar ti) ::
      (* [terminal], [nonterminal]. *)
      SIInclude (MVar symbols) ::
      (* This functor application builds the types [symbol] and [xsymbol]
         in terms of the types [terminal] and [nonterminal]. This saves
         us the trouble of generating these definitions. *)
      SIInclude (MApp (MVar make_symbol, MVar symbols)) ::
      List.map valdef (
        terminal() ::
        nonterminal() ::
        lr0_incoming() @
        rhs() @
        lr0_core() @
        lr0_items() @
        nullable() @
        first() @
        []
      )
    );
    (* Argument 3, of type [EngineTypes.TABLE]. *)
    MVar et;
    (* Argument 4, of type [EngineTypes.ENGINE with ...]. *)
    MVar ti;
  ]) ::

  []

(* -------------------------------------------------------------------------- *)

(* The (optional) unparsing API. *)

let unparsing_API () : structure =
  let module A = struct
    (* A list of the start symbols and start states. *)
    let entry =
      fold_entry (fun _prod state nt _ty accu ->
        (nt, Lr1.encode state) :: accu
      ) []
  end in
  let module N = struct
    (* The internal name of the module that contains the parse tables. *)
    let tables = interpreter_submodule_name ^ "." ^ et
  end in
  let module C = UnparsingAPI.Code(Grammar)(A)(N)(Settings) in
  C.unparsing_API()

(* -------------------------------------------------------------------------- *)

(* All of the generated code (the parser, plus the optional APIs). *)

let program =

  [ SIFunctor (grammar.parameters,

    (* Make a reference to [MenhirLib.StaticVersion.require_XXXXXXXX], where
       [XXXXXXXX] is our 8-digit version number. This ensures that the
       generated code can be linked only with an appropriate version of
       MenhirLib. This is important because we use unsafe casts, and a
       version mismatch could cause a crash. *)

    SIComment "This generated code requires the following version of MenhirLib:" ::
    valdef versiondef ::

    (* Define the internal sub-module [Basics], which contains the definitions
       of the exception [Error] and of the type [token]. Then, include this
       sub-module. This sub-module is used again below, as part of the
       application of the functor [TableInterpreter.Make]. *)

    basics_submodule_def grammar @

    (* In order to avoid hiding user-defined identifiers, only the
       exception [Error] and the type [token] should be defined (at
       top level, with non-mangled names) above this line. We also
       define the value [_eRR] above this line so that we do not
       have a problem if a user prelude hides the name [Error]. *)

    SIFragment grammar.preludes ::

    (* Define the tables. *)

    SIModuleDef (tables,
      MStruct (
        (* The internal sub-module [Basics] contains the definitions of the
           exception [Error] and of the type [token]. *)
        SIInclude (MVar basics_submodule_name) ::

        (* This is a cascade of non-recursive definitions. An accessor
           function may need to access the table that is defined above it. The
           semantic actions come first so as to avoid a name collision. *)
        List.map valdef (
          semantic_action ::
          def "terminal_count" (EIntConst (Terminal.n - 1)) ::
          token2terminal ::
          def "error_terminal" (EIntConst (Terminal.encode Terminal.error)) ::
          token2value ::
          default_reduction @
          error @
          start_def ::
          action @
          lhs @
          goto @
          trace ::
          []
        )
      )
    ) ::

    SIModuleDef (interpreter_submodule_name, MStruct (

      (* Apply the functor [TableInterpreter.MakeEngineTable] to the tables. *)
      SIModuleDef (et, MApp (MVar make_engine_table, MVar tables)) ::
      (* Apply the functor [Engine.Make] to obtain an engine. *)
      SIModuleDef (ti, MApp (MVar make_engine, MVar et)) ::
      SIInclude (MVar ti) ::

      (* Optionally generate the inspection API. *)
      provided Settings.inspection inspection_API @

      []
    )) ::

    SIValDefs (false, monolithic_api) ::

    SIModuleDef (incremental_submodule_name, MStruct [
      SIValDefs (false, incremental_api)
    ]) ::

    (* Optionally generate the unparsing API. *)
    provided Settings.unparsing unparsing_API @

    SIFragment grammar.postludes ::

  [])]

let () =
  Time.stop start_time "Producing tables and abstract syntax"

end (* Run *)
