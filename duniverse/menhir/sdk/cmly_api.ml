(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This module defines a signature that describes the result of {!Cmly_read}. *)

(* The following signatures describe the API offered by the functor
   [Cmly_read.Read]. This functor reads in a .cmly file and gives
   access to the description of the grammar and automaton contained
   in this file. *)

(* This API is currently entirely self-contained, except for a reference
   to the module [Keyword], which is also part of [MenhirSdk]. *)

(**The module type [INDEXED] describes a type [t] whose elements are
   in a bijection with an integer interval of the form [\[0..count)]. *)
module type INDEXED = sig
  type t
  val count : int
  val of_int : int -> t
  val to_int : t -> int
  (* Hashing, equality and ordering. *)
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  (* Iteration and tabulation. *)
  val iter : (t -> unit) -> unit
  val fold : (t -> 'a -> 'a) -> 'a -> 'a
  val tabulate : (t -> 'a) -> t -> 'a
end

(**The module type [GRAMMAR] describes the grammar and automaton. *)
module type GRAMMAR = sig

  type terminal    = private int
  type nonterminal = private int
  type production  = private int
  type lr0         = private int
  type lr1         = private int
  type item        = production * int
  type ocamltype   = string
  type ocamlexpr   = string
  type identifier  = string

  module Range : sig
    type t
    val startp: t -> Lexing.position
    val endp: t -> Lexing.position
  end

  type 'a located = 'a * Range.t

  module Attribute : sig
    type t
    val label        : t -> string
    val has_label    : string -> t -> bool
    val payload      : t -> string
    val position     : t -> Range.t
  end

  module Action : sig
    type t
    val expr         : t -> ocamlexpr
    val keywords     : t -> Keyword.keyword list
  end

  module Surface : sig

    type filename = string
    type name = string

    module Priority_level : sig
      type t
      val input_file : t -> filename
      val level : t -> int
    end

    module Producer : sig
      type 'sym t
      val symbol : 'sym t -> 'sym
      val identifier : _ t -> identifier
      val attributes : _ t -> Attribute.t list
    end

    module Branch : sig
      type 'sym t
      val position         : _ t -> Range.t
      val producers        : 'sym t -> 'sym Producer.t list
      val action           : _ t -> Action.t
      val prec_annotation  : _ t -> name located option
      val production_level : _ t -> Priority_level.t
      val attributes       : _ t -> Attribute.t list
    end

    module Parameter : sig
      type t
      type desc =
        | Var of name
        | App of name * t list
        | Anonymous of t Branch.t list

      val desc : t -> desc
      val located : t -> Range.t
    end

    module Rule : sig
      type ('param, 'sym) t
      val parameters : ('param, _) t -> 'param
      val branches   : (_, 'sym) t -> 'sym Branch.t list
      val inline     : _ t -> bool
      val positions  : _ t -> Range.t list
      val public     : _ t -> bool
      val attributes : _ t -> Attribute.t list
    end

    module Token : sig
      type t

      type associativity =
        | LeftAssoc
        | RightAssoc
        | NonAssoc
        | UndefinedAssoc

      val ocamltype     : t -> ocamltype option
      val position      : t -> Range.t
      val alias         : t -> string option
      val attributes    : t -> Attribute.t list
      val associativity : t -> associativity
      val precedence    : t -> Priority_level.t located option
      val is_declared   : t -> bool
    end

    module Syntax : sig
      type ('param, 'sym) t
      val types  : (_, 'sym) t -> ('sym * ocamltype) list
      val tokens : _ t -> (name * Token.t) list
      val rules  : ('param, 'sym) t -> (name * ('param, 'sym) Rule.t) list
      type ground = (unit, name) t
      type higher = (name list, Parameter.t) t
    end

    val start_symbols : name list
    val on_error_reduce : (name * Priority_level.t) list
    val before_expansion : Syntax.higher
    val before_inlining : Syntax.ground

  end (* Surface *)

  module Grammar : sig
    val basename     : string
    val preludes     : string list
    val postludes    : string list
    val parameters   : string list
    val entry_points : (nonterminal * production * lr1) list
    val attributes   : Attribute.t list
  end

  module Terminal : sig
    include INDEXED with type t = terminal
    val name         : t -> string
    val kind         : t -> [`REGULAR | `ERROR | `EOF | `PSEUDO]
    val typ          : t -> ocamltype option
    val attributes   : t -> Attribute.t list
  end

  module Nonterminal : sig
    include INDEXED with type t = nonterminal
    val name         : t -> string
    val mangled_name : t -> string
    val kind         : t -> [`REGULAR | `START]
    val typ          : t -> ocamltype option
    val positions    : t -> Range.t list
    val nullable     : t -> bool
    val first        : t -> terminal list
    val attributes   : t -> Attribute.t list
  end

  module Symbol : sig

    type t =
      | T of terminal
      | N of nonterminal

    val name : ?mangled:bool -> t -> string

    (* Hashing, equality and ordering. *)
    val hash : t -> int
    val equal : t -> t -> bool
    val compare : t -> t -> int

  end

  (**The type [symbol] is a synonym for [Symbol.t]. *)
  type symbol = Symbol.t =
    | T of terminal
    | N of nonterminal

  (**[symbol_name] is a synonym for [Symbol.name]. *)
  val symbol_name : ?mangled:bool -> symbol -> string

  module Production : sig
    include INDEXED with type t = production
    val kind         : t -> [`REGULAR | `START]
    val lhs          : t -> nonterminal
    val rhs          : t -> (symbol * identifier * Attribute.t list) array
    val positions    : t -> Range.t list
    val action       : t -> Action.t option
    val attributes   : t -> Attribute.t list
        (* Before 2023/08/02, these were the attributes of the left-hand
           side of the production. Now, these are the attributes of the
           production itself. *)
  end

  module Lr0 : sig
    include INDEXED with type t = lr0
    val incoming     : t -> symbol option
    val items        : t -> item list
  end

  module Lr1 : sig
    include INDEXED with type t = lr1
    val lr0          : t -> lr0
    val transitions  : t -> (symbol * t) list
    val get_reductions : t -> (terminal * production) list
    val default_reduction : t -> production option

    val reductions   : t -> (terminal * production list) list
    [@@ocaml.deprecated "Please use [get_reductions]"]
  end

  module Print : sig
    open Format
    val terminal            : formatter -> terminal -> unit
    val nonterminal         : formatter -> nonterminal -> unit
    val symbol              : formatter -> symbol -> unit
    val mangled_nonterminal : formatter -> nonterminal -> unit
    val mangled_symbol      : formatter -> symbol -> unit
    val production          : formatter -> production -> unit
    val item                : formatter -> item -> unit
    val itemset             : formatter -> item list -> unit
    val annot_item          : string      list -> formatter -> item      -> unit
    val annot_itemset       : string list list -> formatter -> item list -> unit
  end

end (* GRAMMAR *)
