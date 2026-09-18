From Coq Require Import ExtrOcamlIntConv.
Require MiniCalc.Parser.
Require Extraction.
Set Extraction Output Directory ".".

(* Datatypes *)
Extract Inlined Constant Datatypes.fst => "fst".
Extract Inlined Constant Datatypes.snd => "snd".
Extract Inductive prod => "( * )" [ "" ].

Extract Inlined Constant Parser.Ast.string => "String.t".
Extract Constant Parser.Ast.loc => "Lexing.position * Lexing.position".

Set Extraction AccessOpaque.
Set Warnings "-extraction-opaque-accessed".

Set Extraction Output Directory "extraction".

Separate Extraction nat_of_int int_of_n.
Recursive Extraction Library Parser.
