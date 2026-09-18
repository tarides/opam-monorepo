This is a variant of the demo `calc-unparsing`.

The grammar in `parser.mly` is slightly more complex and involves
mutually recursive nonterminal symbols `expr` and `raw_expr`.

We check that, in spite of this, the unparsing process works.
