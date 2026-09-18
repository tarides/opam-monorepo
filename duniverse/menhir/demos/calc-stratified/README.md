This is a variant of the demo `calc-mini`.

In this demo, instead of using precedence declarations (`%left`),
we explicitly stratify the grammar of expressions into three levels,
namely `factor`, `term`, and `expr`.

Because the code back-end performs Unit Production Elimination (UPE),
the generated code is expected to be as efficient as in `calc-mini`.
