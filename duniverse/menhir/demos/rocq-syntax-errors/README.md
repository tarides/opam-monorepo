# rocq-syntax-errors : a demo of Parsing with error messages in Rocq

Brian Ward, 2021

License: CC0

This is a toy demo of the Rocq backend of `menhir` that also uses the error
messaging functionality. It takes input in the form of a file name on the
command line.

Build with `make`. (This assumes that `coq-menhirlib` is installed.)
Run the provided tests with `make test`.

It is based on `demos/rocq-minicalc` and `demos/calc-syntax-errors`.
