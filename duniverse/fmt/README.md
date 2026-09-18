Fmt — OCaml Format pretty-printer combinators
=============================================

Fmt exposes combinators to devise `Format` pretty-printing functions.

Fmt depends only on the OCaml standard library. The optional `Fmt_tty`
library that allows to setup formatters for terminal color output
depends on the Unix library. The optional `Fmt_cli` library that
provides command line support for Fmt depends on [`cmdliner`].

Fmt is distributed under the ISC license.

Home page: <http://erratique.ch/software/fmt>

[`cmdliner`]: http://erratique.ch/software/cmdliner

## Installation

Fmt can be installed with `opam`:

    opam install fmt
    opam install base-unix cmdliner fmt # Install all optional libraries

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation can be consulted [online] or via `odig doc fmt`.

Questions are welcome but better asked on the [OCaml forum] than on the
issue tracker.

[online]: http://erratique.ch/software/fmt/doc/
[OCaml forum]: https://discuss.ocaml.org/
