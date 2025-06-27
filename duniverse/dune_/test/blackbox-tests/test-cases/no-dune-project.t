When no dune-project file is present, a warning is printed.

  $ touch dune
  $ dune build
  Warning: No dune-project file has been found. A default one is assumed but
  the project might break when dune is upgraded. Please create a dune-project
  file.
  Hint: generate the project file with: $ dune init project <name>

In release mode, this is fatal.

  $ dune build --release
  Error: No dune-project file has been found. A default one is assumed but the
  project might break when dune is upgraded. Please create a dune-project file.
  Hint: generate the project file with: $ dune init project <name>
  [1]

This corresponds to a flag:

  $ dune build --require-dune-project-file
  Error: No dune-project file has been found. A default one is assumed but the
  project might break when dune is upgraded. Please create a dune-project file.
  Hint: generate the project file with: $ dune init project <name>
  [1]

Test case: warning should be emitted

  $ mkdir nested-case && cd nested-case
  $ mkdir a && touch a/dune
  $ dune build
  Warning: No dune-project file has been found. A default one is assumed but
  the project might break when dune is upgraded. Please create a dune-project
  file.
  Hint: generate the project file with: $ dune init project <name>
  $ cd ..

Test case: warning should not be emitted

  $ mkdir another-case && cd another-case
  $ mkdir a && touch a/dune
  $ echo "(lang dune 3.0)" > a/dune-project
  $ cp -R a b
  $ dune build
  $ cd ..
