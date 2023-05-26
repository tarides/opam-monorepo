This is a reproduction of a problem where depending on a specific version of the
ocaml compiler at the same time as a package that doesn't build with dune leads
to an error message that doesn't mention the fact that the problem is due to the
non-dune dependency, despite the fact that removing that dependency or adding
an overlay containing a suitable dune port for the package causes the problem to
go away.

  $ gen-minimal-repo

  $ opam-monorepo lock
  ==> Using 1 locally scanned package as the target.
  opam-monorepo: [ERROR] Can't find all required versions.
  Selected: base-bigarray.base base-threads.base base-unix.base ocaml.4.13.1
            ocaml-base-compiler.4.13.1 ocaml-config.2 ocaml-options-vanilla.1
            ocaml-base-compiler&foo ocaml-base-compiler ocaml-base-compiler
            ocaml-base-compiler
  - foo -> (problem)
      Rejected candidates:
        foo.zdev: Requires ocaml = 4.14.1
  [1]
