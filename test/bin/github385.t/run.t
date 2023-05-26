This test asserts that the issue at
https://github.com/tarides/opam-monorepo/issues/385 is fixed.

The problem was that depending on a specific version of the ocaml compiler at
the same time as a package that doesn't build with dune leads to an error
message that doesn't mention the fact that the problem is due to the non-dune
dependency, despite the fact that removing that dependency or adding an overlay
containing a suitable dune port for the package causes the problem to go away.

  $ gen-minimal-repo

  $ opam-monorepo lock
  ==> Using 1 locally scanned package as the target.
  opam-monorepo: [ERROR] Some dependencies cannot be built with dune!
  
  opam-monorepo requires that all dependencies use dune as their build system.
  
  These dependencies (possibly transitive) don't use dune as their build system:
  - without-dune
  
  The dune-universe opam repository (git+https://github.com/dune-universe/opam-overlays.git) contains dune ports of some popular packages to help build more packages with dune. It doesn't appear to be set up on this switch. Adding it to this switch may fix this issue. Add the dune-universe opam repository to this switch by running the command:
  
  opam repository add dune-universe git+https://github.com/dune-universe/opam-overlays.git
  [1]
