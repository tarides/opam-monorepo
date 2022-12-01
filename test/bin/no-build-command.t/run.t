Test that packages with no build commands and no direct dependency on dune and
no build command are not assumed to not build with dune.

The repo contains 4 packages:
- with-dune: directly depends on dune and has a build command
- without-dune: doesn't depend on dune and has a build command
- depend-with-dune: depends on with-dune and doesn't have a build command
- depend-without-dune: depends on without-dune and doesn't have a build command

This test asserts that it's an error to generate a lockfile for a package
depending on depend-without-dune due to the transitive dependency on
without-dune, but that it's not an error to generate a lockfile for a package
which only depends on depend-with-dune, despite the latter not directly
depending on dune.

We setup the default base repository

  $ gen-minimal-repo

Attempt to generate a lockfile for a package which depends on
depend-without-dune (this should fail due to the transitive dependency on
without-dune which has a build command but doesn't depend on dune)

  $ opam-monorepo lock test-depend-without-dune.opam 2>&1 | grep -o "Doesn't build with dune"
  Doesn't build with dune

Attempt to generate a lockfile for a package which depends only on
depend-with-dune

  $ opam-monorepo lock test-depend-with-dune.opam > /dev/null
