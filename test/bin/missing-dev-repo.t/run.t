We have a simple project with a single package defined at the root.
It has a `x-opam-monorepo-opam-repositories` field set to use a local
opam-repository for locking

  $ cat missing-dev-repo.opam
  opam-version: "2.0"
  depends: [
    "dune"
    "no-dev-repo"
  ]
  x-opam-monorepo-opam-repositories: [
    "file://$OPAM_MONOREPO_CWD/minimal-repo"
    "file://$OPAM_MONOREPO_CWD/repo"
  ]

We provided a minimal opam-repository but locking should be successful.

  $ gen-minimal-repo
  $ opam-monorepo lock
  ==> Using 1 locally scanned package as the target.
  ==> Found 9 opam dependencies for the target package.
  ==> Querying opam database for their metadata and Dune compatibility.
  ==> Calculating exact pins for each of them.
  opam-monorepo: [WARNING] Package no-dev-repo.1 has no dev-repo specified, but it needs a dev-repo to be successfully included in the duniverse.
  ==> Wrote lockfile with 0 entries to $TESTCASE_ROOT/missing-dev-repo.opam.locked. You can now run opam monorepo pull to fetch their sources.
