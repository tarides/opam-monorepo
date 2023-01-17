We have two packages that use the same dev-repo

  $ cat no-dedup.opam
  opam-version: "2.0"
  depends: [
    "dune"
    "a-dev-repo"
    "same-dev-repo"
  ]
  x-opam-monorepo-opam-repositories: [
    "file://$OPAM_MONOREPO_CWD/minimal-repo"
    "file://$OPAM_MONOREPO_CWD/repo"
  ]

We provided a minimal opam-repository and locking should produce a solution. It
should deduplicate packages with the same dev-repo by default and collect all
the dune package names together:

  $ gen-minimal-repo
  $ opam-monorepo lock
  ==> Using 1 locally scanned package as the target.
  ==> Found 10 opam dependencies for the target package.
  ==> Querying opam database for their metadata and Dune compatibility.
  ==> Calculating exact pins for each of them.
  opam-monorepo: [WARNING] The following packages come from the same repository https://github.com/tarides/some-dev-repo but are associated with different URLs:
  same-dev-repo.1: https://tarides.com/same-dev-repo.tbz
  a-dev-repo.1: https://tarides.com/a-dev-repo.tbz
  The url for the highest versioned package was selected: same-dev-repo.1: https://tarides.com/same-dev-repo.tbz
  ==> Wrote lockfile with 1 entries to $TESTCASE_ROOT/no-dedup.opam.locked. You can now run opam monorepo pull to fetch their sources.
  $ opam show --just-file --raw -fx-opam-monorepo-duniverse-dirs ./no-dedup.opam.locked
  [
    url {"https://tarides.com/same-dev-repo.tbz"}
    dir {"some-dev-repo"}
    hashes
      {"sha256=0000000000000000000000000000000000000000000000000000000000000000"}
    dune-packages {"same-dev-repo" "a-dev-repo"}
  ]

If we add a newer version of the other package, it should instead pick that one:

  $ cp -a repo/packages/a-dev-repo/a-dev-repo.1 repo/packages/a-dev-repo/a-dev-repo.2
  $ opam-monorepo lock
  ==> Using 1 locally scanned package as the target.
  ==> Found 10 opam dependencies for the target package.
  ==> Querying opam database for their metadata and Dune compatibility.
  ==> Calculating exact pins for each of them.
  opam-monorepo: [WARNING] The following packages come from the same repository https://github.com/tarides/some-dev-repo but are associated with different URLs:
  same-dev-repo.1: https://tarides.com/same-dev-repo.tbz
  a-dev-repo.2: https://tarides.com/a-dev-repo.tbz
  The url for the highest versioned package was selected: a-dev-repo.2: https://tarides.com/a-dev-repo.tbz
  ==> Wrote lockfile with 1 entries to $TESTCASE_ROOT/no-dedup.opam.locked. You can now run opam monorepo pull to fetch their sources.
  $ opam show --just-file --raw -fx-opam-monorepo-duniverse-dirs ./no-dedup.opam.locked
  [
    url {"https://tarides.com/a-dev-repo.tbz"}
    dir {"some-dev-repo"}
    hashes
      {"sha256=0000000000000000000000000000000000000000000000000000000000000000"}
    dune-packages {"same-dev-repo" "a-dev-repo"}
  ]


If we disable deduplication we should get a different result, namely both
`a-dev-repo` and `same-dev-repo` be part of the solution:

  $ opam-monorepo lock --deduplicate-packages=false
  ==> Using 1 locally scanned package as the target.
  ==> Found 10 opam dependencies for the target package.
  ==> Querying opam database for their metadata and Dune compatibility.
  ==> Calculating exact pins for each of them.
  ==> Wrote lockfile with 2 entries to $TESTCASE_ROOT/no-dedup.opam.locked. You can now run opam monorepo pull to fetch their sources.

The packages should both be in there, with each their own unique `dir` and each
just responsible for its own dune package (that is read from the `dune build -p
<name>` invocation):

  $ opam show --just-file --raw -fx-opam-monorepo-duniverse-dirs ./no-dedup.opam.locked
  [
    url {"https://tarides.com/a-dev-repo.tbz"}
    dir {"a-dev-repo.2"}
    hashes
      {"sha256=0000000000000000000000000000000000000000000000000000000000000000"}
    dune-packages {"a-dev-repo"}
  ]
  [
    url {"https://tarides.com/same-dev-repo.tbz"}
    dir {"same-dev-repo.1"}
    hashes
      {"sha256=0000000000000000000000000000000000000000000000000000000000000000"}
    dune-packages {"same-dev-repo"}
  ]
