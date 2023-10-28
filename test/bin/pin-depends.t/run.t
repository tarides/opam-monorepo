We have a simple project with a single package defined at the root.
It has a `x-opam-monorepo-opam-repositories` field set to use a local
opam-repository for locking and a pinned package.

  $ cat pin-depends.opam
  opam-version: "2.0"
  depends: [
    "dune"
    "b"
    "c"
  ]
  pin-depends: ["b.dev" "./duniverse/b"]
  x-opam-monorepo-opam-provided: ["b"]
  x-opam-monorepo-opam-repositories: [
    "file://$OPAM_MONOREPO_CWD/minimal-repo"
    "file://$OPAM_MONOREPO_CWD/repo"
  ]

We provided a minimal opam-repository but locking should be successful.

  $ gen-minimal-repo
  $ opam-monorepo lock
  ==> Using 1 locally scanned package as the target.
  ==> Found 10 opam dependencies for the target package.
  ==> Querying opam database for their metadata and Dune compatibility.
  ==> Calculating exact pins for each of them.
  ==> Wrote lockfile with 1 entries to $TESTCASE_ROOT/pin-depends.opam.locked. You can now run opam monorepo pull to fetch their sources.

The lockfile should contain the base packages, dune and our 2 dependencies
`b` and `c` which should be pulled in the duniverse

  $ cat pin-depends.opam.locked | sed 's|file://.*/pin-depends.t/|file://$LOCAL_PATH/|'
  opam-version: "2.0"
  synopsis: "opam-monorepo generated lockfile"
  maintainer: "opam-monorepo"
  depends: [
    "b" {= "dev"}
    "base-bigarray" {= "base"}
    "base-threads" {= "base"}
    "base-unix" {= "base"}
    "c" {= "1" & ?vendor}
    "dune" {= "2.9.1"}
    "ocaml" {= "4.13.1"}
    "ocaml-base-compiler" {= "4.13.1"}
    "ocaml-config" {= "2"}
    "ocaml-options-vanilla" {= "1"}
  ]
  pin-depends: [
    "b.dev"
    "file://$LOCAL_PATH/duniverse/b"
  ]
  x-opam-monorepo-duniverse-dirs: [
    [
      "https://c.com/c.tbz"
      "c"
      [
        "sha256=0000000000000000000000000000000000000000000000000000000000000001"
      ]
    ]
  ]
  x-opam-monorepo-opam-provided: ["b"]
  x-opam-monorepo-opam-repositories: [
    "file://$OPAM_MONOREPO_CWD/minimal-repo" "file://$OPAM_MONOREPO_CWD/repo"
  ]
  x-opam-monorepo-root-packages: ["pin-depends"]
  x-opam-monorepo-version: "0.3"
