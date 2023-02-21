We have a project which depends on multiple packages with different dev-repos
but which happen to resolve to the same directory under duniverse. This tests
that such a situation results in an explicit error.

  $ gen-minimal-repo
  $ opam-monorepo lock
  ==> Using 1 locally scanned package as the target.
  ==> Found 11 opam dependencies for the target package.
  ==> Querying opam database for their metadata and Dune compatibility.
  ==> Calculating exact pins for each of them.
  opam-monorepo: [ERROR] Multiple dev-repos would be vendored into the directory: duniverse/project
  Dev-repos:
  - git+https://github.com/foo/project.git#branch
  - git+https://github.com/foo/project
  - git+https://github.com/bar/project
  [1]
