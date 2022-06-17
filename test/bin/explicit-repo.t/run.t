We want to specify an opam-repository that is a git URL. For reproducibility we
want the locked repository URL to be fixed to the commit that we have used when
locking.

We start with a minimal `opam-repository`:

  $ gen-minimal-repo

We also want to create an initial repo that is a git repo

  $ mkdir -p git-repository/packages/git-dep/git-dep.1.0
  $ cat >git-repository/packages/git-dep/git-dep.1.0/opam <<EOF
  > opam-version: "2.0"
  > depends: [
  >   "dune"
  > ]
  > EOF
  $ cat >git-repository/repo <<EOF
  > opam-version: "2.0"
  > EOF
  $ (cd git-repository ; git init ; git add -A ; git commit -m "initial commit" > /dev/null)
  Initialized empty Git repository in $TESTCASE_ROOT/git-repository/.git/
  $ SHORT_HASH=$(git -C git-repository rev-parse --short HEAD)

We have a file that depends on package "git-dep".

  $ opam show --just-file --raw -fdepends ./explicit-repo.opam
  dune, git-dep

It should lock successfully.

  $ opam-monorepo lock explicit-repo > /dev/null

The lock file should have the a git repo that ends with "#$SHORT_HASH"

  $ opam show --just-file --raw -fx-opam-monorepo-opam-repositories ./explicit-repo.opam.locked > locked-repos
  $ grep "#$SHORT_HASH" locked-repos > /dev/null
