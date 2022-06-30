We want to specify an opam-repository that is a git URL. For reproducibility we
want the locked repository URL to be fixed to the commit that we have used when
locking.

We have a project that depends on a package "git-dep".

  $ opam show --just-file --raw -fdepends ./explicit-repo.opam
  dune, git-dep

We start with a minimal `opam-repository`, but the package "git-dep" is not in
the minimal repository created locally:

  $ gen-minimal-repo
  $ ls minimal-repo/packages | grep git-dep
  [1]

"git-dep" is in a repository that is accessible via git, create this repository:

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
  $ (cd git-repository ; git init ; git add -A ; git commit -m "initial commit") > /dev/null 2>&1
  $ SHORT_HASH=$(git -C git-repository rev-parse --short HEAD)

To find this package, we need to have the git repository in our
`x-opam-monorepo-opam-repositories` list:

  $ opam show --just-file --raw -fx-opam-monorepo-opam-repositories ./explicit-repo.opam
  file://$OPAM_MONOREPO_CWD/minimal-repo, git+file://$OPAM_MONOREPO_CWD/git-repository

Having it in there should make opam-monorepo able to find the package and lock
successfully and picked up the package:

  $ opam-monorepo lock explicit-repo > /dev/null
  $ opam show --just-file --raw -fdepends ./explicit-repo.opam.locked | grep git-dep
  "git-dep" {= "1.0"}

To make the build reproducible, opam-monorepo should have replaced the URL to
the git repo by an URL to the git repo that features the commit hash of the
repository at the time of locking, so users of the lock file will check out the
git repository in the same state as the creator of the lock file.

Therefore the list of repositories in the lockfile should have the a git repo
that's the same as the previous git-repository path but end with "#$SHORT_HASH"

  $ opam show --just-file --raw -fx-opam-monorepo-opam-repositories ./explicit-repo.opam.locked > locked-repos
  $ grep -Po ".+(?=$SHORT_HASH)" locked-repos
  git+file://$OPAM_MONOREPO_CWD/git-repository#

We also need to make sure that the git cache gets updated when the repository is updated:

  $ mkdir -p git-repository/packages/git-dep/git-dep.2.0
  $ cat >git-repository/packages/git-dep/git-dep.2.0/opam <<EOF
  > opam-version: "2.0"
  > depends: [
  >   "dune"
  > ]
  > EOF
  > (cd git-repository ; git add -A ; git commit -m "New release") > /dev/null 2>&1

Thus a new version of the package has been released in the git repo. Locking it
anew should pick this version of the dependency:

  $ opam-monorepo lock explicit-repo > /dev/null
  $ opam show --just-file --raw -fdepends ./explicit-repo.opam.locked | grep git-dep
  "git-dep" {= "2.0"}
