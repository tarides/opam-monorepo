If users want to symlink out folders of the `duniverse` to work on them, it
should be possible to `pull` without the symlink being overwritten.

Say, we have a project that depends on a package `symlinked-dep`.

  $ opam show --just-file --raw -fdepends ./preserve-symlinks.opam
  dune, symlinked-dep

We start with a minimal `opam-repository` as well as a local repo that has a
package called `symlinked-dep` which should make opam-monorepo be able to pick
up this dependency into the lockfile:

  $ gen-minimal-repo
  $ opam-monorepo lock > /dev/null
  $ opam show --just-file --raw -fdepends ./preserve-symlinks.opam.locked | grep symlinked-dep
  "symlinked-dep" {= "1.0" & ?vendor}

Let's say we have `symlinked-dep` as a repo somewhere else, where we're working
on it.

  $ mkdir symlinked-dep
  $ echo "Yes, it is here" > symlinked-dep/symlinked-dep-is-here

We now create a `duniverse/` folder that has a symlink to `symlinked-dep`. This
should allow `dune` to pick it up.

  $ mkdir duniverse
  $ ln -s ../symlinked-dep duniverse/symlinked-dep
  $ cat duniverse/symlinked-dep/symlinked-dep-is-here
  Yes, it is here

Now we ask to pull, while telling opam-monorepo to preserve symlinks:

  $ opam-monorepo pull --keep-symlinked-dir > /dev/null 2>&1

This should leave us with a `duniverse/` folder where the previous symlink
still exists and still points to the right place:

  $ cat duniverse/symlinked-dep/symlinked-dep-is-here
  Yes, it is here
