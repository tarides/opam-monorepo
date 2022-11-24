We have a simple project which depends on another package

  $ opam show --just-file -fdepends ./this/this.opam
  dune, opam-monorepo-test-other

`git ls-remote` is a bit stupid and for some reason requires a valid `.git`
folder and the dune cram tests don't have one. So let's create one here:

  $ git init > /dev/null 2>&1

Now let's create a git pin:

  $ cd opam-monorepo-test-other
  $ git init > /dev/null 2>&1
  $ git add opam-monorepo-test-other.opam
  $ git commit -m "Initial commit" > /dev/null
  $ cd ..

Let's add this pin and pin it to `HEAD`.

  $ cd this
  $ opam pin --yes --no-action add --kind git opam-monorepo-test-other '../opam-monorepo-test-other#HEAD' > /dev/null 2>&1

Locking with this pin should work.

  $ opam-monorepo lock > /dev/null 2>&1

We need to clean up since this modifies global state.

  $ opam pin remove opam-monorepo-test-other > /dev/null
  $ cd ..
