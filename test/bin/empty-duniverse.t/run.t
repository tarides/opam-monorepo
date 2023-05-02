One should be able to generate a lock file where no dependencies need to
be vendored and that should result in an empty duniverse.
That can happen if one has no external deps besides 'dune' and 'ocaml'
or if those are marked as 'opam-provided' for example.

Here our local package only depends on 'ocaml' and 'dune':

  $ opam show --just-file -fdepends ./empty-duniverse.opam
  ocaml, dune

We should be able to successfully lock:

  $ gen-minimal-repo
  $ opam-monorepo lock > /dev/null

And the lock file should not contain anything to vendor:

  $ opam show --just-file -fdepends ./empty-duniverse.opam.locked | grep "?vendor"
  [1]

Finally, we should be able to pull this lock file, the tool will inform us that
there is nothing to pull and will not create a duniverse:

  $ opam-monorepo pull
  ==> Using lockfile $TESTCASE_ROOT/empty-duniverse.opam.locked
  ==> No dependencies to pull, there's nothing to be done here!
  $ find . -type d -name 'duniverse'
