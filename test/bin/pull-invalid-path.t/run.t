Running `opam monorepo pull` uses dev repo names (usually derived from the
dev_repo field in the package's opam file) to name the directories that will
contain the source code of packages inside the "duniverse" directory. We want to
make sure that adversarial dev repo names don't cause files to be created or
deleted outside the duniverse directory.

Attempt to pull with a lockfile containing a package whose name is the empty
string:
  $ opam-monorepo pull --lockfile=lockfile-refers-to-duniverse-directory.opam.locked
  ==> Using lockfile lockfile-refers-to-duniverse-directory.opam.locked
  opam-monorepo: [ERROR] Refusing to pull https://foo.com/foo.tbz into directory $TESTCASE_ROOT/duniverse/ as it is not inside the directory $TESTCASE_ROOT/duniverse
  [1]

Attempt to pull with a lockfile containing a package whose name is "..":
  $ opam-monorepo pull --lockfile=lockfile-refers-to-parent-directory.opam.locked
  ==> Using lockfile lockfile-refers-to-parent-directory.opam.locked
  opam-monorepo: [ERROR] Refusing to pull https://foo.com/foo.tbz into directory $TESTCASE_ROOT/duniverse/.. as it is not inside the directory $TESTCASE_ROOT/duniverse
  [1]
