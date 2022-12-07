We want to make sure that packages that are marked with the `avoid-version`
flag are avoided, but if explicitly asked the solver will pick them.

We have a project that depends on a package `a` and `b`

  $ opam show --just-file --raw -fdepends ./avoid-versions.opam
  dune, a, b

We use the minimal repository to set up the environment.

  $ gen-minimal-repo

Package `a` version 1.0 is a regular package

  $ opam show --just-file -fflags repo/packages/a/a.1.0/opam
  

No red flags here, but 2.0~beta1 is supposed to be avoided:

  $ opam show --just-file -fflags repo/packages/a/a.2.0~beta1/opam
  avoid-version

Locking that way should yield a solution that uses `a.1.0` and avoids the 2.0
beta release:

  $ opam-monorepo lock avoid-versions.opam > /dev/null
  $ grep -E "\"a\" \{" avoid-versions.opam.locked
    "a" {= "1.0" & ?vendor}

If however we request a version that is newer (using the `~` constraint, which
should match the beta version), the `avoid-version` should be overridden and
pick 2.0.

  $ opam show --just-file --raw -fdepends ./force-versions.opam
  "dune" "a" {>= "2.0~"} "b"
  $ opam-monorepo lock force-versions.opam > /dev/null
  $ grep -E "\"a\" \{" force-versions.opam.locked
    "a" {= "2.0~beta1" & ?vendor}

What is the dependency on `b` good for? It is to make sure that the
deprioritizing of `a.2.0~beta1` works even in the presence of other packages.
