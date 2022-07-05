# Releasing

Releasing opam-monorepo requires a bit of manual work because we decided
to include the duniverse in the release tarball.

That allows to ship opam-monorepo free of any dependencies which makes
installing it easy and seemless to users.

This also makes a good test case for this workflow and can help us identify how
to improve it so others can adopt it.

## Before the release

There are a couple of preliminary steps that need to happen before editting
the changelog and running `dune-release`. Those steps have to be reversed after
the release to resume to our regular dev state so it's good to include them in a
single commit.

There are two important things to do in this commit. Include the duniverse and
prepare the opam file for opam-repository.

### Including the duniverse

First make sure your local duniverse is up to date with the lock file by
running:

```
opam monorepo pull
```

Once this is done, you can include the duniverse by running:

```
git add -f duniverse
```

The `-f` is important for two reasons here:
- The duniverse folder is in our `.gitignore` and can't be added without it
- We want all of the duniverse folder to be included. It happens that files
  that are included in tarballs are generated and otherwise gitignored in
  some projects meaning a regular git add could take the subdir `.gitignore`
  and exclude those files.

### Preparing the opam file

The following changes don't actually touch the opam file so once you are done
applying those changes remember to run

```
dune build opam-monorepo.opam
```

to generate the opam file again.

#### Stripping the dependencies

Since our dependencies are included in the tarball's duniverse we don't need
to declare them as opam dependencies. The only opam dependencies we have are
`ocaml` and `dune`.

You need to edit the `dune-project` accordingly as our opam file is generated.
Remove everything from the `depends` field of the `package` stanza except for
ocaml. The diff usually looks something like this:

```
  (depends
-  (ocaml (>= 4.10.0))
-  dune-build-info
-  base
-  bos
-  (cmdliner (>= 1.1.0))
-  fmt
-  logs
-  (opam-file-format (>= 2.1.0))
-  (opam-format (>= 2.1.0~rc2))
-  (opam-state (>= 2.1.0~rc2))
-  opam-0install
-  ocaml-version
-  sexplib
-  uri
-  (alcotest :with-test))
+  (ocaml (>= 4.10.0)))
  (conflicts
```

#### Bumping the dune lower bound

Since we vendor our dependencies, we also vendor `dune-configurator` that comes
from the `dune` repository. That means that most of the time we vendor a
repository that uses the latest dune lang. Using any older version of dune to
build opam-monorepo from the tarball is therefore bound to fail.

We need to make sure that our depends field express that, i.e. that we depend on
dune greater or equal to the version in the lock file.

This is done by bumping the declared lang  version in the `dune-project` file.

The lang version should correspond to the `major.minor` version of dune in our
lock file at the time of the release.

For example if the lock file has the following dune dependency:

```
  "dune" {= "3.2.0"}
```

The diff should look like this:

```
-(lang dune 2.7)
+(lang dune 3.2)
 (generate_opam_files true)
```

#### Including transitive depexts

Since we do not declare our opam dependencies anymore, we need to make sure
their depexts are declared in our opam file.

The depext field is not directly handled by dune, we use the `.opam.template`
file here instead.

To include the transitive depexts we need to copy the `depext` field from
the lock file to the `opam-monorepo.opam.template` file.

#### Disabling integration tests in opam

Our integration test suite uses the opam binary and this has been causing
troubles in the past when our tests are run by opam.

We take the easy way out there and just disable those tests in opam, still
running the unit tests suite. This is done by editting the build command
in `opam-monorepo.opam.template`. The diff usually looks like this:

```
-build: [ "dune" "build" "-p" name "-j" jobs "@install" "@runtest" {with-test} ]
+build: [ "dune" "build" "-p" name "-j" jobs "@install" "@test/lib/runtest" {with-test} ]
```

The key part here is that instead of building the `@runtest` alias when
`with-test` is true, we run the `@test/lib/runtest` alias, i.e. all rules
attached to `runtest` in `test/lib` which is where our unit tests are defined.

## During the release

Once all of the above changes have been committed you can proceed with the
regular dune-release workflow.

Once you are through and the opam-repository PR is created you need to rewrite
the depexts for opam-repository.
The opam-repository maintainers prefer packages not to define their own depext
formula but to use the proxy `conf-*` packages instead.
To comply with this convention we edit the opam file we submit to
opam-repository and delete the `depext` field and add the corresponding entries
to our `depends` field. Finding out which package we need to depend upon
requires a bit of practice but at the time of writing this document, we only
need to add a dependency to `conf-pkg-config`. The diff looks like this:

```
--- a/packages/opam-monorepo/opam-monorepo.0.3.3/opam
+++ b/packages/opam-monorepo/opam-monorepo.0.3.3/opam
@@ -16,6 +16,7 @@ depends: [
   "dune" {>= "3.2"}
   "ocaml" {>= "4.10.0"}
   "odoc" {with-doc}
+  "conf-pkg-config"
 ]
 conflicts: [
   "dune-build-info" {= "2.7.0" | = "2.7.1"}
@@ -24,25 +25,6 @@ conflicts: [
 dev-repo: "git+https://github.com/ocamllabs/opam-monorepo.git"
 build: [ "dune" "build" "-p" name "-j" jobs "@install" "@test/lib/runtest" {with-test} ]
 flags: [ plugin ]
-depexts: [
-  ["devel/pkgconf"] {os = "openbsd"}
-  ["pkg-config"] {os-family = "debian"}
-  ["pkg-config"] {os = "macos" & os-distribution = "homebrew"}
-  ["pkgconf"] {os = "freebsd"}
-  ["pkgconf"] {os-distribution = "alpine"}
-  ["pkgconf"] {os-distribution = "arch"}
-  ["pkgconf-pkg-config"] {os-distribution = "fedora"}
-  ["pkgconf-pkg-config"] {os-distribution = "mageia"}
-  ["pkgconf-pkg-config"] {os-distribution = "centos" & os-version >= "8"}
-  ["pkgconf-pkg-config"] {os-distribution = "ol" & os-version >= "8"}
-  ["pkgconf-pkg-config"] {os-distribution = "rhel" & os-version >= "8"}
-  ["pkgconfig"] {os-distribution = "nixos"}
-  ["pkgconfig"] {os = "macos" & os-distribution = "macports"}
-  ["pkgconfig"] {os-distribution = "centos" & os-version <= "7"}
-  ["pkgconfig"] {os-distribution = "ol" & os-version <= "7"}
-  ["pkgconfig"] {os-distribution = "rhel" & os-version <= "7"}
-  ["system:pkgconf"] {os = "win32" & os-distribution = "cygwinports"}
-]
 url {
```

Remember to push those changes to update the PR.

## After the release

Now that the release has been accepted, you can revert the commit you introduced
in the preliminary stages. That will reintroduce our dependencies (that we need
to generate the lock file) and unversion the duniverse.
