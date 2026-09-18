# Steps to follow for each release

## Finalise opam code for release
* update version in all the opam files and in configure.ac
* run `make configure` to regenerate `./configure` [checked by github actions]
* update copyright headers
* if you're releasing the first final release of a new branch (e.g. 2.2.0) and the `root_version` has changed since the previous stable version (e.g. 2.1.6): make sure `root_version` in OpamFile.ml is set to the final release number (e.g. for 2.2.0, `root_version` should be 2.2). Make sure that `opamFormatUpgrade.ml` also contains an upgrade function from the previous version (that function will most likely be empty), and that `opamroot-versions.test` is updated accordingly too.
* run `make tests`, `opam-rt` [checked by github actions]
* update the CHANGE file: take `master_changes.md` content to fill it

## Github release

[ once bump version & changes PRs merged ]
* tag the release (git tag -am 2.2.0 2.2.0; git push origin 2.2.0)
* on an alpha1, create a branch `x.y` and push it
* /!\ Once the tag pushed, it can be updated [different commit] only in case of severe issue
* create a release (or prerelease if intermediate release) draft on github based on your tag (https://github.com/ocaml/opam/releases/new)
* add releases notes (content of `master_changes.md`) in the release draft

## Prepare blog posts

* propose the release draft for review
* prepare the blog entry in opam.ocaml.org and propose it for review
* prepare the announcement on discuss.ocaml.org and propose it for review

## Pre-built binaries creation

* If you do not have `Windows-10-x86_64.qcow2` already, read the instruction in the `windows.md` file
* Make sure your macOS system, Docker installation and brew packages are up-to-date
* Make sure the repository is in the correct state: `git switch --detach <tag>`
* launch docker using the Docker GUI macOS app
* generate opam artifacts, using `release/release.sh <tag>` from a macOS/arm64 machine, it requires to have Docker and QEMU installed (see below device requirements)
* generate the signatures using `release/sign.sh <tag>`
* upload everything from `release/out/<tag>` in the release draft

## Publish the release

* finalise the release (publish)
* while doing the rest of the release process, pre-built binaries can be re-built to check for reproduciblity (check side notes about reproducibility below)
* add hashes in `install.sh` and `install.ps1` (and check signatures)
* update `OPAM_TEST_REPO_SHA` in `ci.ml` and update version in `main.sh` for dependencies job
* bring the changes to the changelog (CHANGES) from the branch of the release to the `master` branch
* Update doc/pages/Install.md
* publish opam packages in opam-repository (use `opam publish --pre-release` if this is not a stable version)
* update versions (and messages, if necessary) in https://github.com/ocaml/opam-repository/blob/master/repo

## Announce!

* publish the blog entry in opam.ocaml.org
* wait until the blog post is online
* publish the announcement in discuss.ocaml.org
* update the link to the discuss post in the blog post
* update the link to the blog post in the release note
* on stable releases, copy the blog entry from opam.ocaml.org for https://github.com/ocaml/ocaml.org/tree/main/data/changelog/releases/opam
* announce the release on the OCaml Discord server

## After release

* Remove the milestone that has just been released and create a new milestone for the next version
* Bump the version with a `~dev` at the end (e.g. `2.2.0~alpha~dev`)
* Check if reftests needs an update
* If this is a stable release, publish the release on distributions (including winget) we can easily contribute to ourselves or ping the package maintainers

---

## Device requirements
* Mac M1 or above with Rosetta2
* >=70GB of disk space free
* brew dependencies: git >= 2.40.0, git-lfs, gpg, qemu>=8.1.0 (avoid qemu 9.1.x, see https://gitlab.com/qemu-project/qemu/-/issues/2581), docker>=24.0.0, sshpass
* opam repo with the tag fetched
* Have the secret key available

## Side note on "reproducibility"

Linux, FreeBSD, OpenBSD, NetBSD and Windows binaries should be "reproducible". The `opam-full-*.tar.gz` archive is also reproducible.
macOS binaries are not (lack of sandbox, Mach-O's `LC_UUID` field will change for some reason even on the same machine).

At the moment, "reproduciblity" in the context of opam releases is a weak subset of "reproducible builds" as described in
https://reproducible-builds.org/docs/definition/

Our current definition is that the binaries are "reproducible" modulo:
- C compiler and tools version installed or upgraded during setup (e.g. apk add, cygwin's setup.exe)
- changes or updates to the base images (docker and qemu)

In practice, this means that there might be a timer on whether a binary can be reproduced
(maybe a few months, maybe much more depending on whether the package repository for the given system
at the particular version keeps the same version of the C compiler as was used during the original build).
This is mostly a worry for Windows rather than Linux and BSDs given the rolling-release nature of Cygwin.
