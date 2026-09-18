## v3.1.2 (2026-07-06)

* opatch: exit with 1 if a patch does not apply (#45 @hannesm)

## v3.1.1 (2026-05-05)

* Handle Edit when ---/+++ contradicts git Create/Delete metadata
  (#42 @Alizter, review by @kit-ty-kate)

## v3.1.0 (2025-11-06)

* Use a rope data structure instead of lists. Improves performance especially
  with many hunks (#37 @hannesm, review by @shym @kit-ty-kate)

## v3.0.1 (2025-10-15)

* Only a release of opatch (#35 @shym @kit-ty-kate)

## v3.0.0 (2025-07-03)

* No change since 3.0.0~beta1

## v3.0.0-beta1 (2025-06-02)

* Allow to use Patch.patch on large diffs with OCaml 4 (#30 @kit-ty-kate)
* Patch.pp\_hunk: Fix support for printing large diffs with OCaml 4 (#30 @kit-ty-kate)
* Fix Patch.diff generating malformed patch when adding a line at the end of a file that doesn't end with a newline character (#29 @kit-ty-kate)

## v3.0.0-alpha2 (2025-05-01)

* Add support for large (>= MB) diffs (#23 @kit-ty-kate)
* Fix printing of `Rename_only` (#23 @kit-ty-kate)
* Add support for empty files through git extensions (#26 @kit-ty-kate)
* Fix filename parsing of the git extensions (#26 @kit-ty-kate)
* Move the `Rename_only` variant off of the `operation` type to the dedicated
  new `git_ext` type containing the new variants `Delete_only` and
  `Create_only` (#26 @kit-ty-kate)
* Add a new `Git_ext` variant to the `operation` type (#26 @kit-ty-kate)
* Change the type of `Patch.diff` to avoid the need to know in advance
  what type of operation it is gonna be (#26 @kit-ty-kate)

## v3.0.0-alpha1 (2025-03-06)

* Ensure compatibility with GNU Patch as much as possible:
  * Fix the parsing of filenames coming from GNU or git diffs (#20 @kit-ty-kate @Leonidas-from-XIV)
  * Detect file creation/deletion when parsing patch files created with `diff -N` (#20 @kit-ty-kate)
  * Add a `~p` parameter to `Patch.parse` mimicking the behaviour of `patch -p<num>` (#9 @kit-ty-kate @hannesm)
  * Allow empty lines to be equivalent to a simple newline in both mine/their (#22 @kit-ty-kate)
  * Allow the tab character to be used in place of ` \t` (#22 @kit-ty-kate)
  * `Patch.apply`: allow unclean application using the default GNU Patch algorithm (#22 @kit-ty-kate)
  * Allow up to 3 assumed-empty lines missing at the end of each hunk (#22 @kit-ty-kate)
  * Handle git extensions only when in presence of a git header (#22 @kit-ty-kate)
  * Add support for the empty file deletion git extension (#22 @kit-ty-kate)
  * Add support for spaces instead of tabs between filename and date (#22 @kit-ty-kate)
  * Start the diff start index from 1 (0 if empty) (#22 @kit-ty-kate)
* Quote special characters from filename when pretty-printing them (#21 @kit-ty-kate)
* Refuse context diffs and only accept unified diffs (#22 @kit-ty-kate)
* `Patch.pp_hunk`: Add missing final end of line character (#22 @kit-ty-kate)
* `Patch.pp_operation`: Print the git header when using a git extension (#22 @kit-ty-kate)

## v2.0.0 (2024-04-03)

* Add support for git format-patch headers (#7 @kit-ty-kate)
* Pretty-printer: fix no_newline support (#11 @kit-ty-kate)
* Various fixes to the diff parser ('---' mid diff, hunks, no newline at end
  of file) (#10 @kit-ty-kate)
* Add Patch.pp_list (#13 @kit-ty-kate)
* Merge Edit and Rename operations (#14 @kit-ty-kate)
* Add a diff implementation (#12 @kit-ty-kate)
* Rename to_diffs to parse (#16 @kit-ty-kate)
* Provide API docs, tweak documentation (@hannesm)

## v1.0.1 (2022-10-27)

* Remove unnecessary bytes dependency
* Fix compilation of examples
* Use GitHub actions instead of travis

## v1.0.0 (2019-12-21)

* Initial public release
