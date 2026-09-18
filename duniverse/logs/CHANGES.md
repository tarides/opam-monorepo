

* Make log mutex immune to raising logging functions.
  Thanks to Nathan Taylor for the report and the repro (#57).

v0.9.0 2025-07-08 Zagreb
------------------------

* Replace references and mutable fields by atomic references to avoid
  race conditions (#56). Thanks to Nathan Taylor for reporting.
* Fix `Logs.{err,warn}_count`. The counts were counting the reports
  not the logs which is not what the spec says. This means the counts
  were wrong when the reporting level was below the corresponding
  level (#55). Thanks to Mathieu Barbin for the report.
* Fix `Log.Tag.list` always returning the empty list.
* `Logs.format_reporter` and `Logs_fmt.reporter` replace a few format 
  strings and `^^` uses by direct calls to `Format` primitives.
* Requires OCaml >= 4.14.
* Use Format.pp_print_text instead of our own.
* Export `logs` from each sub library.

v0.8.0 2025-03-10 La Forclaz (VS)
---------------------------------

* Install one library per directory (#48). Thanks to @mefyl
  for the suggestion.
* Requires OCaml >= 4.08, Cmdliner >= 1.3.0, Fmt >= 0.9.0
  and js_of_ocaml-compiler >= 5.5.0
* Depend on the `js_of_ocaml-compiler.runtime` library rather than 
  `js_of_ocaml`.
* Handle `cmdliner` deprecations.

v0.7.0 2019-08-09 Zagreb
------------------------

Support for thread safe logging, thanks to Jules Aguillon for the
work.

* Add `Logs.set_reporter_mutex` for installing mutual exclusion
  primitives to access the reporter.
* Add `Logs_threaded.enable` to install mutual exclusion
  primitives for OCaml threads.

v0.6.3 2019-04-19 La Forclaz (VS)
---------------------------------

* Make the package compatible with `js_of_ocaml` 3.3.0's
  namespacing. Thanks to Hugo Heuzard for the patch.
* Fix toplevel initialisation for `Omod` (#21).
* Fix 4.08 `Pervasives` deprecation.
* Drop support for ocaml < 4.03.0
* Doc: various improvements and typo fixing.

v0.6.2 2016-08-10 Zagreb
------------------------

* 4.04.0 compatibility. Thanks to Damien Doligez for the patch.


v0.6.1 2016-06-08 Cambridge (UK)
--------------------------------

* Fix logs.top package on case sensitive file systems.

v0.6.0 2016-05-23 La Forclaz (VS)
---------------------------------

* Build depend on topkg.
* Relicensed from BSD3 to ISC.
* Revise the command line interface provided by `Logs_cli`. Removes
  the argument from option `-v`. See issue #13 for details.
* Add `Logs.format_reporter` a reporter like `Logs_fmt.reporter`
  but without colors and hence without the dependency on `Fmt`.
  Thanks to Simon Cruanes for the suggestion.
* `Logs_fmt.reporter`, the optional argument `prefix` is changed to
  `pp_header` and becomes a formatter. The default prefix now favors
  the basename of `Sys.argv.(0)` if it exists over
  `Sys.executable_name`; this gives better results for interpreted
  programs.
* Fix colors in `Logs_fmt.pp_header`, only `Logs.err_style` was
  being used.
* Add `Logs.level_{of,to}_string`.


v0.5.0 2016-01-07 La Forclaz (VS)
---------------------------------

* Support for OCaml 4.01.0
* Change the logging structure from `Logs.err fmt (fun m -> m ...)`
  to `Logs.err (fun m -> m fmt ...)`. See the documentation basics
  for more details. Thanks to Edwin Török for suggesting this.
* Remove the `Logs.unit[_msgf]` functions, they are no longer needed.
* Rename the `Logs_stdo` library to `Logs_fmt`.
* Changes the signature of reporters to take a callback function to
  call unconditionally once the report is over. Thanks to Edwin Török
  for suggesting the mecanism.
* Add the optional `Logs_lwt` library. Provides logging functions
  returning `lwt` threads that proceed only once the report is over.
* Add `Logs_fmt.pp_header` and `Logs_fmt.{err_warn,info_debug}_style`.
* Add `Logs.pp_{level,header}`.


v0.4.2 2015-12-03 Cambridge (UK)
--------------------------------

First release.
