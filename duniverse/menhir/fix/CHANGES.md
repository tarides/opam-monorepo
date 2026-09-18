# CHANGES

## 2025/MM/DD

* New signatures `NUMBERING_OPERATIONS` and `NUMBERED`.

* New functor `Fix.Fix.ForIntSegment`.

* New functors
  `Fix.Fix.ForNumberedType`,
  `Fix.DataFlow.ForNumberedType`,
  `Fix.GraphNumbering.ForNumberedType`,
  `Fix.Memoize.ForNumberedType`,
  `Fix.Numbering.ForNumberedType`.

* New functors
  `Fix.Numbering.Operations` and
  `Fix.Numbering.OperationsForIntSegment`.

* New functor `Fix.Glue.ArraysAsImperativeMapsWithNumbering`.

## 2025/09/19

* The new submodule `Fix.SCC` provides an implementation of Tarjan's
  linear-time strongly-connected-component discovery algorithm.

## 2025/04/28

* New functor `Fix.Tabulate.ForNumberedType`.

* In the signature `DATA_FLOW_GRAPH`, document the fact that the transfer
  function `foreach_successor` must transform the property `⊥` at a source
  node to the property `⊥` at each destination node. The least fixed point
  computation would otherwise be incorrect.

  (In reality, only `Fix.DataFlow.ForCustomMaps` actually exploits this
  assumption; the other functors in `Fix.DataFlow` do not. For the sake
  of caution and uniformity, we prefer to document that every functor
  makes this assumption.)

## 2023/05/05

* New modules `Fix.Partition` and `Fix.Minimize`.
  (Contributed by Frédéric Bour; reviewed by François Pottier.)

* New module `Fix.Enum`. The API of this module is not stable yet and is
  likely to change in the future. This could have indirect impact on
  users of `Fix.Minimize`, whose API involves the type `enum`.

* New functionality and minor changes in `Fix.Indexing`.
  (Contributed by Frédéric Bour; reviewed by François Pottier.)

  + New functions `is_fixed`, `equal`, `assert_equal`.

  + New function `rev_iter`.

  + When faced with an invalid argument, `of_int` now raises
    `Invalid_argument` instead of `Assert_failure`.

* New functionality and minor changes in `Fix.Indexing.Vector`.
  (Contributed by Frédéric Bour; reviewed by François Pottier.)

  + The type `('n, 'a) vector` is no longer a private type: it is now an
    abstract type. Thus, it can no longer be coerced to `'a array`. Instead,
    the new function `as_array` is provided for this purpose.

  + In the reverse direction, the new function `of_array` converts an array to
    a vector, with a dynamic check, while the new functor `Of_array` also
    converts an array to a vector, without a dynamic check. `of_array` is used
    when the length of the array already has received a type-level name, while
    `Of_array` is used when one wishes to create a fresh type-level name to
    stand for this length.

  + New functions `mapi`, `copy`, `iter`, `iteri`, `iter2`,
    `fold_left`, `fold_right`, `to_list`, `sort`.
    These functions are aliases for the functions by the same names
    in the module `Array`.

  + New functions `fold_left2` and `fold_right2`.

  + New function `invert`.

* Restrict the type of `Fix.Indexing.Vector.empty`, which was too general
  and would allow out-of-bounds accesses into an empty vector.
  (Contributed by Frédéric Bour.)

## 2022/01/21

* The functor `Memoize.Make` now requires just `MINIMAL_IMPERATIVE_MAPS`
  instead of `IMPERATIVE_MAPS`, which was needlessly strong.

* The functor `Glue.WeakHashTablesAsImperativeMaps` now provides only
  `MINIMAL_IMPERATIVE_MAPS` instead of `IMPERATIVE_MAPS`. This change
  is made for compatibility with the new ephemeron API in OCaml 5.

## 2021/12/31

* Improved documentation.

## 2021/11/25

* The new module `CompactQueue` offers a minimalist mutable FIFO queue. It is
  comparable with OCaml's `Queue` module. In comparison with `Queue`, it uses
  a more compact internal representation: elements are stored contiguously in
  a circular array. This has a positive impact on performance: both time and
  memory consumption are reduced. This data structure is optimized for maximum
  throughput. (Contributed by Frédéric Bour, reviewed by François Pottier.)

* The new functor `DataFlow.ForCustomMaps` offers a forward data flow analysis
  that is tuned for greater performance. (Contributed by Frédéric Bour,
  reviewed by François Pottier.)

* The new module `Indexing` offers a safe API for manipulating indices into
  fixed-size arrays. This API involves some dynamic checks as well as static
  type checks, thereby (hopefully) greatly reducing the risk of confusion in
  code that uses many arrays and many indices into these arrays. (Contributed
  by Frédéric Bour, reviewed by François Pottier.)

* In `DataFlow`, allow the function `foreach_root`
  (which is part of the signature `DATA_FLOW_GRAPH`)
  to call `contribute x _` several times at a single root `x`.

## 2020/11/20

* New module `DataFlow`, which performs a forward data flow analysis over a
  directed graph. (Such a computation could previously be performed by using
  the generic solver `Fix.Make`, but it was somewhat awkward to write, as it
  required access to predecessors. The new algorithm is easier to use and is
  more efficient.)

* In `Memoize`, new combinator `curried`, which can be used in combination
  with `fix` or `defensive_fix`. Thus, for instance, `curried fix` is a
  fixed point combinator that constructs a memoizing two-argument curried
  function.

## 2020/01/31

* In `Gensym`, new abstract type `generator`,
  with three functions `generator`, `fresh`, and `current`.

* In `Memoize`, new function `visibly_memoize`,
  which not only returns a memoized function,
  but also provides outside access to the memoization table.

* New signatures `ONGOING_NUMBERING` and `TWO_PHASE_NUMBERING`
  and new module `Numbering`,
  which provides facilities for numbering things.

* Breaking change: the module `Fix.Number`
  is renamed `Fix.GraphNumbering`.

## 2018/12/06

* New release, including new modules (`Gensym`, `Memoize`,
  `Tabulate`, `Number`, `HashCons`, `Prop`, `Glue`),
  new convenience functors (`Fix.ForHashedType`, etc.),
  and new demos.
  The least-fixed-point computation algorithm is unchanged.

## 2013/06/11

* Initial release of the package,
  containing just `Fix.Make`, the
  least-fixed-point computation algorithm.
