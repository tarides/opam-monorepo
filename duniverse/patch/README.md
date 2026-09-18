## Patch - apply your unified diffs in pure OCaml

The loosely specified `diff` file format is widely used for transmitting
differences of line-based information. The motivating example is
[`opam`](https://opam.ocaml.org), which is able to validate updates being
cryptographically signed (e.g. [conex](https://github.com/robur-coop/conex)) by
providing a unified diff.

The [test-based infered specification](https://www.artima.com/weblogs/viewpost.jsp?thread=164293)
implemented in this library is the following grammar.

```
decimal := [0-9]+
any := any character except newline

filename := "/dev/null" | any except tab character
file := filename "\t" any "\n"
mine := "--- " file
theirs := "+++ " file

no_newline = "\ No newline at end of file"
hunk_line_prefix := " " | "-" | "+"
hunk_line := hunk_line_prefix any | no_newline
range := decimal "," decimal | decimal
hunk_hdr := "@@ -" range " + " range " @@\n"
hunk := hunk_hdr line+

diff := mine theirs hunk+
```

In addition, some support for the git diff format is available, which contains
`diff --git a/nn b/nn` as separator, prefixes filenames with `a/` and `b/`, and
may contain extra headers, especially for pure renaming: `rename from <path>`
followed by `rename to <path>`. The git diff documentation also mentions that a
diff file itself should be an atomic operation, thus all `-` files corrspond to
the files before applying the diff (since `patch` only does single diff
operations, and requires the old content as input). You have to ensure to
provide the correct data yourself.

A `diff` consists of a two-line header containing the filenames (or "/dev/null"
for creation and deletion) followed by the actual changes in hunks. A complete
diff file is represented by a list of `diff` elements. The OCaml types below,
provided by this library, represent mine and theirs as operation (edit, delete,
create). Since a diff is line-based, if the file does not end with a newline
character, the line in the diff always contains a newline, but the special
marker `no_newline` is added to the diff. The `range` information carries start
line and chunk size in the respective file, with two side conditions: if the
chunk size is 0, the start line refers to after which the chunk should be added
or deleted, and if the chunk size is omitted (including the comma), it is set
to 1. NB from practical experiments, only "+1" and "-1" are supported.

```OCaml
type git_ext =
  | Rename_only of string * string
  | Delete_only
  | Create_only

type operation =
  | Edit of string * string
  | Delete of string
  | Create of string
  | Git_ext of (string * string * git_ext)

type hunk (* positions and contents *)

type t = {
  operation : operation ;
  hunks : hunk list ;
  mine_no_nl : bool ;
  their_no_nl : bool ;
}
```

In addition to parsing a diff and applying it, support for generating a diff
from old and new file contents is also provided.

## Shortcomings

The function `patch` assumes that the patch applies cleanly, and does not
check this assumption. Exceptions may be raised if this assumption is violated.
The git diff format allows further features, such as file permissions.

## Installation

`opam install patch`

## Documentation

The API documentation can be browsed [online](https://hannesm.github.io/patch/).

## Testsuite

The testsuite can be ran with a simple `dune test`, however note that to also
test larger files, you must first make sure that the submodule is up-to-date:
```
git submodule update --init
```
