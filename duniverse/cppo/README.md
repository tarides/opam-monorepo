[![Build status](https://github.com/ocaml-community/cppo/actions/workflows/build.yml/badge.svg)](https://github.com/ocaml-community/cppo/actions/workflows/build.yml)

Cppo: cpp for OCaml
===================

Cppo is an equivalent of the C preprocessor for OCaml programs.
It allows the definition of simple macros and file inclusion.

Cppo is:

* more OCaml-friendly than cpp
* easy to learn without consulting a manual
* reasonably fast
* simple to install and to maintain

Meta
----

* Author: Martin Jambon
* OCaml-community maintainers:
  - Martin Jambon ([**@mjambon**](https://github.com/mjambon))
  - Yishuai Li ([**@liyishuai**](https://github.com/liyishuai))
* License: [BSD 3-Clause "New" or "Revised" License](LICENSE.md)
* Compatible OCaml versions: 4.02.3 or later
* Additional dependencies:
  - [Dune](https://dune.build) 1.10 or later
  - [OCamlbuild](https://github.com/ocaml/ocamlbuild) and [Findlib](http://projects.camlcity.org/projects/findlib.html), for Ocamlbuild plugin

Building and installation instructions
--------------------------------------

The easiest way to install the latest released version of cppo
is via [OPAM](https://opam.ocaml.org/doc/Install.html):

```shell
opam install cppo
```

To instead build and install manually, do:

``` shell
git clone https://github.com/ocaml-community/cppo.git
cd cppo
make
make install
```

User guide
----------

Cppo is a preprocessor for programming languages that follow lexical rules
compatible with OCaml including OCaml-style comments `(* ... *)`. These include Ocamllex, Ocamlyacc, Menhir, and extensions of OCaml based on Camlp4, Camlp5, or ppx. Cppo should work with Bucklescript as well. It won't work so well with Reason code because Reason uses C-style comment delimiters `/*` and `*/`.

Cppo supports a number of directives. A directive is a `#` sign placed
at the beginning of a line, possibly preceded by some whitespace, and followed
by a valid directive name or by a number:

```ocaml
BLANK* "#" BLANK* ("def"|"enddef"|"define"|"undef"
                  |"scope"|"endscope"
                  |"if"|"ifdef"|"ifndef"|"else"|"elif"|"endif"
                  |"include"
                  |"warning"|"error"
                  |"ext"|"endext") ...
```

A macro definition that is delimited by `#def` and `#enddef` can span
several lines. There is no need for protecting line endings with
backslash characters `\`.

A directive (other than `#def ... #enddef`)
can be split into multiple lines by placing a backslash character `\` at
the end of the line to be continued. In general, any special character
can be used as a normal character by preceding it with backslash.


File inclusion
--------------

```ocaml
#include "hello.ml"
```

This is how a source file `hello.ml` can be included.
Relative paths are searched first in the directory of the current file
and then in the search paths added on the command line using `-I`, if any.


Macros
------

This is a simple macro that doesn't take an argument ("object-like
macro" in the cpp jargon):

```ocaml
#define Ms Mississippi

match state with
    Ms -> true
  | _ -> false
```

After preprocessing by cppo, the code above becomes:

```ocaml
match state with
    Mississippi -> true
  | _ -> false
```

If needed, defined macros can be undefined. This is required prior to
redefining a macro:

```ocaml
#undef X
```

An important distinction with cpp is that only previously-defined
macros are accessible. Defining, undefining or redefining a macro has
no effect on how previous macros will expand.

Macros can take arguments. That is, a macro can be parameterized;
this is known as a "function-like macro" in `cpp` jargon.
When a parameterized macro is defined
and when it is applied,
the opening parenthesis must stick to the macro's identifier:
that is, there must be no space in between.
For example, this text:

```ocaml
#define debug(args) if !debugging then Printf.eprintf args else ()

debug("Testing %i" (1 + 1))
```

is expanded into:

```ocaml
if !debugging then Printf.eprintf "Testing %i" (1 + 1) else ()
```

An ordinary macro, which takes no arguments, can be viewed as
a parameterized macro that takes zero arguments. However, the
syntax differs: when there is no argument, no parentheses are
used; when there is at least one argument, parentheses must be used.
Here is a summary of the valid syntaxes:

```ocaml
#define FOO 42      (* Definition of an ordinary macro *)
FOO                 (* A use of an ordinary macro *)

#define BAR() 42    (* Invalid! When parentheses are used,
                       there must be at least one parameter *)

#define BAR(x) 42+x (* Definition of a parameterized macro *)
BAR(0)              (* A use of this parameterized macro *)
BAR()               (* Another valid use -- the argument is empty *)
```

All user-definable macros are constant. There are however two
predefined variable macros: `__FILE__` and `__LINE__` which take the value
of the position in the source file where the macro is being expanded.

```ocaml
#define loc (Printf.sprintf "File %S, line %i" __FILE__ __LINE__)
```

Macros can be defined on the command line as follows:

```ocaml
# preprocessing only
cppo -D 'VERSION 1.0' example.ml

# preprocessing and compiling
ocamlopt -c -pp "cppo -D 'VERSION 1.0'" example.ml
```

Multi-line macros and nested macros
-----------------------------------

A macro definition that begins with `#define` can span several lines.
In that case, the end of each line must be protected with a backslash
character, as in this example:

```ocaml
#define repeat_until(action,condition) \
  action; \
  while not (condition) do \
    action \
  done
```

In other words, at the first line ending that is *not* preceded by a `\`
character, an `#enddefine` token is implicitly generated,
and the definition ends.

This convention, which is inherited from C, causes two problems. First,
protecting every line ending with a `\` character is painful. Second, more
seriously, this convention does not allow macro definitions to be nested.
Indeed, if one attempts to nest two definitions that begin with `#define`,
then only one `#enddefine` token is generated; it is generated at the first
unprotected line ending. So, the beginnings and ends of definitions cannot
be correctly balanced.

These problems are avoided by using an alternative syntax where the beginning
and end of a macro definition are explicitly marked by `#def` and `#enddef`.
Here is an example:

```ocaml
#def repeat_until(action,condition)
  action;
  while not (condition) do
    action
  done
#enddef
```

With this syntax, a macro can span several lines:
there is no need to protect line endings with `\` characters.
Furthermore, this syntax allows macro definitions to be nested:
inside a macro definition that is delimited by `#def` and `#enddef`,
both `#def` and `#define` can be used.

Higher-order macros
-------------------

A parameterized macro can take a parameterized macro as a parameter:
this is known as a higher-order macro.

To enable this feature, some annotations are required:
when a macro parameter is itself a parameterized macro,
it must be annotated with its type.

A macro takes *n* arguments (where *n* can be zero)
and returns a piece of text.
So, to describe the type of a macro, it suffices to
describe the types of its *n* arguments.

Thus, the syntax of types is
`τ ::= [τ ... τ]`.
That is, a type is a sequence of *n* types,
  without separators,
surrounded with square brackets.
An ordinary macro,
which takes zero parameters,
has type `[]`.
This is the base type: in other words, it is the type of text.
For greater readability,
this type can also be written in the form of a single period, `.`.
Here are a few examples of types:

```ocaml
  .       (* An ordinary unparameterized macro: in other words, text    *)
  []      (* Same as above.                                             *)
  [.]     (* A parameterized macro that expects one piece of text       *)
  [..]    (* A parameterized macro that expects two pieces of text      *)
  [[.].]  (* A parameterized macro
             whose first parameter is a parameterized macro of type [.]
             and whose second parameter is a piece of text              *)
```

In the definition of a parameterized macro `M`,
each parameter `X` can be annotated with a type
by writing `X : τ`.
This is optional: if no annotation is provided,
the base type `.` is assumed.
If a parameter `X` is annotated with a type `τ` other than the base type,
then, when the parameterized macro `M` is applied,
the actual argument `Y` that is supplied as an instance for `X`
must be the name of a macro of type `τ`.

This is more easily explained via an example. In the following code,

```ocaml
#define TWICE(e)          (e + e)
#define APPLY(F : [.], e) (let x = (e) in F(x))
let forty_two =
  APPLY(TWICE,1+2+3+4+5+6)
```

`TWICE` is a parameterized macro of type `[.]`, and
`APPLY` is a higher-order macro, whose type is `[[.].]`.
Thus, the application `APPLY(TWICE, ...)` is valid.
This code is expanded into:

```ocaml
let forty_two =
   (let x = (1+2+3+4+5+6) in (x + x))
```

Scopes
------
When a block of text is delimited by `#scope ... #endscope`,
all macro definitions (`#define`, `#def ... #enddef`)
and undefinitions (`#undef`)
become local:
they take effect only within this block.

```ocaml
(* Here, assume that the macro FOO is not defined. *)
#scope
#define FOO "FOO is now defined"
let x = FOO (* FOO expands to "FOO is now defined" *)
#endscope
(* Here, the macro FOO is again not defined. *)
#define FOO 42
let y = FOO (* FOO expands to 42 *)
```

Scopes can be nested,
as illustrated by this example:

```ocaml
#scope
  #define HELLO "Hello, "
  #scope
    #define MAN "man"
    let message1 = HELLO ^ MAN
  #endscope
  (* Here, MAN is no longer defined, but HELLO still is. *)
  let message2 = HELLO ^ "world"
#endscope
```

Conditionals
------------

Here is a quick reference on conditionals available in cppo. If you
are not familiar with `#ifdef`, `#ifndef`, `#if`, `#else` and `#elif`, please
refer to the corresponding section in the cpp manual.

```ocaml
#ifndef VERSION
#warning "VERSION is undefined"
#define VERSION "n/a"
#endif
#ifndef VERSION
#error "VERSION is undefined"
#endif
#if OCAML_MAJOR >= 3 && OCAML_MINOR >= 10
...
#endif
#ifdef X
...
#elif defined Y
...
#else
...
#endif
```

The boolean expressions following `#if` and `#elif` may perform arithmetic
operations and tests over 64-bit ints.

Boolean expressions:

* `defined` ...  followed by an identifier, returns true if such a macro exists
* `true`
* `false`
* `(` ... `)`
* ... `&&` ...
* ... `||` ...
* `not` ...

Arithmetic comparisons used in boolean expressions:

* ... `=` ...
* ... `<` ...
* ... `>` ...
* ... `<>` ...
* ... `<=` ...
* ... `>=` ...

Arithmetic operators over signed 64-bit ints:

* `(` ... `)`
* ... `+` ...
* ... `-` ...
* ... `*` ...
* ... `/` ...
* ... `mod` ...
* ... `lsl` ...
* ... `lsr` ...
* ... `asr` ...
* ... `land` ...
* ... `lor` ...
* ... `lxor` ...
* `lnot` ...

Macro identifiers can be used in place of ints as long as they expand
to an int literal or a tuple of int literals, e.g.:

```ocaml
#define one 1

#if one + one <> 2
#error "Something's wrong."
#endif

#define VERSION (1, 0, 5)
#if VERSION <= (1, 0, 2)
#error "Version 1.0.2 or greater is required."
#endif
```

Version strings (http://semver.org/) can also be passed to cppo on the
command line. This results in multiple variables being defined, all
sharing the same prefix. See the output of `cppo -help` (copied at the
bottom of this page).

```
$ cppo -V OCAML:`ocamlc -version`
#if OCAML_VERSION >= (4, 0, 0)
(* All is well. *)
#else
  #error "This version of OCaml is not supported."
#endif
```

Output:
```
# 2 "<stdin>"
(* All is well. *)
```

Source file location
--------------------

Location directives are the same as in OCaml and are echoed in the
output. They consist of a line number optionally followed by a file name:

```ocaml
# 123
# 456 "source"
```

Messages
--------

Warnings and error messages can be produced by the preprocessor:

```ocaml
#ifndef X
  #warning "Assuming default value for X"
  #define X 1
#elif X = 0
  #error "X may not be null"
#endif
```

Calling an external processor
-----------------------------

Cppo provides a mechanism for converting sections of a file using
and external program. Such a section must be placed between `#ext` and
`#endext` directives.

```bash
$ cat foo
ABC
#ext lowercase
DEF
#endext
GHI
#ext lowercase
KLM
NOP
#endext
QRS

$ cppo -x lowercase:'tr "[A-Z]" "[a-z]"' foo
# 1 "foo"
ABC
def
# 5 "foo"
GHI
klm
nop
# 10 "foo"
QRS
```

In the example above, `lowercase` is the name given on the
command-line to external command `'tr "[A-Z]" "[a-z]"'` that reads
input from stdin and writes its output to stdout.


Escaping
--------

The following characters can be escaped by a backslash when needed:

```ocaml
(
)
,
#
```

In OCaml `#` is used for method calls. It is usually not a problem
because in order to be interpreted as a preprocessor directive, it
must be the first non-blank character of a line and be a known
directive. If an object has a define method and you want `#` to appear
first on a line, you would have to use `\#` instead:

```ocaml
obj
  \#define
```

Line directives in the usual format supported by OCaml are correctly
interpreted by cppo.

Comments and string literals constitute single tokens even when they
span across multiple lines. Therefore newlines within string literals
and comments should remain as-is (no preceding backslash) even in a
macro body:

```ocaml
#define welcome \
"**********
*Welcome!*
**********
"
```

Concatenation
-------------

`CONCAT()` is a predefined macro that takes two arguments, removes any
whitespace between and around them and fuses them into a single identifier.
The result of the concatenation must be a valid identifier of the
form [A-Za-z_][A-Za-z0-9_]+ or [A-Za-z], or empty.

For example,

```ocaml
#define x 123
CONCAT(z, x)
```

expands into:

```ocaml
z123
```

However the following is illegal:

```ocaml
#define x 123
CONCAT(x, z)
```

because 123z does not form a valid identifier.

`CONCAT(a,b)` is roughly equivalent to `a##b` in cpp syntax.

CAPITALIZE
---------------

`CAPITALIZE()` is a predefined macro that takes one argument,
removes any leading and trailing whitespace, reduces each internal
whitespace sequence to a single space character and produces
a valid OCaml identifer with first character.

For example,
```ocaml
#define EVENT(n,ty) external CONCAT(on,CAPITALIZE(n)) : ty = STRINGIFY(n) [@@bs.val]
EVENT(exit, unit -> unit)
```
is expanded into:

```ocaml
external  onExit  :  unit -> unit = "exit" [@@bs.val]
```

Stringification
---------------

`STRINGIFY()` is a predefined macro that takes one argument,
removes any leading and trailing whitespace, reduces each internal
whitespace sequence to a single space character and produces
a valid OCaml string literal.

For example,

```ocaml
#define TRACE(f) Printf.printf ">>> %s\n" STRINGIFY(f); f
TRACE(print_endline) "Hello"
```

is expanded into:

```ocaml
Printf.printf ">>> %s\n" "print_endline"; print_endline "Hello"
```

`STRINGIFY(x)` is the equivalent of `#x` in cpp syntax.


Ocamlbuild plugin
------------------

An ocamlbuild plugin is available. To use it, you can call ocamlbuild
with the argument `-plugin-tag package(cppo_ocamlbuild)` (only since
ocaml 4.01 and cppo >= 0.9.4).

Starting from **cppo >= 1.6.0**, the `cppo_ocamlbuild` plugin is in a
separate OPAM package (`opam install cppo_ocamlbuild`).

With Oasis :
```
OCamlVersion: >= 4.01
AlphaFeatures: ocamlbuild_more_args
XOCamlbuildPluginTags: package(cppo_ocamlbuild)
```

After that, you need to add in your `myocamlbuild.ml` :
```ocaml
let () =
  Ocamlbuild_plugin.dispatch
    (fun hook ->
      Ocamlbuild_cppo.dispatcher hook ;
    )
```

By default the plugin will apply cppo on all files ending in `.cppo.ml`
`cppo.mli`, and `cppo.mlpack`, in order to produce `.ml`, `.mli`,
and`.mlpack` files.  The following tags are available:
* `cppo_D(X)` ≡ `-D X`
* `cppo_U(X)` ≡ `-U X`
* `cppo_q` ≡ `-q`
* `cppo_s` ≡ `-s`
* `cppo_n` ≡ `-n`
* `cppo_x(NAME:CMD_TEMPLATE)` ≡ `-x NAME:CMD_TEMPLATE`
* The tag `cppo_I(foo)` can behave in two way:
  * If `foo` is a directory, it's equivalent to `-I foo`.
  * If `foo` is a file, it adds `foo` as a dependency and apply `-I
    parent(foo)`.
* `cppo_V(NAME:VERSION)` ≡ `-V NAME:VERSION`
* `cppo_V_OCAML` ≡ `-V OCAML:VERSION`, where `VERSION`
   is the version of OCaml that ocamlbuild uses.

Balancing delimiters
--------------------

All delimiters,
including scope delimiters (`#scope` and `#endscope`),
delimiters of macro definitions (`#def` and `#enddef`),
and delimiters of conditional constructs (`#if`, `#endif`, etc.),
must be used in a well-balanced manner.

This requirement does *not* apply separately to each category of delimiters.
instead, it applies to all categories of delimiters at once.
This is a stricter requirement.
Thus, for example, `#scope` cannot be followed with `#endif`,
and `#if` cannot be followed with `#endscope`.
In other words,
a scope cannot contain a fragment of a conditional construct,
and a conditional construct cannot contain a fragment of a macro definition.


Detailed command-line usage and options
---------------------------------------

```
Usage: ./cppo [OPTIONS] [FILE1 [FILE2 ...]]
Options:
  -D DEF
          Equivalent of interpreting '#define DEF' before processing the
          input
  -U IDENT
          Equivalent of interpreting '#undef IDENT' before processing the
          input
  -I DIR
          Add directory DIR to the search path for included files
  -V VAR:MAJOR.MINOR.PATCH-OPTPRERELEASE+OPTBUILD
          Define the following variables extracted from a version string
          (following the Semantic Versioning syntax http://semver.org/):

            VAR_MAJOR           must be a non-negative int
            VAR_MINOR           must be a non-negative int
            VAR_PATCH           must be a non-negative int
            VAR_PRERELEASE      if the OPTPRERELEASE part exists
            VAR_BUILD           if the OPTBUILD part exists
            VAR_VERSION         is the tuple (MAJOR, MINOR, PATCH)
            VAR_VERSION_STRING  is the string MAJOR.MINOR.PATCH
            VAR_VERSION_FULL    is the original string

          Example: cppo -V OCAML:4.02.1

  -o FILE
          Output file
  -q
          Identify and preserve camlp4 quotations
  -s
          Output line directives pointing to the exact source location of
          each token, including those coming from the body of macro
          definitions.  This behavior is off by default.
  -n
          Do not output any line directive other than those found in the
          input (overrides -s).
  -version
          Print the version of the program and exit.
  -x NAME:CMD_TEMPLATE
          Define a custom preprocessor target section starting with:
            #ext "NAME"
          and ending with:
            #endext

          NAME must be a lowercase identifier of the form [a-z][A-Za-z0-9_]*

          CMD_TEMPLATE is a command template supporting the following
          special sequences:
            %F  file name (unescaped; beware of potential scripting attacks)
            %B  number of the first line
            %E  number of the last line
            %%  a single percent sign

          Filename, first line number and last line number are also
          available from the following environment variables:
          CPPO_FILE, CPPO_FIRST_LINE, CPPO_LAST_LINE.

          The command produced is expected to read the data lines from stdin
          and to write its output to stdout.
  -help  Display this list of options
  --help  Display this list of options
```


Contributing
------------

See our contribution guidelines at
https://github.com/mjambon/documents/blob/master/how-to-contribute.md
