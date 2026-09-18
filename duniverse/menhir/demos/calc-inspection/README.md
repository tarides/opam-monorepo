This variant of the calc demo uses Menhir with the `--table` and
`--inspection` options. When a syntax error takes place, the stack is
inspected and a syntax error message is automatically constructed. This
primitive way of constructing syntax error messages is *not* recommended;
please use LRgrep instead.
