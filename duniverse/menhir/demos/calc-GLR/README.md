This is a variant of the demo `calc`, compiled in GLR mode.

The grammar does not contain precedence declarations; as a result, it is
highly ambiguous. This is NOT recommended practice. The parsing time is O(n^3)
where n is the length of of the input, or more precisely, the length of the
longest ambiguous input fragment.
