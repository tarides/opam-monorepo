Optint - Efficient integer types on 64-bit architectures
========================================================

This library provides two new integer types, `Optint.t` and `Int63.t`, which
guarantee efficient representation on 64-bit architectures and provide a
best-effort boxed representation on 32-bit architectures.

## Goal

The standard `Int32.t` and `Int64.t` types provided by the standard library have
the same heap-allocated representation on all architectures. This consistent
representation has costs in both memory and run-time performance.

On 64-bit architectures, it's often more efficient to use the native `int`
directly.
This library provides types to do exactly this: 

- `Optint.t`: an integer containing _at least_ 32 bits. On 64-bit, this is an
  immediate integer; on 32-bit, it is a boxed 32-bit value. The overflow
  behaviour is platform-dependent.

- `Int63.t`: an integer containing _exactly_ 63 bits. On 64-bit, this is an
  immediate integer; on 32-bit, it is a boxed 64-bit integer that is wrapped to
  provide 63-bit two's complement semantics. The two implementations are
  observationally equivalent, modulo use of `Marshal` and `Obj`.

In summary:

| Integer type         | 32-bit representation  | 64-bit representation  | Semantics          |
| --                   | --                  | --                  | --                 |
| `Stdlib.Int.t`       | 31-bit immediate ✅ | 63-bit immediate ✅ | Always immediate   |
| `Stdlib.Nativeint.t` | 32-bit boxed ❌     | 64-bit boxed ❌     | Exactly word size  |
| `Stdlib.Int32.t`     | 32-bit boxed ❌     | 32-bit boxed ❌     | Exactly 32 bits    |
| `Stdlib.Int64.t`     | 64-bit boxed ❌     | 64-bit boxed ❌     | Exactly 64 bits    |
| `Optint.t` (_new_)   | 32-bit boxed ❌     | 63-bit immediate ✅ | _At least_ 32 bits |
| `Int63.t` (_new_)    | 64-bit boxed ❌     | 63-bit immediate ✅ | Exactly 63 bits    |

These new types are safe and well-tested, but their architecture-dependent
implementation makes them unsuitable for use with the `Marshal` module. Use the
provided encode and decode functions instead.
