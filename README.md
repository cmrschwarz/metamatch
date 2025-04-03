# `metamatch!`

[![github]](https://github.com/cmrschwarz/metamatch)&ensp;
[![github-build]](https://github.com/cmrschwarz/metamatch/actions/workflows/ci.yml)&ensp;
[![crates-io]](https://crates.io/crates/metamatch)&ensp;
[![msrv]](https://crates.io/crates/metamatch)&ensp;
[![docs-rs]](https://docs.rs/metamatch)&ensp;

[github]: https://img.shields.io/badge/cmrschwarz/metamatch-8da0cb?labelColor=555555&logo=github
[github-build]: https://img.shields.io/github/actions/workflow/status/cmrschwarz/metamatch/ci.yml?branch=main&logo=github
[crates-io]: https://img.shields.io/crates/v/metamatch.svg?logo=rust
[msrv]: https://img.shields.io/crates/msrv/metamatch?logo=rust
[docs-rs]: https://img.shields.io/badge/docs.rs-metamatch-66c2a5?logo=docs.rs

A zero dependency proc-macro for generating repetitive `match` arms.

Match arms for differently typed variants usually cannot be combined,
even if the are syntactically identical.
This macro implements a simple templating attribute (`#[expand]`)
to automatically stamp out the neccessary copies.

The macro syntax was carefully chosen to ensure maximum compatability
with `rustfmt`. It will correctly format the macro body as if it was
a regular `match` expression. Rust-analyzer also works correctly within this macro.
Even auto refactorings that affect the `#[expand]` (like changing the
name of an enum variant) work correctly.

`metamatch!` has since gained a few additional features that make it useful for
a wider range of token manipulation tasks. See the `replicate!` and `quote!`
variants below.

## [`metamatch!`](https://docs.rs/metamatch/latest/metamatch/macro.metamatch.html)

A proc-macro for generating repetitive match arms.

```rust
use metamatch::metamatch;
enum VarIntVec {
    I8(Vec<i8>),
    I16(Vec<i16>),
    I32(Vec<i32>),
    I64(Vec<i64>),
}

impl VarIntVec{
    fn len(&self) -> usize {
        metamatch!(match self {
            #[expand(for X in [I8, I16, I32, I64])]
            VarIntVec::X(v) => v.len(),
        })
    }
    // v   expands into   v
    fn len_expanded(&self) -> usize {
        match self {
            VarIntVec::I8(v) => v.len(),
            VarIntVec::I16(v) => v.len(),
            VarIntVec::I32(v) => v.len(),
            VarIntVec::I64(v) => v.len(),
        }
    }
}
```

Note that `#[expand(..)]` is not an actual attribute,
as Rust does not allow attributes on match arms.
`rustfmt` does though, and that's the trick behind this syntax.
The `metamatch!` proc macro simply replaces
`#[expand(..)]` with the generated match arms.

## [`#[replicate]`](https://docs.rs/metamatch/latest/metamatch/attr.replicate.html)
An attribute styled proc-macro with the same syntax as `#[expand]`.
This still works with `rustfmt`.

```rust
use metamatch::replicate;

struct NodeIdx(usize);

#[replicate(for (TRAIT, FN) in [
    (Add, add),
    (Sub, sub),
    (Mul, mul),
    (Div, div),
])]
impl core::ops::TRAIT for NodeIdx {
    type Output = Self;
    fn FN(self, other: Self) -> Self {
        NodeIdx(self.0.FN(other.0))
    }
}

```

## [`quote!`](https://docs.rs/metamatch/latest/metamatch/macro.quote.html)
A generalized version for arbitrary expressions.
This version gives up on `rustfmt` in exchange for more expressive power.

```rust
use metamatch::quote;

const ARRAY: [i32; 4] = quote!{
    [
        [<for X in 1..5>]
        X,
        [</for>]
    ]
};
assert_eq!(ARRAY, [1, 2, 3, 4]);
```

It uses `[< ... >]` styled template tags inspired by the `paste` crate.
These tags also work within regular `metatmatch` blocks.

## [`unquote!`](https://docs.rs/metamatch/latest/metamatch/macro.quote.html)
Just like `quote!`, but starts out in unquoted mode.
Here's the same example as above expressed using `unquote!`:

```rust
use metamatch::unquote;

const ARRAY: [i32; 4] = unquote! {
    let ELEMENTS = for X in 1..5 {
        quote!(X,)
    }:
    quote!([ELEMENTS])
};
assert_eq!(ARRAY, [1, 2, 3, 4]);
```
You can switch between quoted and unquoted mode from within any macro using
the `[<quote>]` and `[<unquote>]` template tags.
The `quote!(..)` used above is a covenience alias for `[<quote>]..[</quote>]`

The syntax inside the `unquote!` is not full Rust, but a tiny subset
thats evaluated by metamatch. It is dynamically typed and only supports a
few constructs, so no traits etc. inside an `unquote`.
(You can of course use whatever you want inside the quoted context).

All other macros also allow this syntax, despite `for` blocks
being the main usecase in those.


## License
[MIT](./LICENSE-MIT) or [Apache Version 2.0](./LICENSE-APACHE), at your option.
