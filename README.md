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

A zero dependency proc-macro for practical metaprogramming.

Because macros should be simple and readable.

## [`#[replicate]`](https://docs.rs/metamatch/latest/metamatch/attr.replicate.html)
Easily generate repetitive syntax like trait impls.

```rust
use metamatch::replicate;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct NodeIdx(usize);

#[replicate(
    for (TRAIT, FUNC) in [Add, Sub, Mul, Div, Rem].map(|t| (t, t.lowercase()))
)]
impl core::ops::TRAIT for NodeIdx {
    type Output = Self;
    fn FUNC(self, rhs: Self) -> Self {
        NodeIdx(self.0.FUNC(rhs.0))
    }
}
assert!(NodeIdx(1) + NodeIdx(2) == NodeIdx(3));
```

## [`metamatch!`](https://docs.rs/metamatch/latest/metamatch/macro.metamatch.html)
The original motivation and namesake of this crate.

Match arms for differently typed variants cannot be combined, even if the are
syntactically identical. This macro lets you stamp out the neccessary
copies using (`#[expand]`).

The syntax was carefully chosen to ensure compatability with `rustfmt`.
It will correctly format the macro body as if it was a regular `match` expression.
Rust-analyzer also works correctly within this macro,
including auto refactorings that affect the `#[expand]`.
(The same is also true for `#[replicate]`).

```rust
use metamatch::metamatch;
enum DynVec {
    I8(Vec<i8>),
    I16(Vec<i16>),
    I32(Vec<i32>),
    I64(Vec<i64>),
}

impl DynVec{
    fn len(&self) -> usize {
        metamatch!(match self {
            #[expand(for X in [I8, I16, I32, I64])]
            DynVec::X(v) => v.len(),
        })
    }
    // v   expands into   v
    fn len_after_expansion(&self) -> usize {
        match self {
            DynVec::I8(v) => v.len(),
            DynVec::I16(v) => v.len(),
            DynVec::I32(v) => v.len(),
            DynVec::I64(v) => v.len(),
        }
    }
}
```
<sub>
Note: <code>#[expand(..)]</code> is not an 'actual' Rust attribute,
as Rust does not allow attributes on match arms.
<code>rustfmt</code> does though, and that's the trick behind this syntax.
</sub>

## [`unquote!`](https://docs.rs/metamatch/latest/metamatch/macro.quote.html)
A generalized expression evaluator.

```rust
use metamatch::unquote;

const ARRAY: [i32; 4] = unquote! {
    let ELEMENTS = for X in 1..5 {
        quote!(X,)
    };
    quote!([ELEMENTS])
};
assert_eq!(ARRAY, [1, 2, 3, 4]);
```

<sub>
Note: you may have noticed the use of uppercase variables in these examples.
That's because by default, metamatch will not replace identifiers
in your quoted sections that happen to also be defined in your metaprogram.
Because there's no shadowing in the templates (they're just tokens),
this could lead to really subtle bugs e.g. if you used an <code>i</code>
variable. By using uppercase identifiers you indicate that you want these names
to be replaced inside of nested <code>quote!</code> blocks.
</sub>

## [`quote!`](https://docs.rs/metamatch/latest/metamatch/macro.quote.html)
Like `unquote!`, but starts out in quoted mode.
It supports `[< ... >]` styled template tags for more readable templating.
This syntax was inspired by the `paste` crate,
and is mostly useful for larger bodies of code that only have a few dynamic parts.

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

You can switch between quoted and unquoted mode from within any macro using
the `[<quote>]` and `[<unquote>]` template tags.
The `quote!(..)` used earlier is a covenience alias for `[<quote>]..[</quote>]`


## License
[MIT](./LICENSE-MIT) or [Apache Version 2.0](./LICENSE-APACHE), at your option.
