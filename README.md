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

Macro expressions using familiar Rust syntax, evaluated by a tiny interpreter.

Two specialized entrypoints for common usecases,
two generalized versions for maximum flexibility.

## [`#[replicate]`](https://docs.rs/metamatch/latest/metamatch/attr.replicate.html)
Generate repetitive syntax like trait impls without giving up on
`rustfmt` or rust-analyzer.

```rust
use metamatch::replicate;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct NodeIdx(usize);

#[replicate(
    let traits = [Add, Sub, Mul, Div, Rem];
    for (TRAIT, FUNC) in zip(traits, traits.map(lowercase))
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

Just like `#[replicate]`, this macro is fully compatible `rustfmt`
and rust-analyzer. It will be correctly formatted like a regular
`match` expression, and is targettable even by auto refactorings
that affect the `#[expand]`, like changing the name of an enum variant.

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
    fn len_(&self) -> usize {
        match self {
            DynVec::I8(v) => v.len(),
            DynVec::I16(v) => v.len(),
            DynVec::I32(v) => v.len(),
            DynVec::I64(v) => v.len(),
        }
    }
}
```

## [`unquote!`](https://docs.rs/metamatch/latest/metamatch/macro.quote.html)
Evaluate arbitrary expressions.
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
Note:  By default, metamatch will not replace identifiers
in your quoted sections that happen to also be defined in your metaprogram.
Because there's no shadowing in the templates (they're just tokens),
this could lead to really subtle bugs e.g. if you used an <code>i</code>
variable in both contexts. Uppercase identifiers indicate to metamatch that you want these names
to be replaced inside of nested <code>quote!</code> blocks.
</sub>

## [`quote!`](https://docs.rs/metamatch/latest/metamatch/macro.quote.html)
Like `unquote!`, but starts out in quoted mode.
It supports `[< ... >]` styled template tags for readable templating
within large blocks of rust code with few dynamic parts.

```rust
use metamatch::quote;

quote! {
    enum ErrorCode {
        [<for err_id in 0..=42>]
            [<ident("E" + str(err_id))>](String),
        [</for>]
    }
};
let err = ErrorCode::E42("oh noes!".to_owned());
```

You can switch between quoted and unquoted mode from within any macro using
the `[<quote>]` and `[<unquote>]` template tags.
The `quote!(..)` used earlier is a covenience alias for `[<quote>]..[</quote>]`


## License
[MIT](./LICENSE-MIT) or [Apache Version 2.0](./LICENSE-APACHE), at your option.
