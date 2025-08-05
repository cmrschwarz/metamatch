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

Two specialized entrypoints for common usecases (`metamatch!` and `#[replicate]`),
two generalized versions for maximum flexibility (`template!` and `eval!`).

## [`metamatch!`](https://docs.rs/metamatch/latest/metamatch/macro.metamatch.html)
The original motivation and namesake of this crate.

Match arms for differently typed variants cannot be combined, even if the are
syntactically identical. This macro lets you stamp out the neccessary
copies using (`#[expand]`).

This macro is fully compatible `rustfmt` and rust-analyzer.
It will be correctly formatted like a regular
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
}
```

## [`eval!`](https://docs.rs/metamatch/latest/metamatch/macro.eval.html)
Evaluate arbitrary expressions.
```rust
use metamatch::eval;

const ARRAY: [i32; 4] = eval! {
    let ELEMENTS = for X in 1..5 {
        quote!(X,)
    };
    quote!([ELEMENTS])
};

assert_eq!(ARRAY, [1, 2, 3, 4]);
```

<sub>
Note:  By default, metamatch will not replace identifiers
in your `quote!`d sections that happen to also be defined in your metaprogram.
Because there's no shadowing in the templates (they're just tokens),
this could lead to really subtle bugs e.g. if you used an <code>i</code>
variable in both contexts. Screaming case identifiers indicate to metamatch that
you want these names to be replaced inside of nested <code>quote!</code> blocks.
</sub>

## [`template!`](https://docs.rs/metamatch/latest/metamatch/macro.quote.html)
Embed dynamic chunks into a larger body of Rust source.
It uses `[< ... >]` styled template tags for readable templating
within large blocks of rust code with few dynamic parts.

```rust
use metamatch::template;

template! {
    enum ErrorCode {
        [<for err_id in 0..=42>]
            [<ident("E" + str(err_id))>](String),
        [</for>]
    }
};

let err = ErrorCode::E42("oh noes!".to_owned());
```
See the documentation of
[`template!`](https://docs.rs/metamatch/latest/metamatch/macro.template.html) for
a full list of template tags.
You can switch back and forth between template and eval mode from within
any macro using the `[<eval>]` tag and `quote!` pseudo-macro respectively.


## [`#[replicate]`](https://docs.rs/metamatch/latest/metamatch/attr.replicate.html)
Generate repetitive syntax like trait impls using an annotation style macro.
Just like `metamatch!`, this style preserves support for `rustfmt` and rust-analyzer.

```rust
use metamatch::replicate;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct NodeIdx(usize);

#[metamatch::replicate(
    let traits = [Add, Sub, Mul, Div, Rem];
    let trait_fns = traits.map(lowercase);
    for (TRAIT, FN) in zip(traits, trait_fns)
)]
impl std::ops::TRAIT for NodeIdx {
    type Output = Self;
    fn FN(self, rhs: Self) -> Self {
        NodeIdx(self.0.FN(rhs.0))
    }
}

assert!(NodeIdx(1) + NodeIdx(2) == NodeIdx(3));
```

## Supported Rust Syntax
- `let` statements and basic pattern matching.
- `loop`, `while`, `while let`, and `for` loops, including `continue` and
  `break`
- `if` and `else` blocks
- Functions (`fn`)  and lambdas (`|..|`)
- Arrays (`[1,2,3]`)
- Ranges (`0..=10`)
- All basic operators: `+`, `-`, `*`, `/`, `%`, `<<`, `>>`, `=`, `+=`,
  `-=`, `*=`, `/=`, `%=`, `<<=`, `>>=`, `&=`, `|=`, `^=`, `&`, `|`, `^`,
  `!`,  `<`, `>`, `<=`, `>=`, `==`, `!=`, `&&`, `||`, `[..]`

## Currently not Supported
- `struct`, `enum`, `match`, `type`, `trait`, ...

## Builtin Functions
Functions support UFCS, so `[1,2,3].len() == len([1,2,3])`
All `str -> str` functions also work `token -> token`.

- `lowercase(str) -> str`
- `uppercase(str) -> str`
- `capitalize(str) -> str`
- `enumerate([T]) -> [(int, T)]`
- `zip([A], [B], ..) -> [(A, B, ..)]`
- `map([T], Fn(T) -> U) -> [U]`
- `chars(str) -> [char]`
- `bytes(str) -> [int]`
- `ident(str) -> token`
- `str(any) -> str`
- `len([T]) -> int`

## Special Purpose 'Macros'
- `quote!(..) -> [token]`: Like a nested `template!`, this evaluates to a list of raw tokens.
- `raw!(..) -> [token]`: like `quote!`, but no meta variable expansion or template tags.

Just like Rust macros, you can use any of `{}`, `[]`, and `()`
interchangably for these macro invocations.

## Shared Data between Macros
Metamatch supports `extern` function and `let` declarations to share
state between macro invocations.

```rust
metamatch::eval! {
    extern let MY_CONST = 21;
    extern fn double_me (x) {
        x * 2
    };
}


let res = metamatch::eval! {
    // import extern symbols from other macros
    use {MY_CONST, double_me};

    double_me(MY_CONST)
};
assert_eq!(res, 42);
```

Extern symbols desugar into a declarative macro (`macro_rules!`).

The therefore follow the same scoping rules as declarative macros do
(definitions must come lexically before uses).

You can use `pub extern` to have a `#[macro_export]`
attribute generated for an extern symbol.


## License
[MIT](https://github.com/cmrschwarz/metamatch/blob/main/LICENSE-MIT)
or [Apache Version 2.0](https://github.com/cmrschwarz/metamatch/blob/main/LICENSE-APACHE),
at your option.
