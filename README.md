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

A Rust proc-macro for generating repetitive `match` arms.

Match arms for differently typed variants usually cannot be combined,
even if the are syntactically identical.
This macro implements a simple templating attribute (`#[expand]`)
to automatically stamp out the neccessary copies.

`rust-analyzer` works correctly with this macro.
Even auto refactorings that affect the `#[expand]` (like changing the
name of an enum variant) work correctly.

We can get `rustfmt` to nicely format our macro by using
parentheses`()` instead of braces`{}` for the `metamatch!` expression.

Zero dependencies on other crates.

## [`metamatch!`](https://docs.rs/metamatch/latest/metamatch/macro.metamatch.html)

A proc-macro for generating repetitive match arms.

```rust
use metamatch::metamatch;
enum MyEnum {
    A(i8),
    B(i16),
    C(i32),
    D(i64),
}

let mut double_me = MyEnum::A(42);

metamatch!(match &mut double_me {
    #[expand(for T in [A, B, C, D])]
    MyEnum::T(v) => *v *= 2,
})
```

For more complex examples have a look at the
[documentation](https://docs.rs/metamatch/latest/metamatch/macro.metamatch.html).


## [`#[replicate]`](https://docs.rs/metamatch/latest/metamatch/attr.replicate.html)
An attribute styled proc-macro with similar syntax to `metamatch!`.

```
use metamatch::replicate;

struct NodeIdx(usize);

#[replicate(for (TRAIT, FUNC) in [
    (Add, add),
    (Sub, sub),
    (Mul, mul),
    (Div, div)
])]
impl core::ops::TRAIT for NodeIdx {
    type Output = Self;
    fn FUNC(self, other: Self) -> Self {
        NodeIdx(self.0.FUNC(other.0))
    }
}

```

## [`expand!`](https://docs.rs/metamatch/latest/metamatch/macro.expand.html)
A generalized version of `metamatch!` for arbitrary expressions.

```
use metamatch::expand;

let multi_dim_array: [[i32; 2]; 9] = expand!{
    for (X, Y) in matrix([1, 2, 3], [1, 2, 3]) *[
        [X, Y],
    ]
};
let result: [[i32; 2]; 9] = [
    [1, 1],
    [1, 2],
    [1, 3],
    [2, 1],
    [2, 2],
    [2, 3],
    [3, 1],
    [3, 2],
    [3, 3],
];
assert_eq!(multi_dim_array, result);
```




## License
[MIT](./LICENSE-MIT) or [Apache Version 2.0](./LICENSE-APACHE), at your option.
