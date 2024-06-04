# `metamatch!`

[![github]](https://github.com/cmrschwarz/metamatch)&ensp;
[![github-build]](https://github.com/cmrschwarz/metamatch/actions/workflows/ci.yml)&ensp;
[![crates-io]](https://crates.io/crates/metamatch)&ensp;
[![msrv]](https://crates.io/crates/metamatch)&ensp;
[![docs-rs]](https://docs.rs/metamatch)&ensp;

[github]: https://img.shields.io/badge/cmrschwarz/metamatch-8da0cb?style=for-the-badge&labelColor=555555&logo=github
[github-build]: https://img.shields.io/github/actions/workflow/status/cmrschwarz/metamatch/ci.yml?branch=main&style=for-the-badge&logo=github
[crates-io]: https://img.shields.io/crates/v/metamatch.svg?style=for-the-badge&color=fc8d62&logo=rust
[docs-rs]: https://img.shields.io/badge/docs.rs-metamatch-66c2a5?style=for-the-badge&labelColor=555555&logo=docs.rs
[msrv]: https://img.shields.io/crates/msrv/metamatch?style=for-the-badge&logo=rust

A rust proc-macro for generating repetitive match arms.

Match arms for enum variants of *different types* cannot be combined,
even if the match arm bodies are *syntactically* identical.

This macro implements a simple templating attribute (`#[expand]`)
to automatically stamp out the neccessary copies.

Due to limitations on attributes in stable rust, a functional macro
(`metamatch!`) is currently required around the full match expression.
Rustfmt and rust-analyzer are fully able to reason about the macro.
Even auto refactorings affecting the `#[expand]`,
like changing the name of an enum variant, work correctly.

## Example

```rust
use metamatch::metamatch;

enum Number {
    I32(i32),
    I64(i64),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
}

impl Number {
    fn as_i32(&self) -> Option<i32> {
        metamatch!(match self {
            Self::I32(v) => Some(*v),

            #[expand(T in [I64, U32, U64])]
            Self::T(v) => (*v).try_into().ok(),

            // multiple expands in the same match possible
            #[expand(T in [F32, F64])]
            Self::T(v) => Some(*v as i32)
        })
    }
    fn promote_to_64(&mut self) {
        metamatch!(match self {
            // multiple replacement expressions supported
            #[expand((SRC, TGT, TYPE) in [
                (I32, I64, i64),
                (U32, U64, u64),
                (F32, F64, f64),
            ])]
            Self::SRC(v) => {
                *self = Self::TGT(*v as TYPE)
            }

            // no #[expand] needed, types are unused
            Self::I64(_) | Self::U64(_) | Self::F64(_) => (),
        })
    }
}
```

## License
[MIT](./LICENSE)
