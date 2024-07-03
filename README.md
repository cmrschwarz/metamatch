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

A rust proc-macro for generating repetitive match arms.

Unless the enum variant type remains unused, match arms for different
variants cannot be combined, even if the match arm bodies are syntactically
identical.

This macro implements a simple templating attribute (`#[expand]`)
to automatically stamp out the neccessary copies.

Rustfmt and rust-analyzer are fully able to reason about this macro.
Even auto refactorings that affect the `#[expand]` (like changing the
name of an enum variant) work correctly.

## Example

```rust
use metamatch::metamatch;

enum DynVec {
    I32(Vec<i32>),
    I64(Vec<i64>),
    F32(Vec<f32>),
    F64(Vec<f64>),
}

enum DynSlice<'a> {
    I32(&'a [i32]),
    I64(&'a [i64]),
    F32(&'a [f32]),
    F64(&'a [f64]),
}

impl DynVec {
    fn as_slice(&self) -> DynSlice<'_> {
        metamatch!(match self {
            #[expand(T in [I32, I64, F32, F64])]
            DynVec::T(v) => DynSlice::T(v),
        })
    }

    fn promote_to_64(&mut self) {
        metamatch!(match self {
            // multiple replacement expressions supported
            #[expand((SRC, TGT, TYPE) in [
                (I32, I64, i64),
                (F32, F64, f64),
            ])]
            DynVec::SRC(v) => {
                *self = DynVec::TGT(
                    std::mem::take(v).into_iter().map(|v| v as TYPE).collect(),
                );
            }

            // the types are unused, the match body can be shared
            #[expand_pattern(T in [I64, F64])]
            DynVec::T(_) => (),
        })
    }
}
```

## License
[MIT](./LICENSE)
