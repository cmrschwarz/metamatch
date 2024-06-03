# Metamatch

[<img alt="github" src="https://img.shields.io/badge/cmrschwarz/metamatch-8da0cb?style=for-the-badge&labelColor=555555&logo=github" height="20">](https://github.com/cmrschwarz/metamatch)
[<img alt="build status" src="https://img.shields.io/github/actions/workflow/status/cmrschwarz/metamatch/ci.yml?branch=main&style=for-the-badge&logo=github" height="20">](https://github.com/dtolnay/seq-macro/actions?query=branch%3Amaster)
[<img alt="crates.io" src="https://img.shields.io/crates/v/metamatch.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/metamatch)
[<img alt="MSRV" src="https://img.shields.io/crates/msrv/metamatch?style=for-the-badge&logo=rust" height="20">](https://crates.io/crates/metamatch)
[<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-metamatch-66c2a5?style=for-the-badge&labelColor=555555&logo=docs.rs" height="20">](https://docs.rs/metamatch)


A rust proc-macro for generating repetitive match arms.

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

            #[expand(T in [F32, F64])]
            Self::T(v) => Some(*v as i32)
        })
    }
    fn promote_to_64(&mut self) {
        metamatch!(match self {
            #[expand(T in [I64, U64, F64])]
            Self::T(_) => (),

            #[expand((SRC, TGT, TYPE) in [
                (I32, I64, i64),
                (U32, U64, u64),
                (F32, F64, f64),
            ])]
            Self::SRC(v) => {
                *self = Self::TGT(*v as TYPE)
            }
        })
    }
}
```
