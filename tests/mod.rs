use metamatch::metamatch;
use paste::paste;

#[derive(Debug, PartialEq)]
enum DynVec {
    I32(Vec<i32>),
    I64(Vec<i64>),
    F32(Vec<f32>),
    F64(Vec<f64>),
}

#[derive(Debug, PartialEq)]
enum DynSlice<'a> {
    I32(&'a [i32]),
    I64(&'a [i64]),
    F32(&'a [f32]),
    F64(&'a [f64]),
}

#[test]
fn basic_len() {
    let f = DynVec::I32(vec![]);
    let len = metamatch!(match f {
        #[expand( T in [I32, I64, F32, F64] ) ]
        DynVec::T(v) => v.len(),
    });
    assert_eq!(len, 0);
}

#[test]
fn multi_type() {
    let f = DynVec::I64(vec![]);
    let res = metamatch!(match &f {
        #[expand( T in [I32, I64, F32, F64] ) ]
        DynVec::T(v) => {
            DynSlice::T(v)
        }
    });
    assert_eq!(res, DynSlice::I64(&[]));
}

#[test]
fn multi_expand() {
    let src = DynSlice::F32(&[42.0]);
    let res = metamatch!(match src {
        #[expand(T in [I32, I64])]
        #[allow(clippy::unnecessary_cast)]
        DynSlice::T(v) => DynVec::I64(v.iter().map(|v| *v as i64).collect()),

        #[expand(T in [F32, F64])]
        #[allow(clippy::unnecessary_cast)]
        DynSlice::T(v) => DynVec::F64(v.iter().map(|v| *v as f64).collect()),
    });
    assert_eq!(res, DynVec::F64(vec![42.0]));
}

#[test]
fn multi_pattern() {
    let src = DynVec::F32(vec![42.0]);
    let res = metamatch!(match src {
        #[expand((SRC, TGT) in [
            (I32, I64),
            (I64, I64),
            (F32, F64),
            (F64, F64)
        ])]
        #[allow(clippy::unnecessary_cast)]
        DynVec::SRC(v) => DynVec::TGT(v.iter().map(|v| *v as paste!([< TGT:lower >])).collect()),
    });
    assert_eq!(res, DynVec::F64(vec![42.0]));
}
