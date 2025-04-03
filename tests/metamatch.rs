use metamatch::metamatch;

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

impl DynVec {
    fn as_slice(&self) -> DynSlice<'_> {
        metamatch!(match self {
            #[expand(for T in [I32, I64, F32, F64])]
            DynVec::T(v) => DynSlice::T(v),
        })
    }

    fn promote_to_64(&mut self) {
        metamatch!(match self {
            // multiple replacement expressions supported
            #[expand(for (SRC, TGT, TYPE) in [
                (I32, I64, i64),
                (F32, F64, f64),
            ])]
            DynVec::SRC(v) => {
                *self = DynVec::TGT(
                    std::mem::take(v).into_iter().map(|v| v as TYPE).collect(),
                );
            }

            // the types are unused, the match body can be shared
            #[expand_pattern(for T in [I64, F64])]
            DynVec::T(_) => (),
        })
    }

    fn len(&self) -> usize {
        metamatch!(match self {
            #[expand(for T in [I32, I64, F32, F64])]
            DynVec::T(v) => v.len(),
        })
    }
}

#[test]
fn basic_len() {
    let f = DynVec::I32(vec![]);
    assert_eq!(f.len(), 0);
}

#[test]
fn basic_as_slice() {
    let f = DynVec::I32(vec![]);
    assert_eq!(f.as_slice(), DynSlice::I32(&[]));
}

#[test]
fn basic_promote_to_64() {
    let mut f = DynVec::I32(vec![42]);
    f.promote_to_64();
    assert_eq!(f, DynVec::I64(vec![42]));
}

#[test]
fn multi_type() {
    let f = DynVec::I64(vec![]);
    let res = metamatch!(match &f {
        #[expand(for T in [I32, I64, F32, F64])]
        DynVec::T(v) => DynSlice::T(v),
    });
    assert_eq!(res, DynSlice::I64(&[]));
}

#[test]
fn multi_type_nested() {
    let f = DynVec::I64(vec![]);
    let res = metamatch!(match &f {
        #[expand(for T in [I32, I64, F32, F64])]
        DynVec::T(v) => {
            let res = { DynSlice::T(v) };
            res
        }
    });
    assert_eq!(res, DynSlice::I64(&[]));
}

#[test]
fn multi_expand() {
    let src = DynSlice::F32(&[42.0]);
    let res = metamatch!(match src {
        #[expand(for T in [I32, I64])]
        #[allow(clippy::unnecessary_cast)]
        DynSlice::T(v) => DynVec::I64(v.iter().map(|v| *v as i64).collect()),

        #[expand(for T in [F32, F64])]
        #[allow(clippy::unnecessary_cast)]
        DynSlice::T(v) => DynVec::F64(v.iter().map(|v| *v as f64).collect()),
    });
    assert_eq!(res, DynVec::F64(vec![42.0]));
}

#[test]
fn multi_pattern() {
    let src = DynVec::F32(vec![42.0]);
    let res = metamatch!(match src {
        #[expand(for (SRC, TGT, CAST) in [
            (I32, I64, i64),
            (I64, I64, i64),
            (F32, F64, f64),
            (F64, F64, f64)
        ])]
        #[allow(clippy::unnecessary_cast)]
        DynVec::SRC(v) => DynVec::TGT(v.iter().map(|v| *v as CAST).collect()),
    });
    assert_eq!(res, DynVec::F64(vec![42.0]));
}

#[test]
fn paste_macro_interaction() {
    let src = DynVec::F32(vec![42.0]);
    let res = metamatch!(match src {
        #[expand(for (SRC, TGT) in [
            (I32, I64),
            (I64, I64),
            (F32, F64),
            (F64, F64),
        ])]
        #[allow(clippy::unnecessary_cast)]
        DynVec::SRC(v) => DynVec::TGT(
            v.iter().map(|v| *v as [< lowercase(TGT) >]).collect()
        ),
    });
    assert_eq!(res, DynVec::F64(vec![42.0]));
}

#[test]
fn consecutive_no_comma() {
    let src = DynVec::F32(vec![42.0]);
    let res = metamatch!(match &src {
        #[expand(for T in [I32, I64])]
        DynVec::T(v) => {
            DynSlice::T(v)
        }
        #[expand(for T in [F32, F64])]
        DynVec::T(v) => {
            DynSlice::T(v)
        }
    });
    assert_eq!(res, DynSlice::F32(&[42.0]));
}

#[test]
fn braced_match_expression() {
    let f = DynVec::I32(vec![1, 2, 3]);
    #[allow(clippy::blocks_in_conditions)]
    let len = metamatch!(match { f } {
        #[expand(for T in [I32, I64, F32, F64])]
        DynVec::T(v) => v.len(),
    });
    assert_eq!(len, 3);
}

#[test]
fn expand_pattern() {
    let f = DynVec::I32(vec![]);
    let is_int = metamatch!(match f {
        #[expand_pattern(for T in [I32, I64])]
        DynVec::T(_) => true,

        #[expand_pattern(for T in [F32, F64])]
        DynVec::T(_) => {
            false
        }
    });
    assert!(is_int);
}

#[test]
fn expand_pattern_single_variant() {
    let f = DynVec::I32(vec![]);
    let is_int = metamatch!(match f {
        #[expand_pattern(for T in [()])]
        DynVec::I32(_) | DynVec::I64(_) => true,

        #[expand_pattern(for T in [F32, F64])]
        DynVec::T(_) => false,
    });
    assert!(is_int);
}

#[test]
fn expand_chars() {
    let c = 'b';
    let v = metamatch!(match c {
        #[expand_pattern(for C in ['a', 'b', 'c'])]
        C => 42,
        _ => 69,
    });
    assert_eq!(v, 42);
}
