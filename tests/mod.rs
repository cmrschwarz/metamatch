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

#[test]
fn basic() {
    let f = DynVec::I32(vec![]);
    let len = metamatch!(match f {
        #[expand( T in [I32, I64, F32, F64] ) ]
        DynVec::T(v) => v.len(),
    });
    assert_eq!(len, 0);
}

#[test]
fn multi_type() {
    let f = DynVec::I32(vec![]);
    let len = metamatch!(match f {
        #[expand( T in [I32, I64, F32, F64] ) ]
        DynVec::T(v) => v.len(),
    });
    assert_eq!(len, 0);
}
