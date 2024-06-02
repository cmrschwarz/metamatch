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

fn slice_baseline(f: &DynVec) -> DynSlice {
    match f {
        DynVec::I32(v) => DynSlice::I32(&v),
        DynVec::I64(v) => DynSlice::I64(&v),
        DynVec::F32(v) => DynSlice::F32(&v),
        DynVec::F64(v) => DynSlice::F64(&v),
    }
}

#[cfg(any())]
fn slice_metamatch(f: &DynVec) -> DynSlice {
    metamatch! {
        match f {
            #[expand(T, [I32, I64, F32, F64])]
            DynVec::T(v) => DynSlice::T(&v),
        }
    }
}

#[cfg(any())]
impl DynVec {
    pub fn len(&self) -> usize {
        metamatch!(
            match self {
                #[expand(T in [I32, I64, F32, F64])]
                Self::~T(v) => v.len(),
            }
        );
    }
}

#[test]
fn current() {
    let f = DynVec::I32(vec![]);
    let len = metamatch!(match f {
        #[expand( T in [I32, I64, F32, F64] ) ]
        DynVec::T(v) => v.len(),
    });
    assert_eq!(len, 0);
}
