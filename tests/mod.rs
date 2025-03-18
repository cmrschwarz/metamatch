use metamatch::{expand, metamatch, replicate};
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

    fn len(&self) -> usize {
        metamatch!(match self {
            #[expand(T in [I32, I64, F32, F64])]
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
        #[expand(T in [I32, I64, F32, F64])]
        DynVec::T(v) => DynSlice::T(v),
    });
    assert_eq!(res, DynSlice::I64(&[]));
}

#[test]
fn multi_type_nested() {
    let f = DynVec::I64(vec![]);
    let res = metamatch!(match &f {
        #[expand(T in [I32, I64, F32, F64])]
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
        #[expand((SRC, TGT, CAST) in [
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
        #[expand((SRC, TGT) in [
            (I32, I64),
            (I64, I64),
            (F32, F64),
            (F64, F64),
        ])]
        #[allow(clippy::unnecessary_cast)]
        DynVec::SRC(v) => DynVec::TGT(
            v.iter().map(|v| *v as paste!([< TGT:lower >])).collect()
        ),
    });
    assert_eq!(res, DynVec::F64(vec![42.0]));
}

#[test]
fn consecutive_no_comma() {
    let src = DynVec::F32(vec![42.0]);
    let res = metamatch!(match &src {
        #[expand(T in [I32, I64])]
        DynVec::T(v) => {
            DynSlice::T(v)
        }
        #[expand(T in [F32, F64])]
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
        #[expand(T in [I32, I64, F32, F64])]
        DynVec::T(v) => v.len(),
    });
    assert_eq!(len, 3);
}

#[test]
fn expand_pattern() {
    let f = DynVec::I32(vec![]);
    let is_int = metamatch!(match f {
        #[expand_pattern(T in [I32, I64])]
        DynVec::T(_) => true,

        #[expand_pattern(T in [F32, F64])]
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
        #[expand_pattern(T in [()])]
        DynVec::I32(_) | DynVec::I64(_) => true,

        #[expand_pattern(T in [F32, F64])]
        DynVec::T(_) => false,
    });
    assert!(is_int);
}

#[test]
fn direct_expand_expr() {
    let mut x = 0;

    expand!(T in [1, 2, 3] {
        x += T;
    });

    assert_eq!(x, 6);
}

#[test]
fn matrix_impl_using_replicate() {
    trait Foo<T> {
        fn foo(&self, other: T);
    }

    #[replicate((SELF, OTHER) in matrix([i32, i64], [i32, i64]))]
    impl Foo<OTHER> for SELF {
        fn foo(&self, other: OTHER) {
            println!("{:?} {:?}", self, other);
        }
    }

    let i32_val = 42;
    let i64_val = 42i64;

    i32_val.foo(i32_val);
    i32_val.foo(i64_val);
    i64_val.foo(i32_val);
    i64_val.foo(i64_val);
}

#[test]
fn expand_expr_empty() {
    #[allow(unused_mut)]
    let mut x = 0;

    expand!(T in [] {
        x += T;
    });

    assert_eq!(x, 0);
}

#[test]
fn expand_expr_array() {
    let arr: [i32; 3] = expand!((T) in matrix([1, 2, 3]) *[
        T,
    ]);
    assert_eq!(arr.iter().sum::<i32>(), 6);
}

#[test]
fn expand_nested_array() {
    let arr: [[i32; 2]; 9] = expand!(
        (X, Y) in matrix([1, 2, 3], [1, 2, 3]) *[
            [X, Y],
        ]
    );
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
    assert_eq!(arr, result);
}

#[test]
fn expand_expr_matrix() {
    let arr: [[i32; 2]; 4] = expand!((A, B) in matrix([0, 1], [0, 1]) *[
       [A, B],
    ]);
    assert_eq!(arr, [[0, 0], [0, 1], [1, 0], [1, 1]]);
}

#[test]
fn macro_errors() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/**/*.rs");
}
