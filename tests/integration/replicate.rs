use metamatch::replicate;

#[test]
fn replicate_trait_defs() {
    #[derive(Debug, PartialEq)]
    struct NodeIdx(usize);

    #[replicate(for (TRAIT, FUNC) in [
        (Add, add),
        (Sub, sub),
        (Mul, mul),
        (Div, div),
    ])]
    impl core::ops::TRAIT for NodeIdx {
        type Output = Self;
        fn FUNC(self, other: Self) -> Self {
            NodeIdx(self.0.FUNC(other.0))
        }
    }

    assert_eq!(NodeIdx(1) + NodeIdx(2), NodeIdx(3));

    assert_eq!(NodeIdx(2) * NodeIdx(3), NodeIdx(6));
}

#[test]
fn replicate_trait_defs_fancy() {
    #[derive(Debug, PartialEq)]
    struct NodeIdx(usize);

    #[replicate{
        let traits = [Add, Sub, Mul, Div];
        for (TRAIT, FUNC) in zip(traits, traits.map(lowercase))
    }]
    impl core::ops::TRAIT for NodeIdx {
        type Output = Self;
        fn FUNC(self, other: Self) -> Self {
            NodeIdx(self.0.FUNC(other.0))
        }
    }

    assert_eq!(NodeIdx(1) + NodeIdx(2), NodeIdx(3));
    assert_eq!(NodeIdx(2) * NodeIdx(3), NodeIdx(6));
}

#[test]
fn replicate_while_let() {
    #![allow(unused)]
    #[replicate(
        let items = (1..10);
        while let (FN, X, Y) = zip(items.map(|x| ident("f_" + str(x))), items, items.map(|x|x*2))
    )]
    fn FN() -> i32 {
        X + Y
    }
    assert_eq!(f_5(), 15);
}

#[test]
fn replicate_with_use() {
    metamatch::eval! {
        extern let FOO = [A, B, C];
    }

    #[replicate(for T in use FOO)]
    struct T;

    let _a = A;
    let _b = B;
    let _c = C;
}

#[test]
fn raw_expand_not_evaluated() {
    metamatch::eval! {
        extern let my_fns = [foo, bar, baz];
    }

    #[allow(clippy::needless_lifetimes)]
    #[replicate(for FN in (use my_fns))]
    fn FN<'a>(x: &'a i32) -> i32 {
        *x
    }

    assert_eq!(foo(&1) + bar(&2) + baz(&3), 6);
}

#[test]
fn empty_raw_block_preserved() {
    #![allow(clippy::needless_lifetimes, clippy::unnecessary_mut_passed)]

    metamatch::eval! {
        extern let my_fns = [(foo_mut, mut), (bar, raw!())];
    }

    #[replicate(for (FN, super r#mut) in (use my_fns))]
    fn FN<'a>(x: &'a mut i32) -> i32 {
        *x
    }

    assert_eq!(foo_mut(&mut 1) + bar(&2), 3);
}

#[test]
fn replicate_applies_fn() {
    #![allow(clippy::needless_lifetimes, clippy::unnecessary_mut_passed)]

    #[replicate{
        fn foo(super x) {
            quote!(x);
        }
        foo
    }]
    fn foo_mut<'a>(x: &'a mut i32) -> i32 {
        *x
    }

    assert_eq!(foo_mut(&mut 1), 1);
}

#[test]
fn replicate_applies_extern_fn() {
    #![allow(clippy::needless_lifetimes, clippy::unnecessary_mut_passed)]

    metamatch::eval! {
        extern fn non_mut(x) {
            x
        }
    }

    #[replicate(use non_mut)]
    fn foo_mut<'a>(x: &'a mut i32) -> i32 {
        *x
    }

    assert_eq!(foo_mut(&mut 1), 1);
}
