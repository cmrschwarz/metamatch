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
