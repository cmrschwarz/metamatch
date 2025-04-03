use metamatch::{quote, replicate, unquote};

#[test]
fn basic_enum_variants() {
    quote! {
        enum Foo{
            [<for (super i, VARIANT) in enumerate([A, B, C])>]
                VARIANT = i,
            [</for>]
        }
    };

    let _ = [Foo::A, Foo::B, Foo::C];
}

#[test]
fn let_bindings() {
    quote! {
        [<let VARIANTS = [A, B, C]>]
        enum Foo{
            [<for VARIANT in VARIANTS >]
                VARIANT,
            [</for>]
        }
    };

    let _ = [Foo::A, Foo::B, Foo::C];
}

#[test]
fn let_pattern() {
    let asdf = quote! {
        [<let (FOO, BAR) = (42, 27)>]
        FOO + BAR
    };

    assert_eq!(asdf, 69);
}

#[test]
fn quote_expr() {
    let res = quote! {
        [<for X in [1, quote!(+2), quote!(+3)]>]
            X
        [</for>]
    };

    assert_eq!(res, 6);
}

#[test]
fn quote_stmt() {
    let res = unquote! {
        quote!(1);
        for X in [2, 3, 4] {
            quote!(+ X);
        }
    };

    assert_eq!(res, 10);
}

#[test]
fn quote_block() {
    let res = unquote! {
        quote!(1);
        for X in [2, 3, 4] {
            [<quote>]
                +X
            [</quote>]
        }
    };

    assert_eq!(res, 10);
}

#[test]
fn unquote_block() {
    let res = quote! {
        1
        [<unquote>]
        for X in [2, 3, 4] {
            quote!(+X)
        }
        [</unquote>]
    };

    assert_eq!(res, 10);
}

#[test]
fn raw_expr() {
    let res = quote! {
        [<for X in [1, raw!(+2), raw!(+3)]>]
            X
        [</for>]
    };

    assert_eq!(res, 6);
}

#[test]
fn raw_stmt() {
    const X: i64 = 5;
    let res = unquote! {
        raw!(1);
        for X in [2, 3, 4] {
            // this will *not* evaluate the X
            raw!(+ X);
        }
    };

    assert_eq!(res, 16);
}

#[test]
fn raw_block() {
    const X: i64 = 5;
    let res = unquote! {
        raw!(1);
        for X in [2, 3, 4] {
            // this will *not* evaluate the X
            [<raw>]
            + X
            [</raw>]
        }
    };
    assert_eq!(res, 16);
}

#[test]
fn lowercase_vars_not_superbound() {
    let x: i64 = 5;
    let res = unquote! {
        raw!(1);
        for x in [2, 3, 4] {
            quote!{
                + x
            }
        }
    };
    assert_eq!(res, 16);
}

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
fn quote_array() {
    let array: [i32; 10] = quote! {
        [
            [<for X in 0..5>]
            X,
            [</for>]
        ]
    };
    assert_eq!(array, [0, 1, 2, 3, 4]);
}

#[test]
fn unquote_array() {
    const ARRAY: [i32; 4] = unquote! {
        let ELEMENTS = for X in 1..5 {
            quote!(X,)
        }:
        quote!([ELEMENTS])
    };
    assert_eq!(ARRAY, [1, 2, 3, 4]);
}

#[test]
fn macro_errors() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/**/*.rs");
}
