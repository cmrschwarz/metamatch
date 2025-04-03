use metamatch::{quote, unquote};

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
fn macro_errors() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/**/*.rs");
}
