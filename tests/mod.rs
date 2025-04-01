use metamatch::{eval, template};

#[test]
fn basic_enum_variants() {
    template! {
        enum Foo{
            [<for (i, VARIANT) in enumerate([A, B, C])>]
                VARIANT = i,
            [</for>]
        }
    };

    let _ = [Foo::A, Foo::B, Foo::C];
}

#[test]
fn let_bindings() {
    template! {
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
fn raw_expr() {
    let res = template! {
        [<for X in [1, raw!(+2), raw!(+3)]>]
            X
        [</for>]
    };

    assert_eq!(res, 6);
}

#[test]
fn raw_scope() {
    let res = eval! {
        raw!(1);
        for X in [2, 3, 4] {
            raw!(+ X);
        }
    };

    assert_eq!(res, 6);
}

#[test]
fn eval_block() {
    let res = template! {
        1
        [<eval>]
        for X in [2, 3, 4] {
            raw!(+X)
        }
        [</eval>]
    };

    assert_eq!(res, 6);
}

#[test]
fn macro_errors() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/**/*.rs");
}
