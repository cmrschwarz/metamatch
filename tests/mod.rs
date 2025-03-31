use metamatch::template;

#[test]
fn basic_enum_variants() {
    template! {
        enum Foo{
            [<for (i, VARIANT) in enumerate([A, B, C])>]
                VARIANT,
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
fn macro_errors() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/**/*.rs");
}
