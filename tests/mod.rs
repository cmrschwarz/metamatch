use metamatch::template;

#[test]
fn foo() {
    template! {
        #[derive(PartialEq, Eq)]
        enum Foo{
            [<for VARIANT in [A, B, C]>]
                VARIANT,
            [</for>]
        }
    };

    let x = Foo::A != Foo::C;
}

#[test]
fn macro_errors() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/**/*.rs");
}
