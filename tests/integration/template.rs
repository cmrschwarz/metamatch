use metamatch::template;

#[test]
fn basic_enum_variants() {
    template! {
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
fn let_pattern() {
    let asdf = template! {
        [<let (FOO, BAR) = (42, 27)>]
        FOO + BAR
    };

    assert_eq!(asdf, 69);
}

#[test]
fn quote_expr() {
    let res = template! {
        [<for X in [1, quote!(+2), quote!(+3)]>]
            X
        [</for>]
    };

    assert_eq!(res, 6);
}

#[test]
fn eval_block() {
    let res = template! {
        1
        [<eval>]
        for X in [2, 3, 4] {
            quote!(+X)
        }
        [</eval>]
    };

    assert_eq!(res, 10);
}

#[test]
fn quote_array() {
    let array: [i32; 4] = template! {
        [
            [<for X in 1..5>]
            X,
            [</for>]
        ]
    };
    assert_eq!(array, [1, 2, 3, 4]);
}

#[test]
fn while_template() {
    let array: [i32; 4] = template! {
        [
            [<let mut X = 1;>]
            [<while X < 5>]
                X,
                [< X += 1>]
            [</while>]
        ]
    };
    assert_eq!(array, [1, 2, 3, 4]);
}

#[test]
fn if_templates_parse() {
    let res = template! {
        // 1
        [<if true>]
            1
        [</if>]

        // +1
        [<if false>]
            +99
        [<else>]
            +1
        [</if>]

        // +1
        [<if false>]
            +99
        [<else if true>]
            +1
        [</if>]

        // <nothing>
        [<if false>]
            99+
        [<else if false>]
            99+
        [</if>]


        // + 1
        [<if false>]
            +99
        [<else if true>]
            +1
        [<else>]
            +99
        [</if>]

        // + 1
        [<if false>]
            +99
        [<else if false>]
            +99
        [<else>]
            +1
        [</if>]
    };

    assert_eq!(res, 5);
}
