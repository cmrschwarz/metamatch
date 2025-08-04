use metamatch::{eval, replicate, template};

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
        [<for X in [1, template!(+2), template!(+3)]>]
            X
        [</for>]
    };

    assert_eq!(res, 6);
}

#[test]
fn quote_stmt() {
    let res = eval! {
        template!(1);
        for X in [2, 3, 4] {
            template!(+ X);
        }
    };

    assert_eq!(res, 10);
}

#[test]
fn template_block() {
    let res = eval! {
        template!(1);
        for X in [2, 3, 4] {
            [<template>]
                +X
            [</template>]
        }
    };

    assert_eq!(res, 10);
}

#[test]
fn eval_block() {
    let res = template! {
        1
        [<eval>]
        for X in [2, 3, 4] {
            template!(+X)
        }
        [</eval>]
    };

    assert_eq!(res, 10);
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
fn raw_stmt() {
    const X: i64 = 5;
    let res = eval! {
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
    let res = eval! {
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
    let res = eval! {
        raw!(1);
        for x in [2, 3, 4] {
            template!{
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
fn eval_array() {
    const ARRAY: [i32; 4] = eval! {
        let ELEMENTS = for X in 1..5 {
            template!(X,)
        };
        template!([ELEMENTS])
    };
    assert_eq!(ARRAY, [1, 2, 3, 4]);
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
fn ufcs() {
    let list_len = eval! {
        let list = [1, 2, 3];
        list.len()
    };
    assert_eq!(list_len, 3);

    let list_len = eval! {
        [1, 2, 3].len()
    };
    assert_eq!(list_len, 3);

    let zipped_lists = eval! {
        [1, 2, 3].zip([4, 5, 6])
    };
    assert_eq!(zipped_lists, [(1, 4), (2, 5), (3, 6)]);
}

#[test]
fn lambda_expressions() {
    let result = eval! {
        let add = |x, y| x + y;
        add(2, 3)
    };
    assert_eq!(result, 5);

    let result = eval! {
        let add_tup = |(x, y)| x + y;
        add_tup((1, 2))
    };
    assert_eq!(result, 3);

    let result = eval! {
        (|x| x + 1)(5)
    };
    assert_eq!(result, 6);
}

#[test]
fn char_type() {
    let result = eval! {
        let x = "asdf".chars()[0];
        x
    };
    assert_eq!(result, 'a');

    let result = eval! {
        let x = 'x';
        x
    };
    assert_eq!(result, 'x');

    let result = eval! {
        let x = "äbc".bytes()[3];
        x
    };
    assert_eq!(result, "äbc".as_bytes()[3]);
}

#[test]
fn if_statements() {
    let res = eval! {
        let x = 5;
        if x % 2 == 0 {
            1
        }
        else if x % 2 == 1 {
            2
        }
        else {
            3
        }
    };

    assert_eq!(res, 2);
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

#[test]
fn if_template_expansion() {
    let res = eval! {
        fn expand(x) {
            [<template>]
                [<if x==1>]
                    1
                [<else if x==2>]
                    2
                [<else if x==3>]
                    3
                [<else>]
                    4
                [</if>]
            [</template>]
        }
        (1..=5).map(expand)
    };
    assert_eq!(res, [1, 2, 3, 4, 4]);
}

#[test]
fn if_within_parentheses_remains_typed() {
    let res = eval! {
        (if true {1} else {2}) + 2
    };
    assert_eq!(res, 3);
}

#[test]
fn loop_break() {
    let res = eval! {
        let mut X = 0;
        loop {
            X += 1;
            if X == 10 {
                break;
            }
            template!(X+);
        }
        template!(X);
    };
    assert_eq!(res, 55);
}

#[test]
fn typed_break_expr() {
    let res = eval! {
        let mut X = 0;
        let super res = loop {
            X += 1;
            if X == 10 {
                break X;
            }
        };
        template!(res);
    };
    assert_eq!(res, 10);
}

#[test]
fn for_continue() {
    let res = eval! {
        let ELEMS = for X in [1, 2, 3] {
            if X % 2 == 0 {
                continue;
            }
            template!(X,);
        };
        template!([ELEMS]);
    };
    assert_eq!(res, [1, 3]);
}

#[test]
fn while_loop() {
    let res = eval! {
        let super mut x = 5;
        let ELEMS = while x > 0  {
            template!(x,);
            x -= 2;
        };
        template!([ELEMS]);
    };
    assert_eq!(res, [5, 3, 1]);
}

#[test]
fn while_let() {
    let res = eval! {
        let ELEMS = while let (X,) = [(1,), (2,), 3, (4,)] {
            template!((X,),);
        };
        template!([ELEMS]);
    };
    assert_eq!(res, [(1,), (2,)]);
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
fn macro_errors() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/**/*.rs");
}
