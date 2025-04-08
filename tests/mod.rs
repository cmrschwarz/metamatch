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
    let array: [i32; 4] = quote! {
        [
            [<for X in 1..5>]
            X,
            [</for>]
        ]
    };
    assert_eq!(array, [1, 2, 3, 4]);
}

#[test]
fn unquote_array() {
    const ARRAY: [i32; 4] = unquote! {
        let ELEMENTS = for X in 1..5 {
            quote!(X,)
        };
        quote!([ELEMENTS])
    };
    assert_eq!(ARRAY, [1, 2, 3, 4]);
}

#[test]
fn ufcs() {
    let list_len = unquote! {
        let list = [1, 2, 3];
        list.len()
    };
    assert_eq!(list_len, 3);

    let list_len = unquote! {
        [1, 2, 3].len()
    };
    assert_eq!(list_len, 3);

    let zipped_lists = unquote! {
        [1, 2, 3].zip([4, 5, 6])
    };
    assert_eq!(zipped_lists, [(1, 4), (2, 5), (3, 6)]);
}

#[test]
fn lambda_expressions() {
    let result = unquote! {
        let add = |x, y| x + y;
        add(2, 3)
    };
    assert_eq!(result, 5);

    let result = unquote! {
        let add_tup = |(x, y)| x + y;
        add_tup((1, 2))
    };
    assert_eq!(result, 3);

    let result = unquote! {
        (|x| x + 1)(5)
    };
    assert_eq!(result, 6);
}

#[test]
fn char_type() {
    let result = unquote! {
        let x = "asdf".chars()[0];
        x
    };
    assert_eq!(result, 'a');

    let result = unquote! {
        let x = 'x';
        x
    };
    assert_eq!(result, 'x');

    let result = unquote! {
        let x = "äbc".bytes()[3];
        x
    };
    assert_eq!(result, "äbc".as_bytes()[3]);
}

#[test]
fn if_statements() {
    let res = unquote! {
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
    let res = quote! {
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
    let res = unquote! {
        fn expand(x) {
            [<quote>]
                [<if x==1>]
                    1
                [<else if x==2>]
                    2
                [<else if x==3>]
                    3
                [<else>]
                    4
                [</if>]
            [</quote>]
        }
        (1..=5).map(expand)
    };
    assert_eq!(res, [1, 2, 3, 4, 4]);
}

#[test]
fn macro_errors() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/**/*.rs");
}
