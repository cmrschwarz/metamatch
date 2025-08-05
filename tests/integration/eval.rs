use metamatch::eval;

#[test]
fn quote_stmt() {
    let res = eval! {
        quote!(1);
        for X in [2, 3, 4] {
            quote!(+ X);
        }
    };

    assert_eq!(res, 10);
}

#[test]
fn quote_expr() {
    let res = eval! {
        quote!(1);
        for X in [2, 3, 4] {
            quote! {
                +X
            }
        }
    };

    assert_eq!(res, 10);
}

#[test]
fn raw_expr() {
    let res = eval! {
        for X in [1, raw!(+2), raw!(+3)] {
            X
        }
    };

    assert_eq!(res, 6);
}

#[test]
fn raw_expr_2() {
    const X: i64 = 5;
    let res = eval! {
        raw!(1);
        for X in [2, 3, 4] {
            // this will *not* evaluate the X
            raw!{
                + X
            }
        }
    };
    assert_eq!(res, 16);
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
fn lowercase_vars_not_superbound() {
    let x: i64 = 5;
    let res = eval! {
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
fn eval_array() {
    const ARRAY: [i32; 4] = eval! {
        let ELEMENTS = for X in 1..5 {
            quote!(X,)
        };
        quote!([ELEMENTS])
    };
    assert_eq!(ARRAY, [1, 2, 3, 4]);
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
fn if_template_expansion() {
    let res = eval! {
        fn expand(x) {
            quote! {
                [<if x==1>]
                    1
                [<else if x==2>]
                    2
                [<else if x==3>]
                    3
                [<else>]
                    4
                [</if>]
            }
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
            quote!(X+);
        }
        quote!(X);
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
        quote!(res);
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
            quote!(X,);
        };
        quote!([ELEMS]);
    };
    assert_eq!(res, [1, 3]);
}

#[test]
fn while_loop() {
    let res = eval! {
        let super mut x = 5;
        let ELEMS = while x > 0  {
            quote!(x,);
            x -= 2;
        };
        quote!([ELEMS]);
    };
    assert_eq!(res, [5, 3, 1]);
}

#[test]
fn while_let() {
    let res = eval! {
        let ELEMS = while let (X,) = [(1,), (2,), 3, (4,)] {
            quote!((X,),);
        };
        quote!([ELEMS]);
    };
    assert_eq!(res, [(1,), (2,)]);
}

#[test]
fn func_to_tokens() {
    let double_me = eval! {
        fn double_me(x) {
            x * 2
        }
        double_me
    };
    assert_eq!(42, double_me(21));
}

#[test]
fn extern_decl() {
    eval! {
        extern let foo = [1, 2, 3];
        extern let bar = [4, 5, 6];
    };

    eval! {
        use {foo, bar};
        extern let baz = foo + bar;
    };

    let res = eval! {
        use baz;
        baz
    };

    assert_eq!(res, [1, 2, 3, 4, 5, 6]);
}

#[test]
fn extern_func_decl() {
    eval! {
        extern fn double(x) {
            x * 2
        }
    };

    let res = eval! {
        use double;
        double(21);
    };

    assert_eq!(res, 42);
}

#[test]
fn func_decl_using_extern_var() {
    eval! {
        extern let two = 2;
    }

    eval! {
        use two;
        extern fn double_me(x) {
            x * two
        }
    }

    let res = eval! {
        use double_me;
        double_me(1)
    };

    assert_eq!(res, 2);
}

#[test]
fn func_decl_using_extern_var_internally() {
    eval! {
        extern let two = 2;
    }

    eval! {
        extern fn double_me_2(x) {
            use two;
            x * two
        }
    }

    let res = eval! {
        use double_me_2;
        double_me_2(1)
    };

    assert_eq!(res, 2);
}

#[test]
fn use_as_expression_simple() {
    eval! {
        extern let my_value = 42;
    };

    let result = ::metamatch::eval! {
        use my_value;
        my_value
    };

    assert_eq!(result, 42);
}

#[test]
fn use_as_expression_direct() {
    eval! {
        extern let my_value = 42;
    };

    let result = eval! {
        use my_value
    };

    assert_eq!(result, 42);
}

#[test]
fn use_as_expression_in_arithmetic() {
    eval! {
        extern let my_value = 10;
    };

    let result = eval! {
        (use my_value) + 5
    };

    assert_eq!(result, 15);
}
