use metamatch::unquote;

#[test]
fn basic() {
    #![allow(clippy::bool_assert_comparison)]

    assert_eq!(unquote!(1 + 1), 2);
    assert_eq!(unquote!(1 - 1), 0);
    assert_eq!(unquote!(2 * 3), 6);
    assert_eq!(unquote!(3 / 2), 1);
    assert_eq!(unquote!(7 % 2), 1);
    assert_eq!(unquote!(1 | 2), 3);
    assert_eq!(unquote!(7 & 3), 3);
    assert_eq!(unquote!(1 << 10), 1024);
    assert_eq!(unquote!(8 >> 1), 4);

    assert_eq!(unquote!(7 > 2), true);
    assert_eq!(unquote!(7 > 7), false);
    assert_eq!(unquote!(7 >= 7), true);
    assert_eq!(unquote!(7 > 8), false);

    assert_eq!(unquote!(2 < 7), true);
    assert_eq!(unquote!(7 < 7), false);
    assert_eq!(unquote!(7 <= 7), true);
    assert_eq!(unquote!(8 < 7), false);
}

#[test]
fn precedence() {
    assert_eq!(unquote!(2 * 3 + 5), 11);
    assert_eq!(unquote!(5 + 2 * 3), 11);
}

#[test]
fn parentheses_vs_tuple() {
    assert_eq!(unquote!((1 + 2)), 3);
    assert_eq!(unquote!((1 + 2,)), (3,));
    assert_eq!(unquote!((1 + 2, 5)), (3, 5));
}

#[test]
fn unary_ops() {
    assert_eq!(unquote!(!1), !1);
    assert_eq!(unquote!(1 + -2), -1);
    assert_eq!(unquote!(-(1 + 2)), -3);
    assert_eq!(unquote!(1 + 2 - -5), 8);
}

#[test]
fn list_access() {
    let list = unquote! {
        let list = [1,2,3];
        let x0 = list[0];
        let x1 = list[1];
        let x2 = list[2];
        [x0,x1,x2]
    };

    assert_eq!(list, [1, 2, 3]);
}

#[test]
fn list_assignment() {
    let list = unquote! {
        let list = [0, 1, 2, 3];

        list[3] = 42;

        list
    };

    assert_eq!(list, [0, 1, 2, 42]);
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
fn map() {
    let (a, b, c) = (1, 2, 3);

    let result = unquote! {
        [A, B, C].map(lowercase)
    };
    assert_eq!(result, [1, 2, 3]);

    let result = unquote! {
        (A, B, C).map(lowercase)
    };
    assert_eq!(result, [1, 2, 3]);
}
