use metamatch::eval;

#[test]
fn basic() {
    #![allow(clippy::bool_assert_comparison)]

    assert_eq!(eval!(1 + 1), 2);
    assert_eq!(eval!(1 - 1), 0);
    assert_eq!(eval!(2 * 3), 6);
    assert_eq!(eval!(3 / 2), 1);
    assert_eq!(eval!(7 % 2), 1);
    assert_eq!(eval!(1 | 2), 3);
    assert_eq!(eval!(7 & 3), 3);
    assert_eq!(eval!(1 << 10), 1024);
    assert_eq!(eval!(8 >> 1), 4);

    assert_eq!(eval!(7 > 2), true);
    assert_eq!(eval!(7 > 7), false);
    assert_eq!(eval!(7 >= 7), true);
    assert_eq!(eval!(7 > 8), false);

    assert_eq!(eval!(2 < 7), true);
    assert_eq!(eval!(7 < 7), false);
    assert_eq!(eval!(7 <= 7), true);
    assert_eq!(eval!(8 < 7), false);
}

#[test]
fn precedence() {
    assert_eq!(eval!(2 * 3 + 5), 11);
    assert_eq!(eval!(5 + 2 * 3), 11);
}

#[test]
fn parentheses_vs_tuple() {
    assert_eq!(eval!((1 + 2)), 3);
    assert_eq!(eval!((1 + 2,)), (3,));
    assert_eq!(eval!((1 + 2, 5)), (3, 5));
}

#[test]
fn unary_ops() {
    assert_eq!(eval!(!1), !1);
    assert_eq!(eval!(1 + -2), -1);
    assert_eq!(eval!(-(1 + 2)), -3);
    assert_eq!(eval!(1 + 2 - -5), 8);
}

#[test]
fn list_access() {
    let list = eval! {
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
    let list = eval! {
        let list = [0, 1, 2, 3];

        list[3] = 42;

        list
    };

    assert_eq!(list, [0, 1, 2, 42]);
}

#[test]
fn assignment() {
    let val = eval! {
        let mut x = 3;
        x = 4;
        x
    };
    assert_eq!(val, 4);
}

#[test]
fn add_assign() {
    let val = eval! {
        let mut x = 3;
        x += 4;
        x
    };
    assert_eq!(val, 7);
}

#[test]
fn string_concatenation() {
    let val = eval! {
        "foo" + "bar"
    };
    assert_eq!(val, "foobar");
}

#[test]
fn list_concatenation() {
    let val = eval! {
        [1, 2, 3] + [4, 5]
    };
    assert_eq!(val, [1, 2, 3, 4, 5]);
}
