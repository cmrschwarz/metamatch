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
