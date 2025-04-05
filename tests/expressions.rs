use metamatch::unquote;

#[test]
fn basic() {
    assert_eq!(unquote!(1 + 1), 2);
    assert_eq!(unquote!(1 - 1), 0);
    assert_eq!(unquote!(2 * 3), 6);
    assert_eq!(unquote!(3 / 2), 1);
    assert_eq!(unquote!(7 % 2), 1);
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
