use metamatch::unquote;

#[test]
fn basic() {
    assert_eq!(unquote!(1 + 1), 2);
    assert_eq!(unquote!(1 - 1), 0);
    assert_eq!(unquote!(2 * 3), 6);
    assert_eq!(unquote!(3 / 2), 1);
}
