use metamatch::{eval, template};

#[test]
fn map() {
    let (a, b, c) = (1, 2, 3);

    let result = eval! {
        [A, B, C].map(lowercase)
    };
    assert_eq!(result, [1, 2, 3]);

    let result = eval! {
        (A, B, C).map(lowercase)
    };
    assert_eq!(result, [1, 2, 3]);

    let result = eval! {
        "asdf".bytes().map(|x| x)
    };
    assert_eq!(&result, &*"asdf".bytes().collect::<Vec<_>>());

    let result = eval! {
        "asdf".chars().map(|x| x)
    };
    assert_eq!(result, ['a', 's', 'd', 'f']);
}

#[test]
fn ident() {
    template! {
        let [< ident("foo" + "bar") >] = 42;
    }

    assert_eq!(foobar, 42);
}

#[test]
fn str() {
    let x: &str = eval!(str('x'));
    assert_eq!(x, "x");

    let y: &str = eval!(str("y"));
    assert_eq!(y, "y");

    let z: &str = eval!(str(z));
    assert_eq!(z, "z");
}
