use metamatch::{quote, unquote};

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

    let result = unquote! {
        "asdf".bytes().map(|x| x)
    };
    assert_eq!(&result, &*"asdf".bytes().collect::<Vec<_>>());

    let result = unquote! {
        "asdf".chars().map(|x| x)
    };
    assert_eq!(result, ['a', 's', 'd', 'f']);
}

#[test]
fn ident() {
    quote! {
        let [< ident("foo" + "bar") >] = 42;
    }

    assert_eq!(foobar, 42);
}

#[test]
fn str() {
    let x: &str = unquote!(str('x'));

    assert_eq!(x, "x");
}
