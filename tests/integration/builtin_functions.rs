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

#[test]
fn flatten() {
    // Flatten nested lists
    let result = eval! {
        flatten([[1, 2], [3, 4]])
    };
    assert_eq!(result, [1, 2, 3, 4]);

    // Flatten list of tuples
    let result = eval! {
        flatten([(1, 2), (3, 4)])
    };
    assert_eq!(result, [1, 2, 3, 4]);

    // Flatten with UFCS
    let result = eval! {
        [[1], [2, 3], [4, 5, 6]].flatten()
    };
    assert_eq!(result, [1, 2, 3, 4, 5, 6]);
}

#[test]
fn combinations() {
    // Basic 2-way combinations (cartesian product)
    let result = eval! {
        combinations([1, 2], [3, 4])
    };
    assert_eq!(result, [(1, 3), (1, 4), (2, 3), (2, 4)]);

    // 3-way combinations
    let result = eval! {
        combinations(['a', 'b'], ['c'], ['d', 'e'])
    };
    assert_eq!(result, [
        ('a', 'c', 'd'), ('a', 'c', 'e'),
        ('b', 'c', 'd'), ('b', 'c', 'e')
    ]);

    // With UFCS on first argument
    let result = eval! {
        [1, 2].combinations([10, 20])
    };
    assert_eq!(result, [(1, 10), (1, 20), (2, 10), (2, 20)]);
}
