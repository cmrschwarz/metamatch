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
    assert_eq!(
        result,
        [
            ('a', 'c', 'd'),
            ('a', 'c', 'e'),
            ('b', 'c', 'd'),
            ('b', 'c', 'e')
        ]
    );

    // With UFCS on first argument
    let result = eval! {
        [1, 2].combinations([10, 20])
    };
    assert_eq!(result, [(1, 10), (1, 20), (2, 10), (2, 20)]);
}

#[test]
fn assert_macro() {
    // Basic assert with true condition
    eval! {
        assert!(true);
    };

    // Assert with expression
    eval! {
        assert!(1 + 1 == 2);
    };

    // Assert with lists
    eval! {
        assert!([1, 2, 3] == [1, 2, 3]);
    };

    // Assert with tuples
    eval! {
        assert!((1, "a") == (1, "a"));
    };

    // Assert with token list comparison
    eval! {
        let x = raw!(hello world);
        let y = raw!(hello world);
        assert!(x == y);
    };

    // Assert with strings
    eval! {
        assert!("hello" == "hello");
    };

    // Assert with format message containing expressions
    eval! {
        let x = 5;
        let y = 5;
        assert!(x == y, "expected {x} to equal {y}");
    };

    // Assert with computed format message
    eval! {
        let name = "test";
        let value = 42;
        assert!(value > 0, "value for {name} should be positive, got {value}");
    };
}

#[test]
fn equality_same_types() {
    // Integers
    let result: bool = eval!(1 == 1);
    assert!(result);
    let result: bool = eval!(1 == 2);
    assert!(!result);

    // Floats
    let result: bool = eval!(1.5 == 1.5);
    assert!(result);

    // Booleans
    let result: bool = eval!(true == true);
    assert!(result);
    let result: bool = eval!(true == false);
    assert!(!result);

    // Characters
    let result: bool = eval!('a' == 'a');
    assert!(result);
    let result: bool = eval!('a' == 'b');
    assert!(!result);

    // Strings
    let result: bool = eval!("hello" == "hello");
    assert!(result);
    let result: bool = eval!("hello" == "world");
    assert!(!result);

    // Lists
    let result: bool = eval!([1, 2, 3] == [1, 2, 3]);
    assert!(result);
    let result: bool = eval!([1, 2, 3] == [1, 2, 4]);
    assert!(!result);
    let result: bool = eval!([1, 2] == [1, 2, 3]);
    assert!(!result);

    // Tuples
    let result: bool = eval!((1, 2) == (1, 2));
    assert!(result);
    let result: bool = eval!((1, 2) == (1, 3));
    assert!(!result);

    // Nested structures
    let result: bool = eval!([(1, 2), (3, 4)] == [(1, 2), (3, 4)]);
    assert!(result);

    // Tokens
    let result: bool = eval!(raw!(foo bar) == raw!(foo bar));
    assert!(result);
    let result: bool = eval!(raw!(foo) == raw!(bar));
    assert!(!result);

    // Ranges
    let result: bool = eval!((1..5) == (1..5));
    assert!(result);
    let result: bool = eval!((1..=5) == (1..=5));
    assert!(result);
    let result: bool = eval!((1..5) == (1..6));
    assert!(!result);
}

#[test]
fn eq_same_span_fn() {
    // Same value, same span (from same raw block)
    eval! {
        let x = raw!(hello);
        assert!(eq_same_span(x, x));
    };

    // Same value content but different sources
    eval! {
        let x = raw!(hello);
        let y = raw!(hello);
        // Different spans, so should be false
        assert!(!eq_same_span(x, y));
    };

    // Regular equality ignores spans
    eval! {
        let x = raw!(hello);
        let y = raw!(hello);
        assert!(x == y);
    };
}

#[test]
fn format_macro() {
    // Basic string
    let result: &str = eval!(format!("hello"));
    assert_eq!(result, "hello");

    // Single inline expression
    let result: &str = eval!(format!("hello {\"world\"}"));
    assert_eq!(result, "hello world");

    // Multiple inline expressions
    let result: &str = eval!(format!("{1} + {2} = {1 + 2}"));
    assert_eq!(result, "1 + 2 = 3");

    // Escaped braces
    let result: &str = eval!(format!("{{}}"));
    assert_eq!(result, "{}");

    // Mixed escapes and expressions
    let result: &str = eval!(format!("{{{\"x\"}}} = {42}"));
    assert_eq!(result, "{x} = 42");

    // Different types
    let result: &str =
        eval!(format!("bool: {true}, char: {'a'}, float: {3.14}"));
    assert_eq!(result, "bool: true, char: a, float: 3.14");

    // Variable references
    eval! {
        let x = "hello";
        let y = "world";
        assert!(format!("{x} {y}") == "hello world");
    };

    // Complex expressions
    eval! {
        let nums = [1, 2, 3];
        assert!(format!("len = {len(nums)}") == "len = 3");
    };
}
