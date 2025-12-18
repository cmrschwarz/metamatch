use metamatch_playground::eval_to_str;

#[track_caller]
fn assert_eval_output(input: &str, expected_output: &str) {
    let res = eval_to_str(input);
    assert_eq!(res.trim(), expected_output.trim());
}

#[test]
fn empty_raw_block_preserved() {
    assert_eval_output(
        r#"
extern let my_fns = #();
"#,
        r#"
macro_rules! my_fns {
    ($alias:ident, ($($chain:tt)*), [$($prefix:tt)*], $($rest:tt)*) => {
        $($chain)* { $($prefix)* let $alias = #(); $($rest)* }
    };
}"#,
    )
}
