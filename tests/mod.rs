mod integration;

use metamatch::{eval, replicate, template};

#[test]
fn macro_errors() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/**/*.rs");
}
