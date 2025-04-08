use metamatch::unquote;
fn main() {
    unquote! {
        (fn foo() {}) + 3
    }
}
