use metamatch::eval;
fn main() {
    eval! {
        (fn foo() {}) + 3
    }
}
