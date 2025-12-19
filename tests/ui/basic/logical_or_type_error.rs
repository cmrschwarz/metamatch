use metamatch::eval;

// Logical OR with non-bool
fn main() {
    eval! {
        false || 42
    };
}
