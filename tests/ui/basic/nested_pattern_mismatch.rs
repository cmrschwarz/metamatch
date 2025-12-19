use metamatch::eval;

// Nested pattern mismatch in a complex tuple
fn main() {
    eval! {
        let ((a, b), c) = ((1, 2, 3), 4);
    };
}
