use metamatch::eval;

// bytes() on a non-string
fn main() {
    eval! {
        [1, 2, 3].bytes()
    };
}
