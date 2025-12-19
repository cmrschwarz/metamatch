use metamatch::eval;

// len() on a non-length type
fn main() {
    eval! {
        len(42)
    };
}
