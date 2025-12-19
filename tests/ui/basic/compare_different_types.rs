use metamatch::eval;

// Compare different types
fn main() {
    eval! {
        5 == "5"
    };
}
