use metamatch::eval;

// Chained method calls on wrong types
fn main() {
    eval! {
        [1, 2, 3].enumerate().map(42)
    };
}
