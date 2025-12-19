use metamatch::eval;

// Using super in a pattern where it doesn't make sense
fn main() {
    eval! {
        let super x = 5;
        x = 10;
    };
}
