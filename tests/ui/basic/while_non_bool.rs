use metamatch::eval;

// while with non-bool condition
fn main() {
    eval! {
        while "hello" {
            break;
        }
    };
}
