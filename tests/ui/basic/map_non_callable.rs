use metamatch::eval;

// map() with wrong callable type in second argument
// BUG: the error message says "args[0]" instead of "args[1]"
fn main() {
    eval! {
        [1,2,3].map(42)
    };
}
