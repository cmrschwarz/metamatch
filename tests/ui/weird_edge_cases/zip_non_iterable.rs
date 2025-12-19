// Test: zip with non-iterable arguments
use metamatch::template;
fn main() {
    template! {
        [<eval>]
        zip(42, [1, 2, 3])
        [</eval>]
    };
}
