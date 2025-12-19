// Test: zip with mismatched lengths
use metamatch::template;
fn main() {
    template! {
        [<eval>]
        for pair in zip([1, 2, 3], [4, 5]) { }
        [</eval>]
    };
}
