// Test: method call on undefined variable
use metamatch::template;
fn main() {
    template! {
        [<eval>]
            undefined_var.len()
        [</eval>]
    };
}
