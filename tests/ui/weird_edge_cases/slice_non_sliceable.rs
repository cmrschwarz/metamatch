use metamatch::eval;

// Try to slice a non-sliceable type
fn main() {
    eval! {
        42[0..2]
    };
}
