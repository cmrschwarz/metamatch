use metamatch::eval;

// Slice with end before start
fn main() {
    eval! {
        let x = [1, 2, 3, 4, 5];
        x[3..1]
    };
}
