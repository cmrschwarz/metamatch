use metamatch::eval;

// Assign to a list element that's out of bounds
fn main() {
    eval! {
        let mut x = [1, 2, 3];
        x[10] = 5;
    };
}
