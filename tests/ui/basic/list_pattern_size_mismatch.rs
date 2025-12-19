use metamatch::eval;

// List pattern with wrong number of elements
fn main() {
    eval! {
        let [a, b] = [1, 2, 3, 4];
    };
}
