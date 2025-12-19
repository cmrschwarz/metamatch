use metamatch::eval;

// Mutating list while iterating (should ideally error or have defined
// behavior)
fn main() {
    eval! {
        let mut x = [1, 2, 3];
        for i in x {
            x.push(i);
        }
    };
}
