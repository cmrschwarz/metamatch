use metamatch::eval;

// Using mut pattern on immutable binding then trying to mutate
fn main() {
    eval! {
        let (a, b) = (1, 2);
        a = 10;
    };
}
