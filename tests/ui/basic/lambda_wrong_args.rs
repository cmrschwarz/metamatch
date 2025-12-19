use metamatch::eval;

// Lambda with wrong number of parameters
fn main() {
    eval! {
        let f = |a, b| a + b;
        f(1)
    };
}
