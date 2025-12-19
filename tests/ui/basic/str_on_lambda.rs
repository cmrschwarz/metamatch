use metamatch::eval;

// str() on a lambda (which can't be stringified)
fn main() {
    eval! {
        str(|x| x)
    };
}
