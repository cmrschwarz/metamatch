use metamatch::eval;
fn main() {
    eval! {
        (let foo = 3) + 3
    }
}
