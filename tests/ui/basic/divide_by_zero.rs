use metamatch::eval;

fn main() {
    eval! {
        1 / 0
    };
}
