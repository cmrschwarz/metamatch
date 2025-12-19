use metamatch::eval;

fn main() {
    eval! {
        for x in 42 {
            x
        }
    };
}
