use metamatch::eval;

fn main() {
    eval! {
        let x = 5;
        x()
    };
}
