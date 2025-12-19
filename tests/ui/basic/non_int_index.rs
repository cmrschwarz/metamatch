use metamatch::eval;

fn main() {
    eval! {
        let x = [1, 2, 3];
        x["hello"]
    };
}
