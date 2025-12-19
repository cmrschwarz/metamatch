use metamatch::eval;

fn main() {
    eval! {
        assert_eq!(1, 2)
    };
}
