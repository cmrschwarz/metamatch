use metamatch::eval;

fn main() {
    eval! {
        let x = 5;
        assert_eq!(x, 10, "x should be 10 but got {x}")
    };
}
