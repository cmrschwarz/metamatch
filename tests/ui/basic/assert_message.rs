use metamatch::eval;

fn main() {
    eval! {
        assert!(false, "lul: {21*2}")
    };
}
