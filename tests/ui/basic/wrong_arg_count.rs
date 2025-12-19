use metamatch::eval;

fn main() {
    eval! {
        fn foo(x) { x }
        foo(1, 2, 3)
    };
}
