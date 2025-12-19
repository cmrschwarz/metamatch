use metamatch::eval;

// Calling a function before it's declared
fn main() {
    eval! {
        let x = foo(1);
        fn foo(x) { x + 1 }
    };
}
