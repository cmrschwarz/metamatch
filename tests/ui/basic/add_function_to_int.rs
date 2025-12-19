use metamatch::eval;

// Trying to add a function to a number
fn main() {
    eval! {
        fn foo() { 1 }
        foo + 5
    };
}
