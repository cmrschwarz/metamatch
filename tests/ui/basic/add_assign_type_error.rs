use metamatch::eval;

// Using += on incompatible types
fn main() {
    eval! {
        let mut x = 5;
        x += "hello";
    };
}
