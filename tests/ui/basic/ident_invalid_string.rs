use metamatch::eval;

// ident() with invalid string
fn main() {
    eval! {
        ident("123invalid")
    };
}
