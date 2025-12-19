use metamatch::eval;

// Nested if with wrong types at different levels
fn main() {
    eval! {
        if true {
            if "not a bool" {
                1
            } else {
                2
            }
        } else {
            3
        }
    };
}
