use metamatch::eval;

fn main() {
    eval! {
        {
            extern let FOO = [1, 2, 3];
        };
        INT_VARIANTS
    };
}
