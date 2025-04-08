use metamatch::unquote;
fn main() {
    unquote! {
        (let foo = 3) + 3
    }
}
