use metamatch::unquote;

fn main() {
    unquote! {
        "asdf".map(|x|x)
    }
}
