use metamatch::metamatch;

fn main() {
    let x = 42;
    metamatch! {
        match f {
            #[expand(for (A, B) in [(1, 2), (3)])]
            A => A + B,
            _ => (),
        }
    }
}
