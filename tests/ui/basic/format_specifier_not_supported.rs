use metamatch::eval;

fn main() {
    // Format specifiers are not supported
    let _ = eval!(format!("{x:?}"));
}
