use metamatch::eval;

fn main() {
    let res = eval! {
        [<eval>]
        42
        [</eval>]
    };

    assert_eq!(res, 10);
}
