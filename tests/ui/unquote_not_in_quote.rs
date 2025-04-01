use metamatch::unquote;

fn main() {
    let res = unquote! {
        [<unquote>]
        42
        [</unquote>]
    };

    assert_eq!(res, 10);
}
