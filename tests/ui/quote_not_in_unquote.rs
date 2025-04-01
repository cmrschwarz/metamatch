use metamatch::quote;

fn main() {
    let res = quote! {
        [<quote>]
        42
        [</quote>]
    };

    assert_eq!(res, 10);
}
