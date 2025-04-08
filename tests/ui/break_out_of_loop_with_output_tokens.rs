use metamatch::unquote;

fn main() {
    let res = unquote! {
        let mut X = 0;
        let super res = loop {
            X += 1;
            if X == 10 {
                break X;
            }
            quote!(X);
        };
        quote!(res);
    };
    assert_eq!(res, 10);
}
