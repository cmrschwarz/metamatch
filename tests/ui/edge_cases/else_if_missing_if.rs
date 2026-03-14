use metamatch::template;

fn main() {
    let res = template! {
        [<if 3 > 2>]
        true
        // missing `if` below.
        // the current error for this is not ideal but we certainly
        // don't want it do regress further
        [<else x > 2>]
        false
        [</if>]
    };

    assert!(res);
}
