use metamatch::metamatch;

fn main() {
    let v = "42";
    metamatch!(match v {
        #[expand(for FOO in [v.as_bytes()])],
        "42" => FOO,
        _ => b""
    });
}
