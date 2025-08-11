use std::io::Read;

use metamatch_playground::{
    eval_strs_to_tt, pretty_print_token_stream, MacroKind,
};

use clap::Parser;

#[derive(Parser)]
struct Args {
    #[arg(long, default_value = "false")]
    print_ast: bool,

    #[arg(long, default_value = "false")]
    body_stdin: bool,

    #[arg(value_enum)]
    macro_kind: Option<MacroKind>,
}

fn main() {
    let args = Args::parse();

    let kind = args.macro_kind.unwrap_or(MacroKind::Template);

    let sandbox = format!("{}/sandbox", env!("CARGO_MANIFEST_DIR"));

    let attrib = format!("{sandbox}/attrib.rs");

    let body_path = format!("{sandbox}/body.rs");

    if !std::fs::exists(&body_path).unwrap() {
        std::fs::write(
            &body_path,
            "// Macro body here.\n// This file is intentionally ignored by git.\n\n"
        ).unwrap();
    }

    if !std::fs::exists(&attrib).unwrap() {
        std::fs::write(
            &attrib,
            "// Attribute contents here.\n// This file is intentionally ignored by git.\n\n"
        ).unwrap();
    }

    let body_str = if args.body_stdin {
        let mut res = String::new();
        std::io::stdin()
            .lock()
            .read_to_string(&mut res)
            .expect("failed to stringify stdin as body");
        res
    } else {
        std::fs::read_to_string(&body_path)
            .expect("failed to stringify playground_body.rs")
    };

    let mut attrib_str = None;

    if let MacroKind::Replicate = kind {
        attrib_str = Some(
            std::fs::read_to_string(&attrib)
                .unwrap_or_else(|_| panic!("failed to stringify {attrib}")),
        );
    }

    let result = eval_strs_to_tt(kind, attrib_str, body_str, args.print_ast);

    println!("{}", pretty_print_token_stream(result));
}
