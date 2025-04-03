// Include the metamatch implementation file directly.
// We do it this way to get the proc macros accessible as an
// fn (TokenStream) -> TokenStream;
// without messing with the cargo toml of the main crate.
// This is convenient for debugging.
mod metamatch_impl {
    // NOTE: This file has a different MSRV so many of the lints become
    // incorrect. We also already lint it when using it through metamatch
    // directly.
    #![allow(clippy::all, clippy::pedantic)]
    use proc_macro2 as proc_macro;
    include!("../../src/implementation.rs");
}

use clap::Parser;
use proc_macro2::TokenStream;

pub fn pretty_print_token_stream(input: TokenStream) -> String {
    let input = input.to_string();

    // add main function around it so its a valid file...
    let code = format!("fn main() {{ {input}  }} ");

    let stx = match syn::parse_str::<syn::File>(&code) {
        Ok(stx) => stx,
        Err(err) => {
            return format!(
            "// pretty printing failed: \"{err}\"\n// raw output:\n\n{input}"
        )
        }
    };
    let pretty = prettyplease::unparse(&stx);

    // filter back out the main function wrapper...
    let mut pretty = pretty
        .lines()
        .skip(1)
        .map(|line| format!("{}\n", &line[4.min(line.len())..]))
        .collect::<String>();
    pretty.truncate(pretty.len().saturating_sub(2));

    pretty
}

#[derive(Default, clap::Subcommand)]
enum MacroKind {
    #[default]
    Quote,
    Unquote,
    Metamatch,
    Replicate,
}

#[derive(Parser)]
struct Args {
    #[command(subcommand)]
    subcommand: Option<MacroKind>,
}

fn main() {
    let args = Args::parse();

    let kind = args.subcommand.unwrap_or(MacroKind::Quote);

    let sandbox = format!("{}/sandbox", env!("CARGO_MANIFEST_DIR"));

    let attrib = format!("{sandbox}/attrib.rs");

    let body = format!("{sandbox}/body.rs");

    if !std::fs::exists(&body).unwrap() {
        std::fs::write(
            &body,
            "// Macro body here.\n// This file is intentionally ignored by git.\n\n"
        ).unwrap();
    }

    if !std::fs::exists(&attrib).unwrap() {
        std::fs::write(
            &attrib,
            "// Attribute contents here.\n// This file is intentionally ignored by git.\n\n"
        ).unwrap();
    }

    let body_str = std::fs::read_to_string(&body)
        .expect("failed to stringify playground_body.rs");
    let body_tt = syn::parse_str::<TokenStream>(&body_str)
        .expect("failed to parse playground_body.rs");

    let result = match kind {
        MacroKind::Quote => metamatch_impl::quote(body_tt),
        MacroKind::Unquote => metamatch_impl::unquote(body_tt),
        MacroKind::Metamatch => metamatch_impl::metamatch(body_tt),
        MacroKind::Replicate => {
            let attrib_str = std::fs::read_to_string(&attrib)
                .unwrap_or_else(|_| panic!("failed to stringify {attrib}"));
            let attrib_tt = syn::parse_str::<TokenStream>(&attrib_str)
                .unwrap_or_else(|_| panic!("failed to stringify {body}"));
            metamatch_impl::replicate(attrib_tt, body_tt)
        }
    };

    println!("{}", pretty_print_token_stream(result));
}
