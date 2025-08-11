// Include the metamatch implementation files directly.
// We do it this way to get the proc macros accessible as an
// fn (TokenStream) -> TokenStream;
// without messing with the cargo toml of the main crate.
// This is convenient for debugging.
mod metamatch_impl {
    // NOTE: This file has a different MSRV so many of the lints become
    // incorrect. We also already lint it when using it through metamatch
    // directly.
    #![allow(clippy::all, clippy::pedantic)]

    pub mod ast {
        use proc_macro2 as proc_macro;
        include!("../../src/ast.rs");
    }
    pub mod parse {
        use proc_macro2 as proc_macro;
        include!("../../src/parse.rs");
    }
    pub mod evaluate {
        use proc_macro2 as proc_macro;
        include!("../../src/evaluate.rs");
    }
    pub mod macro_impls {
        use proc_macro2 as proc_macro;
        include!("../../src/macro_impls.rs");
    }
}

use metamatch_impl::macro_impls::IntoVec;

use clap::ValueEnum;
use proc_macro2::{Span, TokenStream};

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

#[derive(Default, ValueEnum, Clone)]
pub enum MacroKind {
    #[default]
    Template,
    Eval,
    Metamatch,
    Replicate,
}

pub fn eval_strs_to_tt(
    kind: MacroKind,
    attr: Option<String>,
    body: String,
    debug_print_ast: bool,
) -> TokenStream {
    let body_tt = syn::parse_str::<TokenStream>(&body)
        .expect("failed to parse playground_body.rs");

    let mut ctx = metamatch_impl::ast::Context::default();

    let exprs = match kind {
        MacroKind::Template => metamatch_impl::macro_impls::parse_template(
            &mut ctx,
            body_tt.into_vec(),
        ),
        MacroKind::Eval => metamatch_impl::macro_impls::parse_eval(
            &mut ctx,
            body_tt.into_vec(),
        ),
        MacroKind::Metamatch => metamatch_impl::macro_impls::parse_metamatch(
            &mut ctx,
            body_tt.into_vec(),
        ),
        MacroKind::Replicate => {
            let attrib_tt = syn::parse_str::<TokenStream>(&attr.unwrap())
                .unwrap_or_else(|_| panic!("failed to stringify {body}"));
            metamatch_impl::macro_impls::parse_replicate(
                &mut ctx,
                attrib_tt.into_vec(),
                body_tt.into_vec(),
            )
        }
    };

    if debug_print_ast && ctx.errors.is_empty() {
        println!("{exprs:#?}");
    }

    ctx.eval_to_token_stream(Span::call_site(), &exprs)
}

pub fn eval_strs_to_str(
    kind: MacroKind,
    attr: Option<String>,
    body: String,
) -> String {
    let res = eval_strs_to_tt(kind, attr, body, false);
    pretty_print_token_stream(res)
}

pub fn eval_to_str(body: impl Into<String>) -> String {
    let res = eval_strs_to_tt(MacroKind::Eval, None, body.into(), false);
    pretty_print_token_stream(res)
}
