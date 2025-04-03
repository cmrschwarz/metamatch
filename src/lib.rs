#![allow(clippy::cmp_owned)]
#![doc = include_str!("../README.md")]

use proc_macro::TokenStream;

mod ast;
mod evaluate;
mod macro_impls;
mod parse;

#[proc_macro]
pub fn unquote(body: TokenStream) -> TokenStream {
    macro_impls::unquote(body)
}

#[proc_macro]
pub fn quote(body: TokenStream) -> TokenStream {
    macro_impls::quote(body)
}

#[proc_macro_attribute]
pub fn replicate(attrib: TokenStream, body: TokenStream) -> TokenStream {
    macro_impls::replicate(attrib, body)
}

#[proc_macro]
pub fn metamatch(body: TokenStream) -> TokenStream {
    macro_impls::metamatch(body)
}
