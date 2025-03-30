#![allow(clippy::cmp_owned)]
#![doc = include_str!("../README.md")]

use proc_macro::TokenStream;

mod implementation;

#[proc_macro]
pub fn expand(body: TokenStream) -> TokenStream {
    implementation::expand(body)
}
