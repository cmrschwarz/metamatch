#![allow(clippy::cmp_owned)]
#![doc = include_str!("../README.md")]

use proc_macro::TokenStream;

mod implementation;

#[proc_macro]
pub fn unquote(body: TokenStream) -> TokenStream {
    implementation::unquote(body)
}

#[proc_macro]
pub fn quote(body: TokenStream) -> TokenStream {
    implementation::quote(body)
}
