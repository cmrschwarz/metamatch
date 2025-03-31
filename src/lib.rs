#![allow(clippy::cmp_owned)]
#![doc = include_str!("../README.md")]

use proc_macro::TokenStream;

mod implementation;

#[proc_macro]
pub fn eval(body: TokenStream) -> TokenStream {
    implementation::eval(body)
}

#[proc_macro]
pub fn template(body: TokenStream) -> TokenStream {
    implementation::template(body)
}
