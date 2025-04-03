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

#[proc_macro_attribute]
pub fn replicate(attrib: TokenStream, body: TokenStream) -> TokenStream {
    implementation::replicate(attrib, body)
}

#[proc_macro]
pub fn metamatch(body: TokenStream) -> TokenStream {
    implementation::metamatch(body)
}
