//! # `metamatch!`
//!
//! A zero dependency proc-macro for practical metaprogramming.
//!
//! Macro expressions using familiar Rust syntax, evaluated by a tiny
//! interpreter.
//!
//! ## Supported Rust Syntax:
//! - `let` statements and pattern matching.
//! - `loop`, `while`, `while let`, and `for` loops, including `continue` and
//!   `break`
//! - `if` and `else` blocks
//! - Functions (`fn`)  and lambdas (`|..|`)
//! - Arrays (`[1,2,3]`)
//! - Ranges (`0..=10`)
//! - All expression operators like `+` or `<<`.
//!
//! ## Not supported:
//! - `struct`, `enum`, `type`, `trait`, generics, ...
//!
//! ## Builtin functions:
//! - `lowercase(str) -> str`
//! - `uppercase(str) -> str`
//! - `capitalize(str) -> str`
//! - `enumerate([T]) -> [(int, T)]`
//! - `zip([A], [B], ..) -> [(A, B, ..)]`
//! - `map([T], Fn(T) -> U) -> [U]`
//! - `chars(str) -> [char]`
//! - `bytes(str) -> [int]`
//! - `ident(str) -> token`
//! - `str(any) -> str`
//! - `len([T]) -> int`
//!
//! String functions also work on tokens.
//!
//! All functions support UFCS, so `[1,2,3].len() == len([1,2,3])`
//!
//! ## Special Purpose Macros:
//! - `quote! {..} -> [token]`: nested version of [`quote!`].
//! - `raw! {..} -> [token]`: raw Rust, no template tags or expanded meta
//!   variables
//!
//! Just like Rust macros, you can use any of `{}`, `[]`, and `()`
//! interchangably for the macro invocations.
use proc_macro::TokenStream;

mod ast;
mod evaluate;
mod macro_impls;
mod parse;

/// Generate repetitive syntax like trait impls without giving up on
/// `rustfmt` or rust-analyzer.
///
/// The last macro expression must be an unterminated block like `for` or
/// `while`.
///
/// See this crate's root for a basic [language documentation](`crate`).
///
///
/// ## Example
/// ```rust
/// use metamatch::replicate;
///
/// #[derive(PartialEq, Eq, PartialOrd, Ord)]
/// struct NodeIdx(usize);
///
/// #[replicate(
///     let traits = [Add, Sub, Mul, Div, Rem];
///     for (TRAIT, FUNC) in zip(traits, traits.map(lowercase))
/// )]
/// impl core::ops::TRAIT for NodeIdx {
///     type Output = Self;
///     fn FUNC(self, rhs: Self) -> Self {
///         NodeIdx(self.0.FUNC(rhs.0))
///     }
/// }
/// assert!(NodeIdx(1) + NodeIdx(2) == NodeIdx(3));
/// ```
#[proc_macro_attribute]
pub fn replicate(attrib: TokenStream, body: TokenStream) -> TokenStream {
    macro_impls::replicate(attrib, body)
}

/// Generate repetitive match arms for differently typed variants.
///
/// See this crate's root for a basic [language documentation](`crate`).
///
///
/// ## Example
/// ```rust
/// use metamatch::metamatch;
/// enum DynVec {
///     I8(Vec<i8>),
///     I16(Vec<i16>),
///     I32(Vec<i32>),
///     I64(Vec<i64>),
/// }
///
/// impl DynVec {
///     fn len(&self) -> usize {
///         metamatch!(match self {
///             #[expand(for X in [I8, I16, I32, I64])]
///             DynVec::X(v) => v.len(),
///         })
///     }
///     // v   expands into   v
///     fn len_(&self) -> usize {
///         match self {
///             DynVec::I8(v) => v.len(),
///             DynVec::I16(v) => v.len(),
///             DynVec::I32(v) => v.len(),
///             DynVec::I64(v) => v.len(),
///         }
///     }
/// }
/// ```
#[proc_macro]
pub fn metamatch(body: TokenStream) -> TokenStream {
    macro_impls::metamatch(body)
}

/// Evaluates arbitrary expressions.
///
/// See this crate's root for a basic [language documentation](`crate`).
///
/// ## Example
/// ```rust
/// use metamatch::unquote;
///
/// const ARRAY: [i32; 4] = unquote! {
///     let ELEMENTS = for X in 1..5 {
///         quote!(X,)
///     };
///     quote!([ELEMENTS])
/// };
/// assert_eq!(ARRAY, [1, 2, 3, 4]);
/// ```
#[proc_macro]
pub fn unquote(body: TokenStream) -> TokenStream {
    macro_impls::unquote(body)
}

/// Like [`unquote!`], but starts out in quoted mode, meaning that
/// tokens will be pasted verbatim.
///
/// Template tags `[< ... >]` can be used to construct dynamic elements.
///
/// See this crate's root for a basic [language documentation](`crate`).
///
/// ## Example
/// ```rust
/// use metamatch::quote;
///
/// quote! {
///     enum ErrorCode {
///         [<for err_id in 0..=42>]
///             [<ident("E" + str(err_id))>](String),
///         [</for>]
///     }
/// };
/// let err = ErrorCode::E42("oh noes!".to_owned());
/// ```
///
/// ## Supported template tags
/// - `[<for ..>] [</for>]`
/// - `[<while ..>] [</while>]`
/// - `[<while let ...>] [</while>]`
/// - `[<loop>] [</loop>]`
/// - `[<if ..>] [<else if ..>] [<else>] [</if>]`
/// - `[<fn .. (..)>] [</fn>]`
/// - `[<let .. = ..>]`;
/// - `[< ("arbitrary expressions") >]`
///
/// ## Special tags
/// - `[<unquote>][</unquote>]`: Evaluate expressions.
/// - `[<raw>]..[</raw>]`: Paste raw Rust without identifier replacements.
#[proc_macro]
pub fn quote(body: TokenStream) -> TokenStream {
    macro_impls::quote(body)
}
