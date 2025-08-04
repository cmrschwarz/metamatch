#![doc = include_str!("../README.md")]

use proc_macro::TokenStream;

mod ast;
mod evaluate;
mod macro_impls;
mod parse;

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
/// #[metamatch::replicate(
///    let traits = [Add, Sub, Mul, Div, Rem];
///    let trait_fns = traits.map(lowercase);
///    for (TRAIT, FN) in zip(traits, trait_fns)
/// )]
/// impl std::ops::TRAIT for NodeIdx {
///     type Output = Self;
///     fn FN(self, rhs: Self) -> Self {
///         NodeIdx(self.0.FN(rhs.0))
///     }
/// }
/// assert!(NodeIdx(1) + NodeIdx(2) == NodeIdx(3));
/// ```
#[proc_macro_attribute]
pub fn replicate(attrib: TokenStream, body: TokenStream) -> TokenStream {
    macro_impls::replicate(attrib, body)
}

/// Evaluates arbitrary expressions.
///
/// See this crate's root for a basic [language documentation](`crate`).
///
/// ## Example
/// ```rust
/// use metamatch::eval;
///
/// const ARRAY: [i32; 4] = eval! {
///     let ELEMENTS = for X in 1..5 {
///         quote!(X,)
///     };
///     quote!([ELEMENTS])
/// };
/// assert_eq!(ARRAY, [1, 2, 3, 4]);
/// ```
#[proc_macro]
pub fn eval(body: TokenStream) -> TokenStream {
    macro_impls::eval(body)
}

/// Embed dynamic chunks into a larger body of Rust source.
///
/// Template tags `[< ... >]` can be used to construct dynamic elements.
///
/// See this crate's root for a basic [language documentation](`crate`).
///
/// ## Example
/// ```rust
/// use metamatch::template;
///
/// template! {
///     enum ErrorCode {
///         [<for err_id in 0..=42>]
///             [<ident("E" + str(err_id))>](String),
///         [</for>]
///     }
/// };
/// let err = ErrorCode::E42("oh noes!".to_owned());
/// ```
///
/// ## Template blocks
/// While template tags can contain arbitrarily complex expressions,
/// template blocks are usually preferred for readability.
///
/// - `[<for ..>] [</for>]`
/// - `[<while ..>] [</while>]`
/// - `[<while let .. = ..>] [</while>]`
/// - `[<loop>] [</loop>]`
/// - `[<if ..>] [<else if ..>] [<else>] [</if>]`
/// - `[<fn .. (..)>] [</fn>]`
///
/// ## Special blocks
/// - `[<eval>][</eval>]`: Larger blocks of dynamic code can be put inside an
///   `eval` block.
/// - `[<raw>]..[</raw>]`: Paste raw Rust without any identifier replacements.
///   Useful e.g. when metamatch is combined with `macro_rules!`.
#[proc_macro]
pub fn template(body: TokenStream) -> TokenStream {
    macro_impls::template(body)
}
