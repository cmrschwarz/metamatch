#![allow(clippy::cmp_owned)]
#![doc = include_str!("../README.md")]

use proc_macro::TokenStream;

mod implementation;

/// A proc-macro for generating repetitive match arms.
///
/// # Basic Example
/// ```
/// use metamatch::metamatch;
/// enum MyEnum {
///     A(i8),
///     B(i16),
///     C(i32),
/// }
///
/// let mut double_me = MyEnum::A(42);
///
/// metamatch!(match &mut double_me {
///     #[expand( T in [A, B, C] )]
///     MyEnum::T(v) => *v *= 2,
/// })
/// ```
///
/// # Multiple Replacement Expressions
/// ```
/// use metamatch::metamatch;
/// enum MyEnum {
///     A(i8),
///     B(i16),
///     C(i32),
/// }
///
/// let mut foo = MyEnum::A(42);
///
/// let upcast = metamatch!(match &mut foo {
///     #[expand( (SRC, TGT, TY) in [
///         (A, B, i16), (B, C, i32), (C, C, i32)
///     ])]
///     MyEnum::SRC(v) => MyEnum::TGT(*v as TY),
/// });
/// ```
///
/// # Matrix Expansion
/// ```
/// use metamatch::metamatch;
/// enum MyEnum {
///     A(i8),
///     B(i16),
///     C(i32),
/// }
///
/// let mut foo = MyEnum::A(42);
/// let mut bar = MyEnum::B(42);
///
/// let same_value = metamatch!(match (foo, bar) {
///     #[expand(for (LHS, RHS) in matrix(
///         [A, B, C],
///         [A, B, C],
///     ))]
///     (MyEnum::LHS(lhs), MyEnum::RHS(rhs)) => {
///         (lhs as i64) == (rhs as i64)
///     }
/// });
/// assert!(same_value);
/// ```
#[proc_macro]
pub fn metamatch(body: TokenStream) -> TokenStream {
    implementation::metamatch(body)
}

/// An attribute styled proc-macro with similar syntax to [`metamatch!`]..
///
/// # Example
///
/// ```
/// use metamatch::replicate;
///
/// struct NodeIdx(usize);
///
/// #[replicate(for (TRAIT, FN) in [
///     (Add, add),
///     (Sub, sub),
///     (Mul, mul),
///     (Div, div),
///     (Rem, rem),
/// ])]
/// impl core::ops::TRAIT for NodeIdx {
///     type Output = Self;
///     fn FN(self, other: Self) -> Self {
///         NodeIdx(self.0.FN(other.0))
///     }
/// }
/// ```
#[proc_macro_attribute]
pub fn replicate(attr: TokenStream, body: TokenStream) -> TokenStream {
    implementation::replicate(attr, body)
}

/// A generalized version [`metamatch!`] for arbitrary expressions.
///
/// # Basic example
///
/// ```
/// use metamatch::expand;
///
/// let mut x = 0;
///
/// expand!(for T in [1, 2, 3] {
///     x += T; // 'loop unrolling'
/// });
///
/// assert_eq!(x, 6);
/// ```
///
/// # Create Multidimensional arrays
/// ```
/// use metamatch::expand;
///
/// // really annoying to do with macro_rules!
/// let arr: [[i32; 2]; 9] = expand! {
///     for (X, Y) in matrix([1, 2, 3], [1, 2, 3]) [
///         [X, Y],
///     ]
/// };
/// ```
#[proc_macro]
pub fn expand(body: TokenStream) -> TokenStream {
    implementation::expand(body)
}
