#![doc = include_str!("../README.md")]
#![warn(clippy::pedantic)]

use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream,
    TokenTree,
};
use std::collections::HashMap;

/// Indexes into [`ExpansionVariant`]::indent_replacements.
type ExpansionIdentIndex = usize;

/// Replacement for a single identifier. Good old `$($any: tt)*`.
type Replacement = Vec<TokenTree>;

/// The `(1, 2)` is one `ExpansionVariant` of two in
/// `#[expand((X, Y) in [(1, 2), (3, 4)])]`.
/// It has one replacement per ident name.
#[derive(Debug, Default, Clone)]
struct ExpansionVariant {
    /// Indexed by [`ExpansionIdentIndex`].
    ident_replacements: Vec<Replacement>,
}

/// a single `#[expand]` from [`metamatch!`] or an [`expand!`] / [`replicate!`]
#[derive(Debug, Default)]
struct ExpandAttribute {
    ident_names: HashMap<String, ExpansionIdentIndex>,
    ident_tup: bool,
    expand_pattern: bool,
    variants: Vec<ExpansionVariant>,
    error_list: Vec<MetaError>,
}

#[derive(Debug)]
struct Substitution {
    pos: usize,
    kind: SubstitutionKind,
}

#[derive(Debug)]
struct MetaError {
    span: Span,
    message: String,
}

#[allow(unused)] // TODO
#[derive(Debug)]
enum MetaFnKind {
    Lower,
    Upper,
    Capitalize,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Raw,
    Concat,
    Seq,
}

#[allow(unused)] // TODO
#[derive(Debug)]
enum MetaExpr {
    Token(TokenTree),
    Ident(String),
    ExpansionIdent(ExpansionIdentIndex),
    FnCall {
        kind: MetaFnKind,
        args: Vec<MetaExpr>,
    },
}

#[derive(Debug)]
enum SubstitutionKind {
    InlineIdent(ExpansionIdentIndex),
    MetaExpr(MetaExpr),
    Nested(Vec<Substitution>),
}

#[derive(Debug)]
struct Template {
    offset: usize,
    substitutions: Vec<Substitution>,
    add_trailing_comma: bool,
    pattern_end: usize,
    body_end: usize,
}

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
///     #[expand( (LHS, RHS) in matrix(
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
    let mut tokens = Vec::from_iter(body);

    let mut tokens_iter = tokens.iter_mut();

    match tokens_iter.next() {
        Some(TokenTree::Ident(ident)) if ident.to_string() == "match" => {}
        other => {
            return compile_error(
                "metamatch! body must start with `match`",
                other.map_or_else(Span::call_site, |t| t.span()),
            );
        }
    };

    let mut match_body_found = false;

    for tt in tokens_iter {
        if let TokenTree::Group(group) = tt {
            if group.delimiter() != Delimiter::Brace {
                continue;
            }
            match process_match_body(group.stream()) {
                Err(err) => {
                    return compile_error(&err.message, err.span);
                }
                Ok(body) => {
                    let mut body_replacement =
                        Group::new(group.delimiter(), body);
                    body_replacement.set_span(group.span());
                    *group = body_replacement;
                    // we allow multiple bodies. something like
                    // `match {42} { x => x}` is legal Rust.
                    // we don't do the work to figure out the 'correct'
                    // match body, but just do each of them (non recursive
                    // though)
                    match_body_found = true;
                }
            };
        }
    }
    if !match_body_found {
        return compile_error(
            "missing match statement body for metamatch!",
            Span::call_site(),
        );
    }
    TokenStream::from_iter(tokens)
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
/// #[replicate( FN in [ add, sub, mul, div ])]
/// impl core::ops::[<capitalize(FN)>] for NodeIdx {
///     type Output = Self;
///     fn FN(self, other: Self) -> Self {
///         NodeIdx(self.0.FN(other.0))
///     }
/// }
/// ```

#[proc_macro_attribute]
pub fn replicate(attr: TokenStream, body: TokenStream) -> TokenStream {
    let mut expand_attr =
        parse_expand_attrib_inner(None, false, None, &mut attr.into_iter());

    let body = body.into_iter().collect::<Vec<_>>();
    let substitutions = collect_substitutions(&mut expand_attr, &body, 0);

    let template = Template {
        offset: 0,
        substitutions,
        add_trailing_comma: false,
        pattern_end: 0,
        body_end: body.len(),
    };

    let mut res = Vec::new();

    expand_full(&expand_attr, &body, &template, &mut res);

    TokenStream::from_iter(res)
}

/// A generalized version [`metamatch!`] for arbitrary expressions.
///
/// # Basic example
///
/// ```
/// use metamatch::expand;
///
///  let mut x = 0;
///
///  expand!(T in [1, 2, 3] {
///      x += T; // 'loop unrolling'
///  });
///
///  assert_eq!(x, 6);
/// ```
///
/// # Create Multidimensional arrays
/// ```
/// use metamatch::expand;
///
/// // macros cannot produce
/// let arr: [[i32; 2]; 9] = expand! {
///     (X, Y) in matrix([1, 2, 3], [1, 2, 3]) [
///         [X, Y],
///     ]
/// };
/// ```
#[proc_macro]
pub fn expand(body: TokenStream) -> TokenStream {
    let mut iter = body.into_iter();
    let mut expand_attr =
        parse_expand_attrib_inner(None, false, None, &mut iter);

    let (body, delim) = match parse_expand_expr_body(&mut iter) {
        Ok(v) => v,
        Err(e) => return compile_error(&e.message, e.span),
    };

    let substitutions = collect_substitutions(&mut expand_attr, &body, 0);

    let template = Template {
        offset: 0,
        substitutions,
        add_trailing_comma: false,
        pattern_end: 0,
        body_end: body.len(),
    };

    let mut res = Vec::new();

    expand_full(&expand_attr, &body, &template, &mut res);

    for err in expand_attr.error_list {
        res.extend(compile_error(&err.message, err.span));
    }

    let res = TokenStream::from_iter(res);

    if let Some(delim) = delim {
        let x = TokenStream::from_iter(std::iter::once(TokenTree::Group(
            Group::new(delim, res),
        )));
        x
    } else {
        res
    }
}

fn parse_expand_expr_body(
    iter: &mut impl Iterator<Item = TokenTree>,
) -> Result<(Vec<TokenTree>, Option<Delimiter>), MetaError> {
    let mut t = iter.next();
    let mut delim = None;
    let mut spread = false;

    let mut expected = "`{` or `*`";

    if let Some(TokenTree::Punct(p)) = &t {
        if p.as_char() == '*' {
            spread = true;
            t = iter.next();
            expected = "`{`";
        }
    }

    let group = match t {
        Some(TokenTree::Group(group)) => group,
        Some(t) => {
            return Err(MetaError {
                message: format!("expected {expected} to begin expand body"),
                span: t.span(),
            })
        }
        None => {
            return Err(MetaError {
                message: format!(
                "missing body for expand, expected {expected} after expansion group"
            ),
                span: Span::call_site(),
            })
        }
    };
    if let Some(t) = iter.next() {
        return Err(MetaError {
            message: "stray token after the end of expand expresssion"
                .to_string(),
            span: t.span(),
        });
    }
    if !spread {
        delim = Some(group.delimiter());
    }
    Ok((group.stream().into_iter().collect(), delim))
}

fn compile_error(message: &str, span: Span) -> TokenStream {
    TokenStream::from_iter(vec![
        TokenTree::Ident(Ident::new("compile_error", span)),
        TokenTree::Punct({
            let mut punct = Punct::new('!', Spacing::Alone);
            punct.set_span(span);
            punct
        }),
        TokenTree::Group({
            let mut group = Group::new(Delimiter::Brace, {
                TokenStream::from_iter(vec![TokenTree::Literal({
                    let mut string = Literal::string(&format!("{message}"));
                    string.set_span(span);
                    string
                })])
            });
            group.set_span(span);
            group
        }),
    ])
}

fn tok_span_or_parent_close(
    tok: &Option<TokenTree>,
    parent: Option<&Group>,
) -> Span {
    match (tok, parent) {
        (Some(t), _) => t.span(),
        (None, Some(parent)) => parent.span_close(),
        (None, None) => Span::call_site(),
    }
}

fn tok_span_open_or_parent_close(
    tok: Option<&TokenTree>,
    parent: Option<&Group>,
) -> Span {
    match (tok, parent) {
        (Some(TokenTree::Group(g)), _) => g.span_open(),
        (Some(other), _) => other.span(),
        (None, Some(parent)) => parent.span_close(),
        (None, None) => Span::call_site(),
    }
}

fn parse_comma_separated_token_tree_lists(
    group: &Group,
) -> Result<Vec<Vec<TokenTree>>, MetaError> {
    let mut variants = Vec::new();
    let mut curr_variant = Vec::new();
    for tt in group.stream() {
        if let TokenTree::Punct(p) = &tt {
            if p.as_char() == ',' {
                if curr_variant.is_empty() {
                    return Err(MetaError {
                        message:
                            "expansion variant must have at least one token"
                                .to_string(),
                        span: p.span(),
                    });
                }
                variants.push(curr_variant);
                curr_variant = Vec::new();
                continue;
            }
        }
        curr_variant.push(tt);
    }
    if !curr_variant.is_empty() {
        variants.push(curr_variant);
    }
    Ok(variants)
}

fn add_cross_product_variants<'a>(
    mut pending: ExpansionVariant,
    mut replacement_arrs: impl Clone + Iterator<Item = &'a Vec<Replacement>>,
    arr_idx: usize,
    variants: &mut Vec<ExpansionVariant>,
) {
    let Some(rep_arr) = replacement_arrs.next() else {
        variants.push(pending);
        return;
    };
    for rep in &rep_arr[0..rep_arr.len().saturating_sub(1)] {
        let mut p = pending.clone();
        p.ident_replacements.push(rep.clone());
        add_cross_product_variants(
            p,
            replacement_arrs.clone(),
            arr_idx + 1,
            variants,
        );
    }
    if let Some(rep) = rep_arr.last() {
        pending.ident_replacements.push(rep.clone());
        add_cross_product_variants(
            pending,
            replacement_arrs,
            arr_idx + 1,
            variants,
        );
    }
}

fn parse_expand_variants_arr_of_tup(
    expand_attrib: &mut ExpandAttribute,
    variants_group: &Group,
    cross_product: bool,
) {
    let ident_count = expand_attrib.ident_names.len();

    let mut arrs = Vec::new();
    if cross_product {
        arrs.extend(
            std::iter::repeat(Vec::<Replacement>::new()).take(ident_count),
        );
    }

    let mut group_iter = variants_group.stream().into_iter();
    loop {
        let group = match group_iter.next() {
            Some(TokenTree::Group(group))
                if group.delimiter() == Delimiter::Parenthesis =>
            {
                group
            }
            Some(other) => {
                expand_attrib.error_list.push(MetaError {
                    message: "expected `(` or `]`".to_string(),
                    span: other.span(),
                });
                return;
            }
            None => break,
        };
        let ident_replacements =
            match parse_comma_separated_token_tree_lists(&group) {
                Ok(r) => r,
                Err(err) => {
                    expand_attrib.error_list.push(err);
                    return;
                }
            };
        let replacements_len = ident_replacements.len();
        if replacements_len != ident_count {
            expand_attrib.error_list.push(MetaError {
                message: format!("expected {ident_count} replacements, got {replacements_len}",),
                span: group.span(),
            });
            return;
        }
        if cross_product {
            for (i, r) in ident_replacements.iter().enumerate() {
                arrs[i].push(r.clone());
            }
        } else {
            expand_attrib
                .variants
                .push(ExpansionVariant { ident_replacements });
        }
        match group_iter.next() {
            Some(TokenTree::Punct(p)) if p.as_char() == ',' => {
                continue;
            }
            Some(other) => {
                expand_attrib.error_list.push(MetaError {
                    message: "expected `,` or `]`".to_string(),
                    span: other.span(),
                });
                return;
            }
            None => break,
        }
    }
    if cross_product {
        add_cross_product_variants(
            ExpansionVariant::default(),
            arrs.iter(),
            0,
            &mut expand_attrib.variants,
        );
    }
}

fn parse_expand_variants_tup_of_arr(
    expand_attrib: &mut ExpandAttribute,
    variants_group: &Group,
    cross_product: bool,
) {
    struct ReplacementArr {
        group: Group,
        replacements: Vec<Vec<TokenTree>>,
    }
    let ident_count = expand_attrib.ident_names.len();

    let mut group_iter = variants_group.stream().into_iter();
    let mut arrs = Vec::new();
    loop {
        let group = match group_iter.next() {
            Some(TokenTree::Group(group))
                if group.delimiter() == Delimiter::Bracket =>
            {
                group
            }
            Some(other) => {
                expand_attrib.error_list.push(MetaError {
                    message: "expected `[` or `)`".to_string(),
                    span: other.span(),
                });
                return;
            }
            None => break,
        };
        match parse_comma_separated_token_tree_lists(&group) {
            Ok(replacements) => {
                arrs.push(ReplacementArr {
                    group,
                    replacements,
                });
            }
            Err(err) => expand_attrib.error_list.push(err),
        }

        match group_iter.next() {
            Some(TokenTree::Punct(p)) if p.as_char() == ',' => {
                continue;
            }
            Some(other) => {
                expand_attrib.error_list.push(MetaError {
                    message: "expected `,` or `]`".to_string(),
                    span: other.span(),
                });
                return;
            }
            None => break,
        }
    }
    let replacement_count = arrs.len();
    if replacement_count != ident_count {
        expand_attrib.error_list.push(MetaError {
            message: format!(
                "expected {ident_count} replacement arrays, got {replacement_count}",
            ),
            span: variants_group.span(),
        });
        return;
    }
    let len_0 = arrs[0].replacements.len();
    if cross_product {
        add_cross_product_variants(
            ExpansionVariant::default(),
            arrs.iter().map(|ra| &ra.replacements),
            0,
            &mut expand_attrib.variants,
        );
        return;
    }
    for a in arrs.iter().skip(1) {
        let len_i = a.replacements.len();
        if len_i != len_0 {
            expand_attrib.error_list.push(MetaError {
                    message: format!("replacement array length ({len_i}) differs from first ({len_0})"),
                    span: a.group.span(),
                });
            return;
        }
    }
    for i in 0..len_0 {
        let mut ident_replacements = Vec::new();
        for a in arrs.iter() {
            ident_replacements.push(a.replacements[i].clone());
        }
        expand_attrib
            .variants
            .push(ExpansionVariant { ident_replacements });
    }
}

fn parse_expand_ident_names(
    tok: &Option<TokenTree>,
    expand_group: Option<&Group>,
    expand_pattern: bool,
) -> ExpandAttribute {
    let mut expand_attrib = ExpandAttribute::default();
    expand_attrib.expand_pattern = expand_pattern;
    let ident_group = match tok {
        Some(TokenTree::Ident(ident)) => {
            expand_attrib.ident_names.insert(ident.to_string(), 0);
            return expand_attrib;
        }
        Some(TokenTree::Group(group))
            if group.delimiter() == Delimiter::Parenthesis =>
        {
            expand_attrib.ident_tup = true;
            group
        }
        _ => {
            expand_attrib.error_list.push(MetaError {
                message: "expected `(` or identifier".to_string(),
                span: tok_span_or_parent_close(tok, expand_group),
            });
            return expand_attrib;
        }
    };

    let mut name_count = 0;

    let mut group_iter = ident_group.stream().into_iter();

    loop {
        let Some(first) = group_iter.next() else {
            break;
        };
        let TokenTree::Ident(ident) = first else {
            expand_attrib.error_list.push(MetaError {
                message: "expected identifier".to_string(),
                span: first.span(),
            });
            return expand_attrib;
        };
        if expand_attrib
            .ident_names
            .insert(ident.to_string(), name_count)
            .is_some()
        {
            expand_attrib.error_list.push(MetaError {
                message: "same identifier used again".to_string(),
                span: ident.span(),
            });
            return expand_attrib;
        }
        name_count += 1;
        match group_iter.next() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == ',' => {
                continue;
            }
            Some(other) => {
                expand_attrib.error_list.push(MetaError {
                    message: "expected `,`".to_string(),
                    span: other.span(),
                });
                return expand_attrib;
            }
            None => break,
        }
    }
    if name_count == 0 {
        expand_attrib.error_list.push(MetaError {
            message: "expand identifier tuple can't be empty".to_string(),
            span: ident_group.span(),
        });
    }

    expand_attrib
}

fn parse_expand_attribute(
    tt1: &TokenTree,
    tt2: &TokenTree,
) -> Result<Option<ExpandAttribute>, MetaError> {
    let TokenTree::Punct(punct) = tt1 else {
        return Ok(None);
    };
    if punct.as_char() != '#' {
        return Ok(None);
    }
    let TokenTree::Group(attrib_group) = tt2 else {
        return Ok(None);
    };

    if attrib_group.delimiter() != Delimiter::Bracket {
        return Ok(None);
    }
    let mut attrib_body = attrib_group.stream().into_iter();
    let Some(TokenTree::Ident(ident)) = attrib_body.next() else {
        return Ok(None);
    };

    let ident_str = ident.to_string();

    let expand_pattern = match &*ident_str {
        "expand" => false,
        "expand_pattern" => true,
        _ => return Ok(None),
    };

    // from this point on we know that the user intended to talk to us
    // ( we found `#[expand]` ), so we start to emit syntax errors in case
    // something unexpected appears

    let expand_body_tok = attrib_body.next();
    let expand_body = match expand_body_tok {
        Some(TokenTree::Group(expand_body))
            if expand_body.delimiter() == Delimiter::Parenthesis =>
        {
            expand_body
        }
        _ => {
            return Err(MetaError {
                message: "expected `(` to begin expand body".to_string(),
                span: tok_span_or_parent_close(
                    &expand_body_tok,
                    Some(attrib_group),
                ),
            });
        }
    };

    if let Some(stray_tok) = attrib_body.next() {
        return Err(MetaError {
            message: "expected expand attribute to end after body".to_string(),
            span: stray_tok.span(),
        });
    }

    let mut expand_body_stream = expand_body.stream().into_iter();

    let expand_attrib = parse_expand_attrib_inner(
        Some(attrib_group),
        expand_pattern,
        Some(&expand_body),
        &mut expand_body_stream,
    );

    if let Some(t) = expand_body_stream.next() {
        return Err(MetaError {
            message: "stray token after the end of expand expresssion"
                .to_string(),
            span: t.span(),
        });
    }

    Ok(Some(expand_attrib))
}
// parses the T/(X, Y) in [..] part of the expand attribute
fn parse_expand_attrib_inner(
    _attrib_group: Option<&Group>,
    expand_pattern: bool,
    expand_body: Option<&Group>,
    expand_body_stream: &mut proc_macro::token_stream::IntoIter,
) -> ExpandAttribute {
    let expand_ident_tok = expand_body_stream.next();

    let mut expand_attrib = parse_expand_ident_names(
        &expand_ident_tok,
        expand_body,
        expand_pattern,
    );

    // TODO: we probably need to do somethign here
    let kw_in_tok = expand_body_stream.next();
    match kw_in_tok {
        Some(TokenTree::Ident(ident)) if ident.to_string() == "in" => (),
        _ => {
            expand_attrib.error_list.push(MetaError {
                message: "expand `in` after expand identifier".to_string(),
                span: tok_span_open_or_parent_close(
                    kw_in_tok.as_ref(),
                    expand_body,
                ),
            });
            return expand_attrib;
        }
    };
    let ident_count = expand_attrib.ident_names.len();

    let mut t = expand_body_stream.next();

    let mut cross_product = false;

    if let Some(TokenTree::Ident(p)) = &t {
        if p.to_string() == "matrix" {
            if !expand_attrib.ident_tup {
                expand_attrib.error_list.push(MetaError {
                    message: "cross product not supported on single replacement identifier".to_string(),
                    span: tok_span_open_or_parent_close(t.as_ref(), expand_body),
                });
                return expand_attrib;
            }
            cross_product = true;
            t = expand_body_stream.next();
        }
    }

    if let Some(TokenTree::Group(variants_group)) = &t {
        if variants_group.delimiter() == Delimiter::Bracket {
            if !expand_attrib.ident_tup {
                debug_assert_eq!(ident_count, 1);
                match parse_comma_separated_token_tree_lists(&variants_group) {
                    Ok(replacements) => {
                        for r in replacements {
                            expand_attrib.variants.push(ExpansionVariant {
                                ident_replacements: vec![r],
                            });
                        }
                    }
                    Err(e) => expand_attrib.error_list.push(e),
                }
                return expand_attrib;
            }
            parse_expand_variants_arr_of_tup(
                &mut expand_attrib,
                &variants_group,
                cross_product,
            );
            return expand_attrib;
        }
        if variants_group.delimiter() == Delimiter::Parenthesis {
            if expand_attrib.ident_tup {
                parse_expand_variants_tup_of_arr(
                    &mut expand_attrib,
                    &variants_group,
                    cross_product,
                );
            }
            return expand_attrib;
        }
    };
    let expected = match (expand_attrib.ident_tup, cross_product) {
        (true, true) => "`[` or `(`",
        (true, false) => "`matrix` or `[` or `(`",
        (false, _) => "`[`",
    };
    expand_attrib.error_list.push(MetaError {
        message: format!("expected {expected} to begin expansion variants"),
        span: tok_span_open_or_parent_close(t.as_ref(), expand_body),
    });
    expand_attrib
}

// returns true if any substutions were found
fn collect_substitutions_in_tt(
    expand: &mut ExpandAttribute,
    tt: &TokenTree,
    substitutions: &mut Vec<Substitution>,
    tok_idx: usize,
) -> bool {
    if let TokenTree::Ident(ident) = tt {
        if let Some(&ident_idx) = expand.ident_names.get(&ident.to_string()) {
            substitutions.push(Substitution {
                pos: tok_idx,
                kind: SubstitutionKind::InlineIdent(ident_idx),
            });
            return true;
        }
    }
    if let TokenTree::Group(g) = tt {
        let nested = collect_substitutions_in_group(expand, g);
        if !nested.is_empty() {
            substitutions.push(Substitution {
                pos: tok_idx,
                kind: SubstitutionKind::Nested(nested),
            });
        }
        return true;
    }
    false
}

fn collect_substitutions(
    expand: &mut ExpandAttribute,
    tokens: &[TokenTree],
    offset: usize,
) -> Vec<Substitution> {
    let mut substitutions = Vec::new();
    for (i, tt) in tokens.iter().enumerate() {
        collect_substitutions_in_tt(
            expand,
            &tt,
            &mut substitutions,
            offset + i,
        );
    }
    substitutions
}

fn parse_fn_call() {}

fn parse_expression_group(
    expand: &ExpandAttribute,
    expr: Vec<TokenTree>,
) -> Result<MetaExpr, MetaError> {
    let Some(first) = expr.get(0) else {
        return Ok(MetaExpr::FnCall {
            kind: MetaFnKind::Concat,
            args: Vec::default(),
        });
    };
    let Some(second) = expr.get(1) else {
        if let TokenTree::Ident(ident) = first {
            let s = ident.to_string();
            if let Some(ident_idx) = expand.ident_names.get(&s) {
                return Ok(MetaExpr::ExpansionIdent(*ident_idx));
            }
            return Ok(MetaExpr::Ident(s));
        }
        return Ok(MetaExpr::Token(expr.into_iter().next().unwrap()));
    };

    if let TokenTree::Group(g) = second {
        if g.delimiter() == Delimiter::Parenthesis {
            parse_fn_call();
        }
    }
    Ok(MetaExpr::FnCall {
        kind: MetaFnKind::Concat,
        args: Vec::default(),
    })
}

fn handle_expression_group(
    expand: &mut ExpandAttribute,
    _group: &Group,
    expr: Vec<TokenTree>,
    substitutions: &mut Vec<Substitution>,
) {
    match parse_expression_group(expand, expr) {
        Ok(expr) => substitutions.push(Substitution {
            pos: 0,
            kind: SubstitutionKind::MetaExpr(expr),
        }),
        Err(err) => {
            expand.error_list.push(err);
        }
    };
}

fn collect_substitutions_in_group(
    expand: &mut ExpandAttribute,
    group: &Group,
) -> Vec<Substitution> {
    let mut substitutions = Vec::new();
    let mut i = 0;
    let mut iter = group.stream().into_iter();
    if group.delimiter() == Delimiter::Bracket {
        if let Some(tt) = iter.next() {
            if let TokenTree::Punct(p) = &tt {
                if p.as_char() == '<' {
                    let mut collected: Vec<_> = iter.collect();
                    if let Some(TokenTree::Punct(p)) = collected.last() {
                        if p.as_char() == '>' {
                            collected.pop();
                            handle_expression_group(
                                expand,
                                group,
                                collected,
                                &mut substitutions,
                            );
                            return substitutions;
                        }
                    }
                    for tt in collected {
                        i += 1;
                        collect_substitutions_in_tt(
                            expand,
                            &tt,
                            &mut substitutions,
                            i,
                        );
                    }
                    return substitutions;
                }
            }
            collect_substitutions_in_tt(expand, &tt, &mut substitutions, 0);
            i += 1;
        }
    }

    for tt in iter {
        collect_substitutions_in_tt(expand, &tt, &mut substitutions, i);
        i += 1;
    }
    substitutions
}

fn parse_match_arm_pattern(
    expand: &mut ExpandAttribute,
    tokens: &[TokenTree],
    offset: usize,
    substitutions: &mut Vec<Substitution>,
) -> usize {
    let mut i = offset;
    while i < tokens.len() {
        let tok_idx = i;
        i += 1;
        let tt = &tokens[tok_idx];
        if collect_substitutions_in_tt(expand, tt, substitutions, tok_idx) {
            continue;
        }
        let TokenTree::Punct(p) = tt else {
            continue;
        };
        if p.as_char() != '=' {
            continue;
        }
        if p.spacing() != Spacing::Joint {
            continue;
        }
        let Some(tt) = &tokens.get(i) else {
            break;
        };
        let TokenTree::Punct(p) = tt else {
            continue;
        };
        i += 1;
        if p.as_char() != '>' {
            continue;
        }
        break;
    }
    i
}

fn parse_match_arm_body(
    expand: &mut ExpandAttribute,
    tokens: &[TokenTree],
    offset: usize,
    allow_substitutions: bool,
    substitutions: &mut Vec<Substitution>,
    requires_trailing_comma: &mut bool,
) -> usize {
    let mut i = offset;
    *requires_trailing_comma = false;

    if let Some(TokenTree::Group(group)) = tokens.get(i) {
        if group.delimiter() == Delimiter::Brace {
            if allow_substitutions {
                substitutions.push(Substitution {
                    pos: i,
                    kind: SubstitutionKind::Nested(
                        collect_substitutions_in_group(expand, group),
                    ),
                });
            }
            return i + 1;
        }
    }
    *requires_trailing_comma = true;
    while i < tokens.len() {
        let tok_idx = i;
        i += 1;

        let tt = &tokens[tok_idx];

        if allow_substitutions
            && collect_substitutions_in_tt(expand, tt, substitutions, tok_idx)
        {
            continue;
        }
        let TokenTree::Punct(p) = tt else {
            continue;
        };
        if p.as_char() != ',' {
            continue;
        }
        *requires_trailing_comma = false;
        break;
    }
    i
}

fn parse_match_arm(
    expand: &mut ExpandAttribute,
    input: &[TokenTree],
    offset: usize,
) -> Template {
    let mut substitutions = Vec::new();
    let pattern_end =
        parse_match_arm_pattern(expand, input, offset, &mut substitutions);

    let mut requires_trailing_comma = false;
    let body_end = parse_match_arm_body(
        expand,
        input,
        pattern_end,
        !expand.expand_pattern,
        &mut substitutions,
        &mut requires_trailing_comma,
    );

    Template {
        offset,
        substitutions,
        add_trailing_comma: requires_trailing_comma,
        // makes the fat arrow '=>' belong to the body
        // because it should only appear once in case of expand_pattern
        pattern_end: pattern_end - 2,
        body_end,
    }
}

fn expand_substitution(
    source: &[TokenTree],
    expansion: &mut Vec<TokenTree>,
    substitutions: &[Substitution],
    variant: &ExpansionVariant,
    initial_offset: usize,
) {
    let mut template_pos = initial_offset;

    for subst in substitutions {
        match &subst.kind {
            SubstitutionKind::InlineIdent(ident_idx) => {
                expansion
                    .extend(source[template_pos..subst.pos].iter().cloned());
                expansion.extend(
                    variant.ident_replacements[*ident_idx].iter().cloned(),
                );
            }
            SubstitutionKind::Nested(nested_substs) => {
                let TokenTree::Group(source_group) = &source[subst.pos] else {
                    unreachable!()
                };
                let group_body =
                    source_group.stream().into_iter().collect::<Vec<_>>();
                let mut group_target = Vec::new();
                expand_substitution(
                    &group_body,
                    &mut group_target,
                    nested_substs,
                    variant,
                    0,
                );
                expansion
                    .extend(source[template_pos..subst.pos].iter().cloned());
                let mut group = Group::new(
                    source_group.delimiter(),
                    TokenStream::from_iter(group_target),
                );
                group.set_span(source_group.span());
                expansion.push(TokenTree::Group(group));
            }
            SubstitutionKind::MetaExpr(expr) => {
                expansion
                    .extend(source[template_pos..subst.pos].iter().cloned());
                if let Err(err) =
                    evaluate_subtituion_expr(Some(&variant), expr, expansion)
                {
                    expansion.extend(compile_error(&err.message, err.span));
                }
            }
        }
        template_pos = subst.pos + 1;
    }
    expansion.extend(source[template_pos..].iter().cloned());
}

fn evaluate_subtituion_expr(
    _variant: Option<&&ExpansionVariant>,
    _expr: &MetaExpr,
    _expansion: &[TokenTree],
) -> Result<(), MetaError> {
    Ok(())
}

fn expand_pattern(
    expand_attribute: &ExpandAttribute,
    output_tokens: &mut Vec<TokenTree>,
    input_tokens: &[TokenTree],
    template: &Template,
) {
    for (i, variant) in expand_attribute.variants.iter().enumerate() {
        if i != 0 {
            output_tokens
                .push(TokenTree::Punct(Punct::new('|', Spacing::Alone)));
        }
        expand_substitution(
            &input_tokens[..template.pattern_end],
            output_tokens,
            &template.substitutions,
            variant,
            template.offset,
        );
    }
    output_tokens.extend_from_slice(
        &input_tokens[template.pattern_end..template.body_end],
    );
}

fn expand_full(
    expand_attribute: &ExpandAttribute,
    input_tokens: &[TokenTree],
    template: &Template,
    output_tokens: &mut Vec<TokenTree>,
) {
    for variant in &expand_attribute.variants {
        expand_substitution(
            &input_tokens[..template.body_end],
            output_tokens,
            &template.substitutions,
            variant,
            template.offset,
        );
    }
}

fn process_match_body(body: TokenStream) -> Result<TokenStream, MetaError> {
    let input_tokens = Vec::from_iter(body);
    let mut output_tokens = Vec::new();
    let mut i = 0;
    let mut last_directive_end = 0;
    while i < input_tokens.len() {
        if i + 1 == input_tokens.len() {
            break;
        }

        let Some(mut expand_attribute) =
            parse_expand_attribute(&input_tokens[i], &input_tokens[i + 1])?
        else {
            i += 1;
            continue;
        };

        let expand_directive_start = i;

        // #[expand(...)] are two tokens
        let match_arm_start = expand_directive_start + 2;

        let template = parse_match_arm(
            &mut expand_attribute,
            &input_tokens,
            match_arm_start,
        );

        output_tokens.extend(
            input_tokens[last_directive_end..expand_directive_start]
                .iter()
                .cloned(),
        );
        last_directive_end = template.body_end;
        i = last_directive_end;

        if expand_attribute.expand_pattern {
            expand_pattern(
                &expand_attribute,
                &mut output_tokens,
                &input_tokens,
                &template,
            );
        } else {
            expand_full(
                &expand_attribute,
                &input_tokens,
                &template,
                &mut output_tokens,
            );
        }
        if template.add_trailing_comma {
            output_tokens
                .push(TokenTree::Punct(Punct::new(',', Spacing::Alone)));
        }
    }
    output_tokens.extend(input_tokens[last_directive_end..].iter().cloned());
    Ok(TokenStream::from_iter(output_tokens))
}
