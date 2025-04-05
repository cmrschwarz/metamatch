use proc_macro::{Delimiter, Group, Spacing, Span, TokenTree};
use std::{collections::HashMap, fmt::Debug, rc::Rc};

use super::{
    ast::{
        BinaryOpKind, Binding, BindingParameter, Context, ExpandPattern,
        Function, MetaError, MetaExpr, MetaValue, Pattern, Scope, ScopeKind,
        TrailingBlockKind, UnaryOpKind,
    },
    macro_impls::IntoIterIntoVec,
};

type Result<T> = std::result::Result<T, ()>;

pub enum RawBodyParseResult {
    Plain,
    Complete(Vec<Rc<MetaExpr>>),
    UnmatchedEnd {
        span: Span,
        kind: TrailingBlockKind,
        offset: usize,
        contents: Vec<Rc<MetaExpr>>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExpandKind {
    ExpandFull,
    ExpandPattern,
}

struct MatchArmEnds {
    pattern_end: usize,
    guard_end: usize,
    body_end: usize,
}

// for a match arm ( ... => ... )
// returns the location before the fat arrow and the location after the end of
// match expression body, and after a potential trailing comma
fn find_match_arm_bounds(tokens: &[TokenTree]) -> Option<MatchArmEnds> {
    let mut i = 0;
    // ... =>
    let mut pattern_end = None;
    let mut guard_end = None;
    while i < tokens.len() {
        let t = &tokens[i];
        i += 1;

        if let TokenTree::Ident(ident) = t {
            if ident.to_string() == "if" {
                if pattern_end.is_some() {
                    // not possible in a valid match arm
                    return None;
                }
                pattern_end = Some(i);
            }
        }

        let TokenTree::Punct(p) = t else {
            continue;
        };
        if p.as_char() != '=' {
            continue;
        }
        if p.spacing() != Spacing::Joint {
            continue;
        }
        let Some(t) = &tokens.get(i) else {
            break;
        };
        let TokenTree::Punct(p) = t else {
            continue;
        };
        if p.as_char() != '>' {
            continue;
        }
        i += 1;
        guard_end = Some(i - 2);
        break;
    }

    if pattern_end.is_none() {
        pattern_end = guard_end;
    }

    let mut ends = MatchArmEnds {
        pattern_end: pattern_end?,
        guard_end: guard_end?,
        body_end: 0,
    };

    if let Some(TokenTree::Group(g)) = tokens.get(i) {
        if g.delimiter() == Delimiter::Brace {
            // => {}
            if let Some(TokenTree::Punct(p)) = &tokens.get(i + 1) {
                if p.as_char() == ',' {
                    ends.body_end = i + 2;
                    return Some(ends);
                }
            }
            ends.body_end = i + 1;
            return Some(ends);
        }
    }
    // => ... ,
    while i < tokens.len() {
        if let TokenTree::Punct(p) = &tokens[i] {
            if p.as_char() == ',' {
                ends.body_end = i + 1;
                return Some(ends);
            }
        }
        i += 1;
    }
    ends.body_end = i;
    Some(ends)
}

fn peek_binary_operator(
    tokens: &[TokenTree],
) -> Option<(BinaryOpKind, Span, &[TokenTree])> {
    let mut chars = [b'_'; 3];

    let mut len = 0;

    #[allow(clippy::needless_range_loop)]
    for i in 0..3 {
        if let Some(TokenTree::Punct(p)) = tokens.get(i) {
            let c = p.as_char();
            if !c.is_ascii() {
                len = i;
                break;
            }
            chars[i] = c as u8;
            if p.spacing() == Spacing::Alone {
                len = i + 1;
                break;
            }
        }
    }

    let kind = match std::str::from_utf8(&chars[0..len]).ok()? {
        "+" => BinaryOpKind::Add,
        "-" => BinaryOpKind::Sub,
        "*" => BinaryOpKind::Mul,
        "/" => BinaryOpKind::Div,
        "%" => BinaryOpKind::Rem,
        "=" => BinaryOpKind::Assign,
        "==" => BinaryOpKind::Equal,
        "!=" => BinaryOpKind::NotEqual,
        ".." => BinaryOpKind::RangeExclusive,
        "..=" => BinaryOpKind::RangeInclusive,
        "&" => BinaryOpKind::BinaryAnd,
        "|" => BinaryOpKind::BinaryOr,
        "^" => BinaryOpKind::BinaryXor,
        "&&" => BinaryOpKind::LogicalAnd,
        "||" => BinaryOpKind::LogicalOr,
        "<" => BinaryOpKind::LessThan,
        "<=" => BinaryOpKind::LessThanOrEqual,
        ">" => BinaryOpKind::GreaterThan,
        ">=" => BinaryOpKind::GreaterThanOrEqual,
        "<<" => BinaryOpKind::ShiftLeft,
        ">>" => BinaryOpKind::ShiftRight,
        "+=" => BinaryOpKind::AddAssign,
        "-=" => BinaryOpKind::SubAssign,
        "*=" => BinaryOpKind::MulAssign,
        "/=" => BinaryOpKind::DivAssign,
        "%=" => BinaryOpKind::RemAssign,
        "<<=" => BinaryOpKind::ShiftLeftAssign,
        ">>=" => BinaryOpKind::ShiftRightAssign,
        "&=" => BinaryOpKind::BinaryAndAssign,
        "|=" => BinaryOpKind::BinaryOrAssign,
        "^=" => BinaryOpKind::BinaryXorAssign,
        _ => return None,
    };

    Some((kind, tokens[0].span(), &tokens[len..]))
}

fn as_unary_operator(p: &proc_macro::Punct) -> Option<UnaryOpKind> {
    if p.spacing() != Spacing::Alone {
        return None;
    }
    match p.as_char() {
        '-' => Some(UnaryOpKind::Minus),
        '!' => Some(UnaryOpKind::Not),
        _ => None,
    }
}

fn has_template_angle_backets(tokens: &[TokenTree]) -> bool {
    let Some(TokenTree::Punct(p)) = tokens.first() else {
        return false;
    };
    if p.as_char() != '<' {
        return false;
    }

    let Some(TokenTree::Punct(p)) = tokens.last() else {
        return false;
    };
    if p.as_char() != '>' {
        return false;
    }
    true
}

fn append_token_list(
    exprs: &mut Vec<Rc<MetaExpr>>,
    tokens: &[TokenTree],
    start: usize,
    end: usize,
) {
    if start != end {
        exprs.push(Rc::new(MetaExpr::Literal {
            span: tokens[start].span(),
            value: Rc::new(MetaValue::Tokens(tokens[start..end].to_vec())),
        }));
    }
}

fn parse_literal(token: &TokenTree) -> Rc<MetaValue> {
    if let TokenTree::Literal(lit) = token {
        let s = lit.to_string();
        if s.starts_with('"') {
            return Rc::new(MetaValue::String {
                value: Rc::from(s[1..s.len() - 1].to_string()),
                span: Some(token.span()),
            });
        }
        if let Ok(n) = s.parse::<i64>() {
            return Rc::new(MetaValue::Int {
                value: n,
                span: Some(token.span()),
            });
        }
        if let Ok(n) = s.parse::<f64>() {
            return Rc::new(MetaValue::Float {
                value: n,
                span: Some(token.span()),
            });
        }
    }
    Rc::new(MetaValue::Token(token.clone()))
}

fn parse_ident(token: &TokenTree) -> Option<(Span, Rc<str>)> {
    if let TokenTree::Ident(ident) = token {
        Some((ident.span(), Rc::from(ident.to_string())))
    } else {
        None
    }
}

fn delimiter_chars(d: Delimiter) -> (char, char) {
    match d {
        Delimiter::Parenthesis => ('(', ')'),
        Delimiter::Brace => ('{', '}'),
        Delimiter::Bracket => ('[', ']'),
        Delimiter::None => (' ', ' '),
    }
}

impl Context {
    pub fn new(primary_scope_kind: ScopeKind) -> Self {
        let empty_token_list = Rc::new(MetaValue::Tokens(Vec::new()));
        let mut ctx = Self {
            empty_token_list_expr: Rc::new(MetaExpr::Literal {
                span: Span::call_site(),
                value: empty_token_list.clone(),
            }),
            empty_token_list,
            scopes: vec![Scope {
                kind: primary_scope_kind,
                bindings: HashMap::new(),
            }],
            errors: Vec::new(),
        };
        ctx.insert_builtins();
        ctx
    }
    pub fn push_dummy_scope(&mut self, kind: ScopeKind) {
        self.scopes.push(Scope {
            kind,
            bindings: HashMap::new(),
        });
    }
    fn parse_expr_deny_trailing_block<'a>(
        &mut self,
        parent_span: Span,
        tokens: &'a [TokenTree],
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree])> {
        let (expr, rest, _trailing_block) =
            self.parse_expr(parent_span, tokens, false)?;
        Ok((expr, rest))
    }
    fn parse_expr<'a>(
        &mut self,
        parent_span: Span,
        tokens: &'a [TokenTree],
        allow_trailing_block: bool,
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        self.parse_expr_with_prec(parent_span, tokens, allow_trailing_block, 0)
    }

    fn parse_expr_with_prec<'a>(
        &mut self,
        parent_span: Span,
        tokens: &'a [TokenTree],
        allow_trailing_block: bool,
        min_prec: u8,
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        let (mut lhs, mut rest, mut trailing) =
            self.parse_expr_value(parent_span, tokens, allow_trailing_block)?;

        loop {
            if rest.is_empty() || trailing.is_some() {
                return Ok((lhs, rest, trailing));
            }

            let Some((op, op_span, next_rest)) = peek_binary_operator(rest)
            else {
                return Ok((lhs, rest, trailing));
            };

            let prec = op.precedence();
            if prec < min_prec {
                return Ok((lhs, rest, trailing));
            }

            let next_min_bp = if op.is_right_associative() {
                prec
            } else {
                prec + 1
            };

            let (rhs, rest_new, trailing_new) = self.parse_expr_with_prec(
                parent_span,
                next_rest,
                allow_trailing_block,
                next_min_bp,
            )?;

            lhs = Rc::new(MetaExpr::OpBinary {
                span: op_span,
                kind: op,
                lhs,
                rhs,
            });

            rest = rest_new;
            trailing = trailing_new;
        }
    }

    fn parse_template_tag_in_expr<'a>(
        &mut self,
        group: &Group,
        group_tokens: &[TokenTree],
        rest_tokens: &'a [TokenTree],
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        let template_tokens = &group_tokens[1..&group_tokens.len() - 1];

        if let Some(TokenTree::Punct(p)) = template_tokens.first() {
            if p.as_char() == '/' {
                let end_tag =
                    self.parse_closing_tag(group, template_tokens)?;
                if end_tag == TrailingBlockKind::Unquote {
                    return Ok((
                        self.empty_token_list_expr.clone(),
                        rest_tokens,
                        Some(end_tag),
                    ));
                } else {
                    self.error(
                        group.span(),
                        format!("unexpected end tag `{}`", end_tag.to_str()),
                    );
                    return Err(());
                }
            }
        }

        let mut is_raw = false;
        let mut is_quote = false;
        if template_tokens.len() == 1 {
            if let Some(TokenTree::Ident(i)) = template_tokens.first() {
                is_quote = i.to_string() == "quote";
                is_raw = i.to_string() == "raw";
            }
        }

        if !is_quote && !is_raw {
            self.error(group.span(), "template tags besides `raw` and `quote` are only supported in quoted output");
            return Err(()); // TODO: debatable?
        }

        let (trail_kind, scope_kind) = if is_raw {
            (TrailingBlockKind::Raw, ScopeKind::Raw)
        } else {
            (TrailingBlockKind::Quote, ScopeKind::Quoted)
        };

        self.push_dummy_scope(scope_kind);

        let res = self.parse_raw_block(group.span(), rest_tokens);

        self.scopes.pop();

        match res? {
            RawBodyParseResult::Plain | RawBodyParseResult::Complete(_) => {
                self.error(group.span(), "`quote` tag is never closed");
                Err(())
            }
            RawBodyParseResult::UnmatchedEnd {
                span,
                kind,
                offset,
                contents,
            } => {
                if kind != trail_kind {
                    self.error(
                        span,
                        format!("unmatched closing tag `/{}`", kind.to_str()),
                    );
                    return Err(());
                }
                Ok((
                    Rc::new(MetaExpr::Scope {
                        span: group.span(),
                        body: contents,
                    }),
                    &rest_tokens[offset + 1..],
                    None,
                ))
            }
        }
    }
    fn parse_expr_value<'a>(
        &mut self,
        parent_span: Span,
        tokens: &'a [TokenTree],
        allow_trailing_block: bool,
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        if tokens.is_empty() {
            self.error(parent_span, "unexpected end of input".to_owned());
            return Err(());
        }
        match &tokens[0] {
            TokenTree::Group(group) => {
                let group_tokens = group.stream().into_vec();
                match group.delimiter() {
                    Delimiter::Parenthesis => {
                        let expr = self.parse_tuple_or_expr(
                            group.span(),
                            &group_tokens,
                        )?;
                        Ok((expr, &tokens[1..], None))
                    }
                    Delimiter::Bracket => {
                        let rest = &tokens[1..];
                        let (expr, rest, trailing_block_tag) =
                            if has_template_angle_backets(&group_tokens) {
                                self.parse_template_tag_in_expr(
                                    group,
                                    &group_tokens,
                                    rest,
                                )?
                            } else {
                                let list = self.parse_comma_separated(
                                    Some("list"),
                                    group.delimiter(),
                                    group.span(),
                                    &group_tokens,
                                )?;
                                (
                                    Rc::new(MetaExpr::List {
                                        span: group.span(),
                                        exprs: list,
                                    }),
                                    rest,
                                    None,
                                )
                            };
                        Ok((expr, rest, trailing_block_tag))
                    }
                    Delimiter::None => {
                        let (expr, rest) = self
                            .parse_expr_deny_trailing_block(
                                group.span(),
                                &group_tokens,
                            )?;
                        if !rest.is_empty() {
                            self.error(
                                rest[0].span(),
                                "stay token after end of expression",
                            );
                            return Err(());
                        }
                        Ok((expr, &tokens[1..], None))
                    }
                    Delimiter::Brace => Ok((
                        Rc::new(MetaExpr::Scope {
                            span: group.span(),
                            body: self.parse_body_deny_trailing(
                                group.span(),
                                &group_tokens,
                            ),
                        }),
                        &tokens[1..],
                        None,
                    )),
                }
            }

            TokenTree::Ident(ident) => {
                let span = ident.span();
                let name = Rc::from(ident.to_string());

                match &*name {
                    "let" => {
                        return self.parse_let(
                            span,
                            &tokens[1..],
                            allow_trailing_block,
                        )
                    }
                    "for" => {
                        return self.parse_for(
                            span,
                            &tokens[1..],
                            allow_trailing_block,
                        );
                    }
                    "fn" => {
                        return self.parse_fn(
                            span,
                            &tokens[1..],
                            allow_trailing_block,
                        );
                    }

                    "if" => {
                        return self.parse_if(
                            span,
                            &tokens[1..],
                            allow_trailing_block,
                        );
                    }
                    "quote" => {
                        return self.parse_quote_expr(
                            span,
                            &tokens[1..],
                            allow_trailing_block,
                        )
                    }
                    "raw" => {
                        return self.parse_raw_expr(
                            span,
                            &tokens[1..],
                            allow_trailing_block,
                        )
                    }
                    "unquote" => {
                        return self.parse_unquote_expr(
                            span,
                            &tokens[1..],
                            allow_trailing_block,
                        )
                    }
                    "true" | "false" => {
                        let val = &*name == "true";
                        return Ok((
                            Rc::new(MetaExpr::Literal {
                                span,
                                value: Rc::new(MetaValue::Bool {
                                    value: val,
                                    span: Some(ident.span()),
                                }),
                            }),
                            &tokens[1..],
                            None,
                        ));
                    }
                    _ => (),
                }

                // Check if it's a function call
                if tokens.len() > 1 {
                    if let TokenTree::Group(group) = &tokens[1] {
                        if group.delimiter() == Delimiter::Parenthesis {
                            let args = group.stream().into_vec();
                            let fn_args = self.parse_comma_separated(
                                Some("function call arguments"),
                                Delimiter::Parenthesis,
                                group.span(),
                                &args,
                            )?;
                            return Ok((
                                Rc::new(MetaExpr::FnCall {
                                    span,
                                    name,
                                    args: fn_args,
                                }),
                                &tokens[2..],
                                None,
                            ));
                        }
                    }
                }

                Ok((
                    Rc::new(MetaExpr::Ident { span, name }),
                    &tokens[1..],
                    None,
                ))
            }

            TokenTree::Literal(lit) => Ok((
                Rc::new(MetaExpr::Literal {
                    span: lit.span(),
                    value: parse_literal(&tokens[0]),
                }),
                &tokens[1..],
                None,
            )),

            TokenTree::Punct(p) => {
                if let Some(op_kind) = as_unary_operator(p) {
                    let (operand, rest, _tb) = self.parse_expr_with_prec(
                        p.span(),
                        &tokens[1..],
                        false,
                        op_kind.precedence(),
                    )?;
                    return Ok((
                        Rc::new(MetaExpr::OpUnary {
                            kind: op_kind,
                            operand,
                            span: p.span(),
                        }),
                        rest,
                        None,
                    ));
                }
                Ok((
                    Rc::new(MetaExpr::Literal {
                        span: p.span(),
                        value: Rc::new(MetaValue::Token(tokens[0].clone())),
                    }),
                    &tokens[1..],
                    None,
                ))
            }
        }
    }

    pub fn error(&mut self, span: Span, message: impl Into<String>) {
        self.errors.push(MetaError {
            span,
            message: message.into(),
        });
    }

    fn insert_dummy_binding(&mut self, name: Rc<str>, super_bound: bool) {
        // dummy binding during parsing so raw blocks can detect identifiers
        self.scopes.last_mut().unwrap().bindings.insert(
            name.clone(),
            Binding {
                span: Span::call_site(),
                super_bound,
                value: self.empty_token_list.clone(),
                mutable: false,
            },
        );
    }

    fn insert_dummy_bindings_for_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Ident(i) => {
                self.insert_dummy_binding(i.name.clone(), i.super_bound)
            }
            Pattern::Tuple { span: _, elems } => {
                for pat in elems {
                    self.insert_dummy_bindings_for_pattern(pat);
                }
            }
            Pattern::List { span: _, elems } => {
                for pat in elems {
                    self.insert_dummy_bindings_for_pattern(pat);
                }
            }
        }
    }

    fn parse_let<'a>(
        &mut self,
        let_span: Span,
        tokens: &'a [TokenTree],
        allow_trailing_block: bool,
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        if tokens.is_empty() {
            self.error(let_span, "expected pattern after `let`");
            return Err(());
        }

        let (pattern, rest) = self.parse_pattern(let_span, tokens)?;

        if rest.is_empty() && allow_trailing_block {
            self.insert_dummy_bindings_for_pattern(&pattern);
            return Ok((
                Rc::new(MetaExpr::LetBinding {
                    span: let_span,
                    pattern,
                    expr: None,
                }),
                &[],
                Some(TrailingBlockKind::Let),
            ));
        }

        let mut eq_span = None;
        if let Some(TokenTree::Punct(p)) = rest.first() {
            if p.as_char() == '=' {
                eq_span = Some(p.span());
            }
        }

        let Some(eq_span) = eq_span else {
            self.error(
                rest.first().map(|t| t.span()).unwrap_or(let_span),
                "expected = after let pattern",
            );
            return Err(());
        };

        let rest = &rest[1..];

        if rest.is_empty() {
            self.error(eq_span, "expected expression after `=`");
            return Err(());
        }
        let (expr, rest) =
            self.parse_expr_deny_trailing_block(let_span, rest)?;

        self.insert_dummy_bindings_for_pattern(&pattern);

        Ok((
            Rc::new(MetaExpr::LetBinding {
                span: let_span,
                pattern,
                expr: Some(expr),
            }),
            rest,
            None,
        ))
    }

    fn parse_quote_expr<'a>(
        &mut self,
        raw_span: Span,
        tokens: &'a [TokenTree],
        allow_trailing_block: bool,
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        let mut has_exclam = false;
        if let Some(TokenTree::Punct(p)) = tokens.first() {
            has_exclam = p.as_char() == '!';
        }

        if allow_trailing_block
            && (tokens.is_empty() || has_exclam && tokens.len() == 1)
        {
            if has_exclam {
                self.error(
                    tokens.first().unwrap().span(),
                    "the `quote` template tag does not use an `!`",
                );
            }

            if self.scopes.last().as_mut().unwrap().kind == ScopeKind::Quoted {
                self.error(raw_span, "redundant quote tag, arlready quoted");
            }

            self.push_dummy_scope(ScopeKind::Quoted);
            return Ok((
                Rc::new(MetaExpr::Scope {
                    span: raw_span,
                    body: Vec::new(),
                }),
                &[],
                Some(TrailingBlockKind::Quote),
            ));
        }

        if !has_exclam {
            self.error(
                tokens.first().map(|t| t.span()).unwrap_or(raw_span),
                "expected `!` after `quote`",
            );
        }

        let tokens = &tokens[usize::from(has_exclam)..];

        self.push_dummy_scope(ScopeKind::Quoted);

        let Some(TokenTree::Group(p)) = tokens.first() else {
            if has_exclam {
                self.error(
                    tokens.first().map(|t| t.span()).unwrap_or(raw_span),
                    "expected block after `quote!`",
                );
            }
            return Err(());
        };

        let raw_block_contents = p.stream().into_vec();

        let contents = self
            .parse_raw_block_to_exprs(raw_span, &raw_block_contents)
            .unwrap_or_default(); // we continue in case of a nested error

        self.scopes.pop();

        Ok((
            Rc::new(MetaExpr::Scope {
                span: raw_span,
                body: contents,
            }),
            &tokens[1..],
            None,
        ))
    }

    fn parse_raw_expr<'a>(
        &mut self,
        raw_span: Span,
        tokens: &'a [TokenTree],
        allow_trailing_block: bool,
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        let mut has_exclam = false;
        if let Some(TokenTree::Punct(p)) = tokens.first() {
            has_exclam = p.as_char() == '!';
        }

        if allow_trailing_block
            && (tokens.is_empty() || has_exclam && tokens.len() == 1)
        {
            if has_exclam {
                self.error(
                    tokens.first().unwrap().span(),
                    "the `raw` template tag does not use an `!`",
                );
            }
            self.push_dummy_scope(ScopeKind::Raw);
            return Ok((
                Rc::new(MetaExpr::Scope {
                    span: raw_span,
                    body: Vec::new(),
                }),
                &[],
                Some(TrailingBlockKind::Raw),
            ));
        }

        if !has_exclam {
            self.error(
                tokens.first().map(|t| t.span()).unwrap_or(raw_span),
                "expected `!` after `raw`",
            );
        }

        let tokens = &tokens[usize::from(has_exclam)..];

        let Some(TokenTree::Group(group)) = tokens.first() else {
            if has_exclam {
                self.error(
                    tokens.first().map(|t| t.span()).unwrap_or(raw_span),
                    "expected block after `quote!`",
                );
            }
            return Err(());
        };

        let raw_block_contents = group.stream().into_vec();

        Ok((
            Rc::new(MetaExpr::Literal {
                span: group.span(),
                value: Rc::new(MetaValue::Tokens(raw_block_contents)),
            }),
            &tokens[1..],
            None,
        ))
    }

    fn parse_unquote_expr<'a>(
        &mut self,
        raw_span: Span,
        tokens: &'a [TokenTree],
        allow_trailing_block: bool, // must be true, otherwise error
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        if !allow_trailing_block || !tokens.is_empty() {
            self.error(
                tokens.first().map(|t| t.span()).unwrap_or(raw_span),
                "`unquote` is only allowed as a template block",
            );
            return Err(());
        }

        self.push_dummy_scope(ScopeKind::Unquoted);

        Ok((
            Rc::new(MetaExpr::Scope {
                span: raw_span,
                body: Vec::new(),
            }),
            &[],
            Some(TrailingBlockKind::Unquote),
        ))
    }

    fn parse_fn<'a>(
        &mut self,
        fn_span: Span,
        tokens: &'a [TokenTree],
        allow_trailing_block: bool,
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        if tokens.is_empty() {
            self.error(fn_span, "expected identifier after `fn`");
        }

        let (name_span, name) = parse_ident(&tokens[0]).ok_or_else(|| {
            self.error(tokens[0].span(), "expected function name");
        })?;

        let Some(TokenTree::Group(param_group)) = tokens.get(1) else {
            self.error(tokens[1].span(), "expected parameter list");
            return Err(());
        };

        if param_group.delimiter() != Delimiter::Parenthesis {
            self.error(
                param_group.span(),
                "expected parentheses around parameter list".to_owned(),
            );
            return Err(());
        }

        let param_tokens = param_group.stream().into_vec();
        let mut params = Vec::new();
        let mut params_rest = &param_tokens[..];

        let rest = &tokens[3..];

        self.push_dummy_scope(ScopeKind::Unquoted);

        while !params_rest.is_empty() {
            // TODO: support mutable and super here

            let (param_span, param_name) = parse_ident(&params_rest[0])
                .ok_or_else(|| {
                    self.error(
                        params_rest[0].span(),
                        "expected parameter name".to_owned(),
                    );
                })?;

            let super_bound = param_name
                .chars()
                .next()
                .map(|c| c.is_uppercase())
                .unwrap_or_default();

            self.insert_dummy_binding(param_name.clone(), super_bound);

            params.push(BindingParameter {
                span: param_span,
                name: param_name,
                super_bound,
                mutable: false,
            });

            params_rest = &params_rest[1..];
            if !params_rest.is_empty() {
                if let TokenTree::Punct(p) = &params_rest[0] {
                    if p.as_char() == ',' {
                        params_rest = &params_rest[1..];
                        continue;
                    }
                }
                if !params_rest.is_empty() {
                    self.error(
                        params_rest[0].span(),
                        "expected comma between parameters".to_owned(),
                    );
                    return Err(());
                }
            }
        }

        let (body, rest, trailing_block) = if rest.is_empty() {
            if !allow_trailing_block {
                self.error(
                    tokens[2].span(),
                    "expected function body after parameters",
                );
                return Err(());
            }
            (Vec::new(), rest, Some(TrailingBlockKind::Fn))
        } else {
            let TokenTree::Group(body_group) = &rest[0] else {
                self.error(tokens[0].span(), "expected function body");
                return Err(());
            };

            if body_group.delimiter() != Delimiter::Brace {
                self.error(
                    body_group.span(),
                    "expected braces around function body",
                );
                return Err(());
            }

            let body_tokens = body_group.stream().into_vec();
            let body = self.parse_body_deny_trailing(name_span, &body_tokens);

            self.scopes.pop();

            (body, &rest[1..], None)
        };

        Ok((
            Rc::new(MetaExpr::FnDecl(Rc::new(Function {
                span: name_span,
                name: name.clone(),
                params,
                body,
            }))),
            rest,
            trailing_block,
        ))
    }

    fn parse_for<'a>(
        &mut self,
        for_span: Span,
        tokens: &'a [TokenTree],
        allow_trailing_block: bool,
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        let (pattern, rest) = self.parse_pattern(for_span, tokens)?;

        let mut is_in = false;
        if let Some(TokenTree::Ident(i)) = rest.first() {
            is_in = i.to_string() == "in";
        }
        if !is_in {
            self.error(
                rest.first().map(|f| f.span()).unwrap_or(for_span),
                "expected 'in' after pattern",
            );
            return Err(());
        }

        if rest.len() < 2 {
            self.error(rest[0].span(), "expected expression after 'in'");
            return Err(());
        }

        let last_tok = rest.last().unwrap();

        let (variants_expr, rest) =
            self.parse_expr_deny_trailing_block(for_span, &rest[1..])?;

        self.push_dummy_scope(ScopeKind::Unquoted);
        self.insert_dummy_bindings_for_pattern(&pattern);

        let (body, final_rest, trailing_block);
        if rest.is_empty() && allow_trailing_block {
            body = Vec::new();
            final_rest = rest;
            trailing_block = Some(TrailingBlockKind::For);
        } else {
            // Parse body
            let Some(TokenTree::Group(body_group)) = rest.first() else {
                self.error(
                    rest.first().unwrap_or(last_tok).span(),
                    "expected for loop body",
                );
                return Err(());
            };

            if body_group.delimiter() != Delimiter::Brace {
                self.error(
                    body_group.span(),
                    "expected braces around for loop body",
                );
                return Err(());
            }

            let body_tokens = body_group.stream().into_vec();

            body =
                self.parse_body_deny_trailing(body_group.span(), &body_tokens);

            self.scopes.pop();
            final_rest = &rest[1..];
            trailing_block = None;
        };

        Ok((
            Rc::new(MetaExpr::ForExpansion {
                span: for_span,
                pattern,
                variants_expr,
                body,
            }),
            final_rest,
            trailing_block,
        ))
    }

    fn parse_if<'a>(
        &mut self,
        if_span: Span,
        tokens: &'a [TokenTree],
        allow_trailing_block: bool,
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        let (condition, rest) =
            self.parse_expr_deny_trailing_block(if_span, tokens)?;

        self.push_dummy_scope(ScopeKind::Unquoted);

        if rest.is_empty() && allow_trailing_block {
            return Ok((
                Rc::new(MetaExpr::IfExpr {
                    span: if_span,
                    condition,
                    body: Vec::new(),
                    else_expr: None,
                }),
                rest,
                Some(TrailingBlockKind::If),
            ));
        }

        // Parse body
        let Some(TokenTree::Group(body_group)) = rest.first() else {
            self.error(
                rest.first().unwrap_or(tokens.last().unwrap()).span(),
                "expected if expression body",
            );
            return Err(());
        };

        if body_group.delimiter() != Delimiter::Brace {
            self.error(
                body_group.span(),
                "expected braces around if expression body",
            );
            return Err(());
        }

        let body_tokens = body_group.stream().into_vec();

        let body =
            self.parse_body_deny_trailing(body_group.span(), &body_tokens);

        self.scopes.pop();

        let mut rest = &rest[1..];

        let mut has_else = false;
        if let Some(TokenTree::Ident(i)) = rest.first() {
            has_else = i.to_string() == "else";
        }

        if !has_else || (rest.len() == 1 && allow_trailing_block) {
            return Ok((
                Rc::new(MetaExpr::IfExpr {
                    span: if_span,
                    condition,
                    body,
                    else_expr: None,
                }),
                rest,
                if has_else {
                    Some(TrailingBlockKind::Else)
                } else {
                    None
                },
            ));
        }

        let (else_expr, trailing_block);

        let Some(tok) = rest.get(1) else {
            self.error(rest[0].span(), "expected `if` or block after `else`");
            return Err(());
        };
        if let TokenTree::Ident(i) = tok {
            if i.to_string() != "if" {
                self.error(i.span(), "expected `if` or block after `else`");
                return Err(());
            }
            let (expr, rest_if, trailing_block_if) =
                self.parse_if(i.span(), &rest[2..], allow_trailing_block)?;
            rest = rest_if;
            else_expr = Some(expr);
            trailing_block = trailing_block_if;
        } else {
            let mut block = None;
            if let TokenTree::Group(g) = tok {
                if g.delimiter() == Delimiter::Brace {
                    block = Some(g);
                }
            };
            let Some(block) = block else {
                self.error(tok.span(), "expected `if` or block after `else`");
                return Err(());
            };
            let block_content = block.stream().into_vec();
            rest = &rest[2..];
            else_expr = Some(Rc::new(MetaExpr::Scope {
                span: block.span(),
                body: self
                    .parse_body_deny_trailing(block.span(), &block_content),
            }));
            trailing_block = None;
        }

        Ok((
            Rc::new(MetaExpr::IfExpr {
                span: if_span,
                condition,
                body,
                else_expr,
            }),
            rest,
            trailing_block,
        ))
    }

    fn parse_pattern_group(
        &mut self,
        parent_span: Span,
        group: &Group,
    ) -> Pattern {
        let tokens = group.stream().into_vec();
        let mut patterns = Vec::new();
        let mut rest = &tokens[..];

        let mut final_comma = None;

        while !rest.is_empty() {
            let Ok((pat, new_rest)) = self.parse_pattern(parent_span, rest)
            else {
                break;
            };
            patterns.push(pat);
            rest = new_rest;

            let Some(first) = rest.first() else {
                break;
            };
            if let TokenTree::Punct(p) = first {
                if p.as_char() == ',' {
                    rest = &rest[1..];
                    if rest.is_empty() {
                        final_comma = Some(p.span());
                        break;
                    }
                    continue;
                }
            };
            self.error(
                first.span(),
                format!(
                    "expected `,` or {}",
                    delimiter_chars(group.delimiter()).1
                ),
            );
        }

        match group.delimiter() {
            Delimiter::Parenthesis => {
                if patterns.len() == 1 && final_comma.is_none() {
                    return patterns.into_iter().next().unwrap();
                }
                Pattern::Tuple {
                    span: group.span(),
                    elems: patterns,
                }
            }
            Delimiter::Bracket => Pattern::List {
                span: group.span(),
                elems: patterns,
            },
            Delimiter::Brace | Delimiter::None => {
                self.error(group.span(), "invalid pattern");
                Pattern::List {
                    span: parent_span,
                    elems: Vec::new(),
                }
            }
        }
    }

    fn parse_pattern<'a>(
        &mut self,
        parent_span: Span,
        tokens: &'a [TokenTree],
    ) -> Result<(Pattern, &'a [TokenTree])> {
        if tokens.is_empty() {
            self.error(parent_span, "unexpected end of input".to_owned());
            return Err(());
        }
        match &tokens[0] {
            TokenTree::Ident(ident) => {
                let mut name = ident.to_string();
                let mut rest = &tokens[1..];
                let mut span = ident.span();
                let mut mutable = false;
                let mut super_bound =
                    name.chars().next().unwrap().is_uppercase();
                if &*name == "super" {
                    if let Some(TokenTree::Ident(ident)) = tokens.get(1) {
                        name = ident.to_string();
                        span = ident.span();
                        rest = &rest[1..];
                        super_bound = true;
                    } else {
                        self.error(
                            span,
                            "`super` is a keyword and not a valid identifier",
                        );
                        return Err(());
                    }
                }
                if &*name == "mut" {
                    if let Some(TokenTree::Ident(ident)) = tokens.get(1) {
                        name = ident.to_string();
                        span = ident.span();
                        rest = &rest[1..];
                        mutable = true;
                    } else {
                        self.error(
                            span,
                            "`mut` is a keyword and not a valid identifier",
                        );
                        return Err(());
                    }
                }
                if &*name == "super" {
                    self.error(span, "`super` must come before `mut`");
                    return Err(());
                }
                Ok((
                    Pattern::Ident(BindingParameter {
                        span: ident.span(),
                        name: Rc::from(name),
                        super_bound,
                        mutable,
                    }),
                    rest,
                ))
            }
            TokenTree::Group(group) => Ok((
                self.parse_pattern_group(parent_span, group),
                &tokens[1..],
            )),
            _ => {
                self.error(
                    tokens[0].span(),
                    "invalid token, expected pattern".to_owned(),
                );
                Err(())
            }
        }
    }

    fn parse_tuple_or_expr(
        &mut self,
        parent_span: Span,
        tokens: &[TokenTree],
    ) -> Result<Rc<MetaExpr>> {
        let mut exprs = Vec::new();
        let mut rest = tokens;

        while !rest.is_empty() {
            let (expr, new_rest) =
                self.parse_expr_deny_trailing_block(parent_span, rest)?;

            if new_rest.is_empty() && exprs.is_empty() {
                return Ok(expr);
            }
            exprs.push(expr);
            rest = new_rest;

            let Some(first) = rest.first() else {
                break;
            };
            if let TokenTree::Punct(p) = first {
                if p.as_char() == ',' {
                    rest = &rest[1..];
                    if rest.is_empty() {
                        break;
                    }
                    continue;
                }
            };
            self.error(
                first.span(),
                format!(
                    "syntax error{}: expected comma or `)`",
                    if exprs.len() > 1 { " in tuple" } else { "" }
                ),
            );
            return Err(());
        }

        Ok(Rc::new(MetaExpr::Tuple {
            span: parent_span,
            exprs,
        }))
    }

    fn parse_comma_separated(
        &mut self,
        kind: Option<&'static str>,
        delimiter: Delimiter,
        parent_span: Span,
        tokens: &[TokenTree],
    ) -> Result<Vec<Rc<MetaExpr>>> {
        let mut exprs = Vec::new();
        let mut rest = tokens;

        while !rest.is_empty() {
            let (expr, new_rest) =
                self.parse_expr_deny_trailing_block(parent_span, rest)?;
            exprs.push(expr);
            rest = new_rest;

            let Some(first) = rest.first() else {
                break;
            };
            if let TokenTree::Punct(p) = first {
                if p.as_char() == ',' {
                    rest = &rest[1..];
                    if rest.is_empty() {
                        break;
                    }
                    continue;
                }
            };
            self.error(
                first.span(),
                format!(
                    "syntax error{}: expected comma or `{}`",
                    kind.map(|k| format!(" in {k}")).unwrap_or_default(),
                    delimiter_chars(delimiter).1
                ),
            );
            return Err(());
        }

        Ok(exprs)
    }

    pub fn parse_body_deny_trailing(
        &mut self,
        parent_span: Span,
        tokens: &[TokenTree],
    ) -> Vec<Rc<MetaExpr>> {
        self.parse_body(parent_span, tokens, false).0
    }

    // syntax errors are contained withing the body
    // we return all the exprs that we were able to parse
    pub fn parse_body<'a>(
        &mut self,
        parent_span: Span,
        tokens: &'a [TokenTree],
        allow_trailing_block: bool,
    ) -> (
        Vec<Rc<MetaExpr>>,
        &'a [TokenTree],
        Option<TrailingBlockKind>,
    ) {
        let mut exprs = Vec::new();
        let mut rest = tokens;
        let mut pending_trailing_semi_error = None;

        while !rest.is_empty() {
            let Ok((expr, new_rest, trailing_block)) =
                self.parse_expr(parent_span, rest, allow_trailing_block)
            else {
                break;
            };

            // we have delay this error by once iteration because if the
            // expression after the one with the missing semi
            // might be an `[</unquote>]`, in qhich case it was fine
            // to drop the semi
            if let Some(tse) = pending_trailing_semi_error {
                if trailing_block != Some(TrailingBlockKind::Unquote) {
                    self.error(tse, "missing semicolon after expression");
                }
                pending_trailing_semi_error = None;
            }

            rest = new_rest;
            exprs.push(expr);

            if trailing_block.is_some() {
                return (exprs, rest, trailing_block);
            }

            if let Some(first) = rest.first() {
                let mut is_semi = false;
                if let TokenTree::Punct(p) = first {
                    if p.as_char() == ';' {
                        is_semi = true;
                    }
                }
                // TODO: define exceptions that may drop the semi
                if !is_semi {
                    pending_trailing_semi_error = Some(first.span());
                } else {
                    rest = &rest[1..];
                }
            }
        }
        (exprs, &[], None)
    }

    pub fn close_expr_after_trailing_body(
        &mut self,
        exprs: &mut [Rc<MetaExpr>],
        kind: TrailingBlockKind,
        contents: Vec<Rc<MetaExpr>>,
    ) {
        // This function assumes that we successfully parsed an syntactical
        // element with a trailing block before this.
        // If that's not what it finds thats a logic error.
        let expr = exprs.last_mut().unwrap();
        let expr = Rc::get_mut(expr).unwrap();

        match kind {
            TrailingBlockKind::For => {
                let MetaExpr::ForExpansion { body, .. } = expr else {
                    unreachable!()
                };
                *body = contents;
                self.scopes.pop();
            }
            TrailingBlockKind::If => {
                let MetaExpr::IfExpr { body, .. } = expr else {
                    unreachable!()
                };
                *body = contents;
                self.scopes.pop();
            }
            TrailingBlockKind::Let => {
                let MetaExpr::LetBinding {
                    expr: let_expr,
                    span,
                    ..
                } = expr
                else {
                    unreachable!()
                };
                // TODO: this span is wrong but Span::join is not stable...
                *let_expr = Some(Rc::new(MetaExpr::Scope {
                    span: *span,
                    body: contents,
                }))
            }
            TrailingBlockKind::Fn => {
                let MetaExpr::FnDecl(fd) = expr else {
                    unreachable!()
                };
                // even if this function is recursive,
                // we don't lookup identifiers until evaluation
                // so during parsing this rc will always have a refcount of one
                Rc::get_mut(fd).unwrap().body = contents;
                self.scopes.pop();
            }
            TrailingBlockKind::Else => unreachable!(),
            TrailingBlockKind::Unquote
            | TrailingBlockKind::Quote
            | TrailingBlockKind::Raw => {
                let MetaExpr::Scope { body, .. } = expr else {
                    unreachable!()
                };
                *body = contents;
                self.scopes.pop();
            }
        }
    }

    pub fn parse_raw_block_to_exprs(
        &mut self,
        parent_span: Span,
        tokens: &[TokenTree],
    ) -> Result<Vec<Rc<MetaExpr>>> {
        match self.parse_raw_block_deny_unmatched(parent_span, tokens)? {
            None => Ok(vec![Rc::new(MetaExpr::Literal {
                span: parent_span,
                value: Rc::new(MetaValue::Tokens(tokens.to_vec())),
            })]),
            Some(exprs) => Ok(exprs),
        }
    }

    fn parse_raw_block_deny_unmatched(
        &mut self,
        parent_span: Span,
        tokens: &[TokenTree],
    ) -> Result<Option<Vec<Rc<MetaExpr>>>> {
        match self.parse_raw_block(parent_span, tokens) {
            Ok(RawBodyParseResult::Plain) => Ok(None),
            Ok(RawBodyParseResult::Complete(exprs)) => Ok(Some(exprs)),
            Ok(RawBodyParseResult::UnmatchedEnd {
                span,
                kind,
                offset: _,
                contents: _,
            }) => {
                self.error(
                    span,
                    format!("unmatched closing tag `/{}`", kind.to_str()),
                );
                Err(())
            }
            Err(()) => Err(()),
        }
    }

    fn parse_closing_tag(
        &mut self,
        group: &Group,
        template_tokens: &[TokenTree],
    ) -> Result<TrailingBlockKind> {
        let Some(TokenTree::Ident(ident)) = template_tokens.get(1) else {
            self.error(
                template_tokens
                    .get(1)
                    .map(|t| t.span())
                    .unwrap_or(group.span()),
                "closing tag expects an identifier after `/`",
            );
            return Err(());
        };
        let tag = ident.to_string();
        let kind = match &*tag {
            "for" => TrailingBlockKind::For,
            "let" => TrailingBlockKind::Let,
            "fn" => TrailingBlockKind::Fn,
            "quote" => TrailingBlockKind::Quote,
            "unquote" => TrailingBlockKind::Unquote,
            "raw" => TrailingBlockKind::Raw,
            "if" => TrailingBlockKind::If,
            "else" => TrailingBlockKind::Else,
            _ => {
                self.error(
                    ident.span(),
                    format!("unknown closing tag `{tag}`"),
                );
                return Err(());
            }
        };
        if let Some(tok) = template_tokens.get(3) {
            self.error(tok.span(), "stray token after closing tag");
        }
        Ok(kind)
    }

    fn parse_expand_attrib<'a>(
        &mut self,
        expand_kind: ExpandKind,
        tokens: &'a [TokenTree],
        offset: usize,
        group: &Group,
        group_tokens: Vec<TokenTree>,
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree])> {
        let parent_rest = &tokens[offset + 1..];

        let mut paren_group = None;
        if let Some(TokenTree::Group(inner)) = group_tokens.get(1) {
            if inner.delimiter() == Delimiter::Parenthesis {
                paren_group = Some(inner);
            }
        };
        let Some(paren_group) = paren_group else {
            self.error(
                group_tokens.get(1).unwrap_or(&group_tokens[0]).span(),
                "expected `(` after `expand`",
            );
            return Ok((self.empty_token_list_expr.clone(), parent_rest));
        };

        if let Some(stray) = group_tokens.get(2) {
            self.error(stray.span(), "stray token after `expand(..)`");
        }

        let expand_inner = paren_group.stream().into_vec();

        let error_count_before_body = self.errors.len();

        self.push_dummy_scope(ScopeKind::Unquoted);

        let (mut exprs, rest, trailing_block) =
            self.parse_body(Span::call_site(), &expand_inner, true);

        let errors_in_body = error_count_before_body != self.errors.len();

        if !rest.is_empty() {
            if !errors_in_body {
                let tb = trailing_block.expect("rest without trailing block");
                self.error(
                    expand_inner[expand_inner.len() - rest.len() - 1].span(),
                    format!("template tag `{}` is never closed", tb.to_str()),
                );
            }
            self.scopes.pop();
            return Err(());
        }

        let Some(trailing_block) = trailing_block else {
            if !errors_in_body {
                self.error(
                    exprs.last().map(|e| e.span()).unwrap_or(group.span()),
                    "expand ignores the attribute body",
                );
            }
            self.scopes.pop();
            return Ok((self.empty_token_list_expr.clone(), parent_rest));
        };

        let Some(match_arm_ends) = find_match_arm_bounds(parent_rest) else {
            self.error(
                parent_rest
                    .first()
                    .map(|t| t.span())
                    .unwrap_or(group.span()),
                "invalid match arm",
            );
            return Err(());
        };

        match expand_kind {
            ExpandKind::ExpandFull => {
                let contents = self.parse_raw_block_to_exprs(
                    group.span(),
                    &parent_rest[0..match_arm_ends.body_end],
                );
                self.scopes.pop();
                self.close_expr_after_trailing_body(
                    &mut exprs,
                    trailing_block,
                    contents?,
                );
            }
            ExpandKind::ExpandPattern => {
                if trailing_block != TrailingBlockKind::For {
                    self.error(
                        group.span(),
                        "expand_pattern only works with a trailing `for` expression"
                    );
                }

                let pat = self.parse_raw_block_to_exprs(
                    group.span(),
                    &parent_rest[0..match_arm_ends.pattern_end],
                );
                let guard_plus_fat_arrow = self.parse_raw_block_to_exprs(
                    group.span(),
                    &parent_rest[match_arm_ends.pattern_end
                        ..match_arm_ends.guard_end + 2],
                );
                let body = self.parse_raw_block_to_exprs(
                    group.span(),
                    &parent_rest[match_arm_ends.guard_end + 2
                        ..match_arm_ends.body_end],
                );
                self.scopes.pop();

                let MetaExpr::ForExpansion {
                    span: _,
                    pattern: for_pattern,
                    variants_expr: for_expr,
                    body: _,
                } = Rc::into_inner(exprs.pop().unwrap()).unwrap()
                else {
                    unreachable!()
                };
                exprs.push(Rc::new(MetaExpr::ExpandPattern(Box::new(
                    ExpandPattern {
                        span: group.span(),
                        for_pattern,
                        for_expr,
                        match_arm_patterns: pat?,
                        match_arm_guard: guard_plus_fat_arrow?,
                        match_arm_body: body?,
                    },
                ))));
            }
        };

        Ok((
            Rc::new(MetaExpr::Scope {
                span: group.span(),
                body: exprs,
            }),
            &parent_rest[match_arm_ends.body_end..],
        ))
    }

    fn parse_raw_block(
        &mut self,
        parent_span: Span,
        tokens: &[TokenTree],
    ) -> Result<RawBodyParseResult> {
        let scope_kind = self.scopes.last().as_ref().unwrap().kind;

        let mut exprs = Vec::new();

        let mut raw_token_list_start = 0;

        let mut i = 0;

        let mut last_hash = tokens.len();

        while i < tokens.len() {
            let offset = i;
            i += 1;
            let t = &tokens[offset];
            if scope_kind == ScopeKind::Metamatch {
                if let TokenTree::Punct(p) = t {
                    if p.as_char() == '#' {
                        last_hash = offset;
                    }
                    continue;
                }
            }
            if scope_kind != ScopeKind::Raw {
                if let TokenTree::Ident(ident) = t {
                    let name = ident.to_string();
                    if self.lookup(&name, true).is_some() {
                        append_token_list(
                            &mut exprs,
                            tokens,
                            raw_token_list_start,
                            offset,
                        );
                        exprs.push(Rc::new(MetaExpr::Ident {
                            span: ident.span(),
                            name: Rc::from(name),
                        }));
                        raw_token_list_start = i;
                    }
                }
            }
            let TokenTree::Group(group) = t else {
                continue;
            };
            let is_bracketed = group.delimiter() == Delimiter::Bracket;

            let group_tokens = group.stream().into_vec();

            let is_template =
                is_bracketed && has_template_angle_backets(&group_tokens);

            if scope_kind == ScopeKind::Metamatch
                && last_hash + 1 == offset
                && !is_template
            {
                if let Some(TokenTree::Ident(ident)) = group_tokens.first() {
                    let attrib_name = ident.to_string();
                    let expand_kind = match &*attrib_name {
                        "expand" => Some(ExpandKind::ExpandFull),
                        "expand_pattern" => Some(ExpandKind::ExpandPattern),
                        _ => None,
                    };
                    if let Some(expand_kind) = expand_kind {
                        append_token_list(
                            &mut exprs,
                            tokens,
                            raw_token_list_start,
                            last_hash,
                        );
                        let (expr, rest) = self.parse_expand_attrib(
                            expand_kind,
                            tokens,
                            offset,
                            group,
                            group_tokens,
                        )?;
                        exprs.push(expr);
                        i = tokens.len() - rest.len();
                        raw_token_list_start = i;
                        continue;
                    }
                }
            }

            if !is_template {
                match self.parse_raw_block_deny_unmatched(
                    group.span(),
                    &group_tokens,
                ) {
                    Err(()) | Ok(None) => (),
                    Ok(Some(body_exprs)) => {
                        append_token_list(
                            &mut exprs,
                            tokens,
                            raw_token_list_start,
                            offset,
                        );
                        exprs.push(Rc::new(MetaExpr::RawOutputGroup {
                            span: group.span(),
                            delimiter: group.delimiter(),
                            contents: body_exprs,
                        }));
                        raw_token_list_start = i;
                    }
                }
                continue;
            }
            let inner_tokens = &group_tokens[1..group_tokens.len() - 1];
            if let Some(res) = self.handle_template_in_raw_block(
                parent_span,
                tokens,
                offset,
                &mut raw_token_list_start,
                &mut i,
                group,
                inner_tokens,
                &mut exprs,
            )? {
                return Ok(res);
            }
        }
        if raw_token_list_start == 0 {
            debug_assert!(exprs.is_empty());
            return Ok(RawBodyParseResult::Plain);
        }
        if i != raw_token_list_start {
            exprs.push(Rc::new(MetaExpr::Literal {
                span: tokens[raw_token_list_start].span(),
                value: Rc::new(MetaValue::Tokens(
                    tokens[raw_token_list_start..i].to_vec(),
                )),
            }));
        }
        Ok(RawBodyParseResult::Complete(exprs))
    }

    #[allow(clippy::too_many_arguments)] // dude stop shaming my code ik this is not ideal
    fn handle_template_in_raw_block(
        &mut self,
        block_parent_span: Span,
        block_tokens: &[TokenTree],
        offset_in_block: usize,
        raw_token_list_start: &mut usize,
        block_continuation: &mut usize,
        group: &Group,
        template_inner_tokens: &[TokenTree],
        exprs: &mut Vec<Rc<MetaExpr>>,
    ) -> Result<Option<RawBodyParseResult>> {
        let is_raw_block =
            self.scopes.last().as_ref().unwrap().kind == ScopeKind::Raw;
        let first_tok = template_inner_tokens.first();
        if let Some(TokenTree::Ident(ident)) = first_tok {
            if !is_raw_block && ident.to_string() == "else" {
                append_token_list(
                    exprs,
                    block_tokens,
                    *raw_token_list_start,
                    offset_in_block,
                );
                return Ok(Some(RawBodyParseResult::UnmatchedEnd {
                    span: group.span(),
                    kind: TrailingBlockKind::Else,
                    offset: offset_in_block,
                    contents: std::mem::take(exprs),
                }));
            }
        }
        if let Some(TokenTree::Punct(p)) = first_tok {
            if p.as_char() == '/' {
                // this is a closing tag
                let end_tag =
                    self.parse_closing_tag(group, template_inner_tokens)?;
                if !is_raw_block || end_tag == TrailingBlockKind::Raw {
                    append_token_list(
                        exprs,
                        block_tokens,
                        *raw_token_list_start,
                        offset_in_block,
                    );
                    return Ok(Some(RawBodyParseResult::UnmatchedEnd {
                        span: group.span(),
                        kind: end_tag,
                        offset: offset_in_block,
                        contents: std::mem::take(exprs),
                    }));
                }
            }
        }
        if is_raw_block {
            *block_continuation += offset_in_block + 1;
            return Ok(None);
        }
        append_token_list(
            exprs,
            block_tokens,
            *raw_token_list_start,
            offset_in_block,
        );
        *raw_token_list_start = *block_continuation;

        let (expr, rest, trailing_block) =
            self.parse_body(group.span(), template_inner_tokens, true);
        debug_assert!(rest.is_empty());
        exprs.extend(expr);
        let Some(trailing_block) = trailing_block else {
            return Ok(None);
        };

        let continuation = &block_tokens[*block_continuation..];

        if trailing_block == TrailingBlockKind::Unquote {
            let (contents, rest, trailing_block_kind) =
                self.parse_body(block_parent_span, continuation, false);

            let tokens_consumed = continuation.len() - rest.len();

            let Some(trailing_block_kind) = trailing_block_kind else {
                self.error(group.span(), "`unquote` tag is never terminated");
                return Err(());
            };
            if trailing_block_kind != TrailingBlockKind::Unquote {
                let continuation_tag_span =
                    continuation[tokens_consumed - 1].span();
                self.error(
                    continuation_tag_span,
                    format!(
                        "unexpected end tag `{}`",
                        trailing_block_kind.to_str()
                    ),
                );
                return Err(());
            }

            self.close_expr_after_trailing_body(
                exprs,
                trailing_block_kind,
                contents,
            );
            *block_continuation += tokens_consumed;
            *raw_token_list_start = *block_continuation;
            return Ok(None);
        }
        match self.parse_raw_block(block_parent_span, continuation)? {
            RawBodyParseResult::Plain | RawBodyParseResult::Complete(_) => {
                self.error(
                    group.span(),
                    format!(
                        "missing closing tag for `{}`",
                        trailing_block.to_str()
                    ),
                );
                Err(())
            }
            RawBodyParseResult::UnmatchedEnd {
                span,
                kind,
                offset,
                contents,
            } => {
                let mut wrong_tag = kind != trailing_block;
                if trailing_block == TrailingBlockKind::If
                    && kind == TrailingBlockKind::Else
                {
                    let TokenTree::Group(_else_tag) =
                        &block_tokens[*block_continuation + offset]
                    else {
                        unreachable!()
                    };
                    // TODO: handle else expr
                    wrong_tag = false;
                }
                if wrong_tag {
                    self.error(
                        span,
                        format!(
                            "expected closing tag for `{}`, found `{}`",
                            trailing_block.to_str(),
                            kind.to_str()
                        ),
                    );
                    return Err(());
                }
                self.close_expr_after_trailing_body(exprs, kind, contents);
                *block_continuation += offset + 1;
                *raw_token_list_start = *block_continuation;
                Ok(None)
            }
        }
    }
}
