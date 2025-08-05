use proc_macro::{Delimiter, Group, Spacing, Span, TokenTree};
use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc};

use super::{
    ast::{
        BinaryOpKind, Binding, BindingParameter, Context, ExpandPattern,
        Function, Lambda, MetaError, MetaExpr, MetaValue, Pattern, Scope,
        ScopeKind, TrailingBlockKind, UnaryOpKind, UseDecl, UsePath,
        UseSegment, UseTree,
    },
    macro_impls::IntoVec,
};

type Result<T> = std::result::Result<T, ()>;

pub enum RawBodyParseResult {
    // in a raw block the template might not get special treatment at all
    ParseRaw,
    Complete(Vec<Rc<MetaExpr>>),
    UnmatchedEnd {
        span: Span,
        kind: TrailingBlockKind,
        offset: usize,
        contents: Vec<Rc<MetaExpr>>,
    },
}

pub enum TemplateInRawParseResult {
    // Template is not matched at all. Happens in Raw blocks.
    Ignore,
    ExprsAdded,
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

fn starts_with_uppercase(name: &str) -> bool {
    name.chars()
        .next()
        .map(|c| c.is_uppercase())
        .unwrap_or(false)
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
    if start < end {
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
        if s.starts_with('\'') {
            let mut chars = s.chars();
            if let (Some('\''), Some(c), Some('\''), None) =
                (chars.next(), chars.next(), chars.next(), chars.next())
            {
                return Rc::new(MetaValue::Char {
                    value: c,
                    span: Some(token.span()),
                });
            }
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

impl Default for Context {
    fn default() -> Self {
        let empty_token_list = Rc::new(MetaValue::Tokens(Vec::new()));
        let mut ctx = Self {
            empty_token_list_expr: Rc::new(MetaExpr::Literal {
                span: Span::call_site(),
                value: empty_token_list.clone(),
            }),
            empty_token_list,
            scopes: vec![Scope {
                kind: ScopeKind::Builtin,
                bindings: HashMap::new(),
            }],
            extern_decls: Vec::new(),
            extern_uses: Vec::new(),
            errors: Vec::new(),
        };
        ctx.insert_builtins();
        ctx
    }
}

impl Context {
    pub fn is_top_level_scope(&self) -> bool {
        // builtin + top level user scope
        self.scopes.len() == 2
    }

    pub fn push_dummy_scope(&mut self, kind: ScopeKind) {
        self.scopes.push(Scope {
            kind,
            bindings: HashMap::new(),
        });
    }
    pub fn pop_scope(&mut self) -> Option<Scope> {
        debug_assert!(self.scopes.len() > 1);
        self.scopes.pop()
    }

    fn parse_expr_deny_rest(
        &mut self,
        parent_span: Span,
        tokens: &[TokenTree],
        min_prec: u8,
    ) -> Result<Rc<MetaExpr>> {
        let (expr, rest) = self.parse_expr_deny_trailing_block(
            parent_span,
            tokens,
            min_prec,
        )?;
        if let Some(stray) = rest.first() {
            self.error(stray.span(), "stray token after expression");
        }
        Ok(expr)
    }

    fn parse_expr_deny_trailing_block<'a>(
        &mut self,
        parent_span: Span,
        tokens: &'a [TokenTree],
        min_prec: u8,
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree])> {
        let (expr, rest, _trailing_block) =
            self.parse_expr(parent_span, tokens, false, min_prec)?;
        Ok((expr, rest))
    }

    fn parse_expr<'a>(
        &mut self,
        parent_span: Span,
        tokens: &'a [TokenTree],
        allow_trailing_block: bool,
        min_prec: u8,
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        let (mut lhs, mut rest, mut trailing) = self.parse_expr_value(
            parent_span,
            tokens,
            allow_trailing_block,
            min_prec,
        )?;

        loop {
            if rest.is_empty()
                || trailing.is_some()
                || lhs.may_drop_semicolon()
            {
                return Ok((lhs, rest, trailing));
            }

            if let Some(TokenTree::Group(g)) = rest.first() {
                // List Access
                if g.delimiter() == Delimiter::Bracket {
                    let inner = g.stream().into_vec();
                    if has_template_angle_backets(&inner) {
                        return Ok((lhs, rest, trailing));
                    }
                    rest = &rest[1..];
                    let index = self
                        .parse_expr_deny_rest(g.span(), &inner, 1)
                        .unwrap_or(self.empty_token_list_expr.clone());
                    lhs = Rc::new(MetaExpr::ListAccess {
                        span: g.span(),
                        list: lhs,
                        index,
                    });
                    continue;
                }
                // Call
                if g.delimiter() == Delimiter::Parenthesis {
                    let params = g.stream().into_vec();
                    let fn_args = self.parse_comma_separated(
                        Some("function call arguments"),
                        Delimiter::Parenthesis,
                        g.span(),
                        &params,
                    )?;
                    lhs = Rc::new(MetaExpr::Call {
                        span: g.span(),
                        lhs,
                        args: fn_args,
                    });
                    rest = &rest[1..];
                    continue;
                }
            }

            // UFCS
            if let Some(TokenTree::Punct(p)) = rest.first() {
                if p.as_char() == '.' && p.spacing() == Spacing::Alone {
                    if let Some((
                        TokenTree::Ident(func_name),
                        TokenTree::Group(param_group),
                    )) = rest.get(1..=2).map(|s| (&s[0], &s[1]))
                    {
                        if param_group.delimiter() == Delimiter::Parenthesis {
                            let params = param_group.stream().into_vec();
                            let mut fn_args = self.parse_comma_separated(
                                Some("function call arguments"),
                                Delimiter::Parenthesis,
                                param_group.span(),
                                &params,
                            )?;
                            fn_args.insert(0, lhs);
                            lhs = Rc::new(MetaExpr::Call {
                                span: func_name.span(),
                                lhs: Rc::from(MetaExpr::Ident {
                                    span: func_name.span(),
                                    name: Rc::from(func_name.to_string()),
                                }),
                                args: fn_args,
                            });
                            rest = &rest[3..];
                            continue;
                        }
                    }
                }
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

            let (rhs, rest_new, trailing_new) = self.parse_expr(
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
                if end_tag == TrailingBlockKind::Eval {
                    return Ok((
                        self.empty_token_list_expr.clone(),
                        rest_tokens,
                        Some(end_tag),
                    ));
                } else {
                    self.error_unexpected_end_tag(group.span(), end_tag);
                    return Err(());
                }
            }
        }

        self.error(
            group.span(),
            "template tags are not supported in eval mode",
        );
        Err(())
    }
    fn parse_expr_value<'a>(
        &mut self,
        parent_span: Span,
        tokens: &'a [TokenTree],
        allow_trailing_block: bool,
        min_prec: u8,
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
                                0,
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
                    Delimiter::Brace => {
                        self.push_dummy_scope(ScopeKind::Eval);
                        let expr_block = self.parse_body_deny_trailing(
                            group.span(),
                            &group_tokens,
                        );
                        self.pop_scope();
                        Ok((
                            Rc::new(MetaExpr::Scope {
                                span: group.span(),
                                body: expr_block,
                            }),
                            &tokens[1..],
                            None,
                        ))
                    }
                }
            }

            TokenTree::Ident(ident) => {
                let span = ident.span();
                let name = Rc::from(ident.to_string());

                match &*name {
                    "let" => {
                        if min_prec != 0 {
                            self.error(
                                ident.span(),
                                "`let` cannot occur within expression",
                            );
                            return Err(());
                        }
                        return self.parse_let(
                            span,
                            &tokens[1..],
                            false,
                            allow_trailing_block,
                        );
                    }
                    "for" => {
                        return self.parse_for(
                            span,
                            &tokens[1..],
                            allow_trailing_block,
                        );
                    }
                    "while" => {
                        return self.parse_while(
                            span,
                            &tokens[1..],
                            allow_trailing_block,
                        );
                    }
                    "break" => {
                        return self.parse_break(span, &tokens[1..]);
                    }
                    "continue" => {
                        return self.parse_continue(span, &tokens[1..]);
                    }
                    "loop" => {
                        return self.parse_loop(
                            span,
                            &tokens[1..],
                            allow_trailing_block,
                        );
                    }
                    "fn" => {
                        if min_prec != 0 {
                            self.error(
                                ident.span(),
                                "`fn` cannot occur within expression",
                            );
                            return Err(());
                        }
                        return self.parse_fn(
                            span,
                            &tokens[1..],
                            false,
                            allow_trailing_block,
                        );
                    }
                    "use" => {
                        if min_prec != 0 {
                            self.error(
                                ident.span(),
                                "`use` cannot occur within expression",
                            );
                            return Err(());
                        }
                        return self.parse_use_decl(span, &tokens[1..]);
                    }
                    "extern" => {
                        if min_prec != 0 {
                            self.error(
                                ident.span(),
                                "`extern` cannot occur within expression",
                            );
                            return Err(());
                        }

                        if !self.is_top_level_scope() {
                            self.error(
                                ident.span(),
                                "`extern` can only occur at the top level",
                            );
                        }

                        let mut is_fn = false;
                        let mut is_let = false;

                        if let Some(TokenTree::Ident(next)) = tokens.get(1) {
                            let s = next.to_string();
                            is_fn = s == "fn";
                            is_let = s == "let";
                        };
                        if is_fn {
                            return self.parse_fn(
                                span,
                                &tokens[2..],
                                true,
                                allow_trailing_block,
                            );
                        }
                        if is_let {
                            return self.parse_let(
                                span,
                                &tokens[2..],
                                true,
                                allow_trailing_block,
                            );
                        }
                        self.error(
                            ident.span(),
                            "`extern` must be followed by `let` or `fn`",
                        );
                        return Err(());
                    }
                    "if" => {
                        return self.parse_if(
                            span,
                            &tokens[1..],
                            allow_trailing_block,
                        );
                    }
                    "quote" => {
                        return self.parse_quote_expr(span, &tokens[1..])
                    }
                    "raw" => {
                        return self.parse_raw_expr(
                            span,
                            &tokens[1..],
                            allow_trailing_block,
                        )
                    }
                    "eval" => {
                        return self.parse_eval_expr(
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
                                Rc::new(MetaExpr::Call {
                                    span,
                                    lhs: Rc::new(MetaExpr::Ident {
                                        span: ident.span(),
                                        name,
                                    }),
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
                if p.as_char() == '|' {
                    let (expr, rest) =
                        self.parse_lambda(p.span(), &tokens[1..])?;
                    return Ok((expr, rest, None));
                }
                if let Some(op_kind) = as_unary_operator(p) {
                    let (operand, rest, _tb) = self.parse_expr(
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

    fn parse_use_decl<'a>(
        &mut self,
        use_span: Span,
        tokens: &'a [TokenTree],
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        if tokens.is_empty() {
            self.error(use_span, "expected path after `use`");
            return Err(());
        }

        let (use_tree, rest) = self.parse_use_tree(use_span, tokens)?;

        let use_decl = Rc::new(UseDecl {
            span: use_span,
            tree: use_tree,
            replacements: RefCell::new(None),
        });

        self.extern_uses.push(use_decl.clone());

        Ok((Rc::new(MetaExpr::UseDecl(use_decl)), rest, None))
    }

    fn parse_use_group_items(
        &mut self,
        parent_span: Span,
        tokens: &[TokenTree],
    ) -> Result<Vec<UseTree>> {
        let mut items = Vec::new();
        let mut rest = tokens;

        while !rest.is_empty() {
            let (item, new_rest) = self.parse_use_tree(parent_span, rest)?;
            items.push(item);
            rest = new_rest;

            if let Some(TokenTree::Punct(p)) = rest.first() {
                if p.as_char() == ',' {
                    rest = &rest[1..];
                    // Allow trailing comma
                    if rest.is_empty() {
                        break;
                    }
                    continue;
                }
            }

            if !rest.is_empty() {
                self.error(
                    rest.first().unwrap().span(),
                    "expected `,` between use items",
                );
                return Err(());
            }
        }

        Ok(items)
    }

    fn parse_use_path<'a>(
        &mut self,
        tokens: &'a [TokenTree],
    ) -> Result<(UsePath, &'a [TokenTree])> {
        let mut segments = Vec::new();
        let mut leading_double_colon = false;
        let mut rest = tokens;

        // Check for leading ::
        if let Some(TokenTree::Punct(p1)) = rest.first() {
            if let Some(TokenTree::Punct(p2)) = rest.get(1) {
                if p1.as_char() == ':' && p2.as_char() == ':' {
                    leading_double_colon = true;
                    rest = &rest[2..];
                }
            }
        }

        // Parse path segments
        while let Some(TokenTree::Ident(ident)) = rest.first() {
            let name = ident.to_string();
            let segment = match &*name {
                "self" => UseSegment::SelfKeyword,
                "super" => UseSegment::SuperKeyword,
                "crate" => UseSegment::CrateKeyword,
                // don't interpret rename as path
                "as" => break,
                _ => UseSegment::Ident(Rc::from(name)),
            };
            segments.push(segment);
            rest = &rest[1..];

            // Check for trailing ::
            if let Some(TokenTree::Punct(p1)) = rest.first() {
                if let Some(TokenTree::Punct(p2)) = rest.get(1) {
                    if p1.as_char() == ':' && p2.as_char() == ':' {
                        rest = &rest[2..];
                        continue;
                    }
                }
            }

            // No more :: found, we're done with path segments
            break;
        }

        Ok((
            UsePath {
                leading_double_colon,
                segments,
            },
            rest,
        ))
    }

    fn parse_use_tree<'a>(
        &mut self,
        parent_span: Span,
        tokens: &'a [TokenTree],
    ) -> Result<(UseTree, &'a [TokenTree])> {
        let (path, rest) = self.parse_use_path(tokens)?;

        if let Some(TokenTree::Group(group)) = rest.first() {
            if group.delimiter() == Delimiter::Brace {
                // Group import: `path::{item1, item2}`
                let group_tokens = group.stream().into_vec();
                let items =
                    self.parse_use_group_items(group.span(), &group_tokens)?;
                return Ok((
                    UseTree::Group {
                        span: parent_span,
                        path,
                        items,
                    },
                    &rest[1..],
                ));
            }
        }

        if let Some(TokenTree::Ident(ident)) = rest.first() {
            if ident.to_string() == "as" {
                // Rename: `path as alias`
                if rest.len() < 2 {
                    self.error(ident.span(), "expected identifier after `as`");
                    return Err(());
                }
                let Some((_, alias)) = parse_ident(&rest[1]) else {
                    self.error(
                        rest[1].span(),
                        "expected identifier after `as`",
                    );
                    return Err(());
                };

                if path.segments.is_empty() {
                    self.error(parent_span, "expected path before `as`");
                    return Err(());
                }

                return Ok((
                    UseTree::Rename {
                        span: parent_span,
                        path,
                        alias,
                    },
                    &rest[2..],
                ));
            }
        }

        Ok((
            UseTree::Path {
                span: parent_span,
                path,
            },
            rest,
        ))
    }

    fn parse_let<'a>(
        &mut self,
        let_span: Span,
        tokens: &'a [TokenTree],
        is_extern: bool,
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
            let let_expr = Rc::new(MetaExpr::LetBinding {
                is_extern,
                span: let_span,
                pattern,
                expr: None,
            });
            if is_extern {
                self.extern_decls.push(let_expr.clone());
            }
            return Ok((let_expr, &[], Some(TrailingBlockKind::Let)));
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
            self.parse_expr_deny_trailing_block(let_span, rest, 1)?;

        self.insert_dummy_bindings_for_pattern(&pattern);

        let let_expr = Rc::new(MetaExpr::LetBinding {
            is_extern,
            span: let_span,
            pattern,
            expr: Some(expr),
        });
        if is_extern {
            self.extern_decls.push(let_expr.clone());
        }
        Ok((let_expr, rest, None))
    }

    fn parse_quote_expr<'a>(
        &mut self,
        raw_span: Span,
        tokens: &'a [TokenTree],
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        let mut has_exclam = false;
        if let Some(TokenTree::Punct(p)) = tokens.first() {
            has_exclam = p.as_char() == '!';
        }

        if !has_exclam {
            self.error(
                tokens.first().map(|t| t.span()).unwrap_or(raw_span),
                "expected `!` after `quote`",
            );
        }

        let tokens = &tokens[usize::from(has_exclam)..];

        self.push_dummy_scope(ScopeKind::Quote);

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

        self.pop_scope();

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
                    "expected block after `template!`",
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

    fn parse_eval_expr<'a>(
        &mut self,
        raw_span: Span,
        tokens: &'a [TokenTree],
        allow_trailing_block: bool, // must be true, otherwise error
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        if !allow_trailing_block || !tokens.is_empty() {
            self.error(
                tokens.first().map(|t| t.span()).unwrap_or(raw_span),
                "`eval` is only allowed as a template block",
            );
            return Err(());
        }

        self.push_dummy_scope(ScopeKind::Eval);

        Ok((
            Rc::new(MetaExpr::Scope {
                span: raw_span,
                body: Vec::new(),
            }),
            &[],
            Some(TrailingBlockKind::Eval),
        ))
    }

    fn parse_fn<'a>(
        &mut self,
        fn_span: Span,
        tokens: &'a [TokenTree],
        is_extern: bool,
        allow_trailing_block: bool,
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        if tokens.is_empty() {
            self.error(fn_span, "expected identifier after `fn`");
        }

        let (name_span, name) = parse_ident(&tokens[0]).ok_or_else(|| {
            self.error(tokens[0].span(), "expected function name");
        })?;

        let superbound = starts_with_uppercase(&name);
        self.insert_dummy_binding(name.clone(), superbound);

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

        self.push_dummy_scope(ScopeKind::Eval);

        while !params_rest.is_empty() {
            let Ok((pat, rest)) = self.parse_pattern(fn_span, params_rest)
            else {
                break;
            };

            params.push(pat);
            params_rest = rest;

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
                        "expected `,` between parameters",
                    );
                    self.pop_scope();
                    return Err(());
                }
            }
        }

        let rest = &tokens[2..];

        let (body, rest, trailing_block) = if rest.is_empty() {
            if !allow_trailing_block {
                self.error(
                    tokens[2].span(),
                    "expected function body after parameters",
                );
                self.pop_scope();

                return Err(());
            }
            (Vec::new(), rest, Some(TrailingBlockKind::Fn))
        } else {
            let TokenTree::Group(body_group) = &rest[0] else {
                self.error(tokens[0].span(), "expected function body");
                self.pop_scope();
                return Err(());
            };

            if body_group.delimiter() != Delimiter::Brace {
                self.error(
                    body_group.span(),
                    "expected braces around function body",
                );
                self.pop_scope();
                return Err(());
            }

            let body_tokens = body_group.stream().into_vec();
            let body = self.parse_body_deny_trailing(name_span, &body_tokens);

            self.pop_scope();

            (body, &rest[1..], None)
        };

        let fn_decl = Rc::new(MetaExpr::FnDecl(Rc::new(Function {
            is_extern,
            span: name_span,
            name: name.clone(),
            params,
            body,
        })));

        if is_extern {
            self.extern_decls.push(fn_decl.clone());
        }

        Ok((fn_decl, rest, trailing_block))
    }

    fn parse_lambda<'a>(
        &mut self,
        fn_span: Span,
        tokens: &'a [TokenTree],
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree])> {
        if tokens.is_empty() {
            self.error(fn_span, "expected pattern or `|`");
        }

        let mut params = Vec::new();
        let mut rest = tokens;

        self.push_dummy_scope(ScopeKind::Eval);

        while !rest.is_empty() {
            let Ok((pat, rest_new)) = self.parse_pattern(fn_span, rest) else {
                break;
            };

            params.push(pat);
            rest = rest_new;

            if !rest.is_empty() {
                if let TokenTree::Punct(p) = &rest[0] {
                    if p.as_char() == '|' {
                        break;
                    }
                    if p.as_char() == ',' && p.spacing() == Spacing::Alone {
                        rest = &rest[1..];
                        continue;
                    }
                }
                self.error(rest[0].span(), "expected `,` between parameters");
                self.pop_scope();
                return Err(());
            }
        }

        let mut has_closing_pipe = false;
        if let Some(TokenTree::Punct(p)) = rest.first() {
            if p.as_char() == '|' {
                has_closing_pipe = true;
            }
        }
        if !has_closing_pipe {
            self.error(
                rest.first()
                    .map(|t| t.span())
                    .unwrap_or(tokens[tokens.len() - rest.len() - 1].span()),
                "expected `|` to close parameter list".to_owned(),
            );
            self.pop_scope();
            return Err(());
        }

        let Ok((body, rest)) =
            self.parse_expr_deny_trailing_block(fn_span, &rest[1..], 1)
        else {
            self.pop_scope();
            return Err(());
        };

        self.pop_scope();

        Ok((
            Rc::new(MetaExpr::Lambda(Rc::new(Lambda {
                span: fn_span,
                params,
                body,
            }))),
            rest,
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
            self.parse_expr_deny_trailing_block(for_span, &rest[1..], 1)?;

        self.push_dummy_scope(ScopeKind::Eval);
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

            self.pop_scope();
            final_rest = &rest[1..];
            trailing_block = None;
        };

        Ok((
            Rc::new(MetaExpr::For {
                span: for_span,
                pattern,
                variants_expr,
                body,
            }),
            final_rest,
            trailing_block,
        ))
    }
    fn parse_while_let<'a>(
        &mut self,
        while_span: Span,
        tokens: &'a [TokenTree],
        allow_trailing_block: bool,
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        let (pattern, rest) = self.parse_pattern(while_span, tokens)?;

        let mut has_eq = false;
        if let Some(TokenTree::Punct(p)) = rest.first() {
            has_eq = p.as_char() == '=';
        }
        if !has_eq {
            self.error(
                rest.first().map(|f| f.span()).unwrap_or(while_span),
                "expected '=' after `while let` pattern",
            );
            return Err(());
        }

        if rest.len() < 2 {
            self.error(rest[0].span(), "expected expression after '='");
            return Err(());
        }

        let last_tok = rest.last().unwrap();

        let (expr, rest) =
            self.parse_expr_deny_trailing_block(while_span, &rest[1..], 1)?;

        self.push_dummy_scope(ScopeKind::Eval);
        self.insert_dummy_bindings_for_pattern(&pattern);

        let (body, final_rest, trailing_block);
        if rest.is_empty() && allow_trailing_block {
            body = Vec::new();
            final_rest = rest;
            trailing_block = Some(TrailingBlockKind::While);
        } else {
            // Parse body
            let Some(TokenTree::Group(body_group)) = rest.first() else {
                self.error(
                    rest.first().unwrap_or(last_tok).span(),
                    "expected while loop body",
                );
                return Err(());
            };

            if body_group.delimiter() != Delimiter::Brace {
                self.error(
                    body_group.span(),
                    "expected braces around while loop body",
                );
                return Err(());
            }

            let body_tokens = body_group.stream().into_vec();

            body =
                self.parse_body_deny_trailing(body_group.span(), &body_tokens);

            self.pop_scope();
            final_rest = &rest[1..];
            trailing_block = None;
        };

        Ok((
            Rc::new(MetaExpr::WhileLet {
                span: while_span,
                pattern,
                expr,
                body,
            }),
            final_rest,
            trailing_block,
        ))
    }

    fn parse_while<'a>(
        &mut self,
        while_span: Span,
        tokens: &'a [TokenTree],
        allow_trailing_block: bool,
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        if let Some(TokenTree::Ident(i)) = tokens.first() {
            if i.to_string() == "let" {
                return self.parse_while_let(
                    while_span,
                    &tokens[1..],
                    allow_trailing_block,
                );
            }
        }

        let (condition, rest) =
            self.parse_expr_deny_trailing_block(while_span, tokens, 1)?;

        self.push_dummy_scope(ScopeKind::Eval);

        let (body, final_rest, trailing_block);
        if rest.is_empty() && allow_trailing_block {
            body = Vec::new();
            final_rest = rest;
            trailing_block = Some(TrailingBlockKind::While);
        } else {
            // Parse body
            let Some(TokenTree::Group(body_group)) = rest.first() else {
                self.error(
                    rest.first().map(|t| t.span()).unwrap_or(while_span),
                    "expected while loop body",
                );
                return Err(());
            };

            if body_group.delimiter() != Delimiter::Brace {
                self.error(
                    body_group.span(),
                    "expected braces around while loop body",
                );
                return Err(());
            }

            let body_tokens = body_group.stream().into_vec();

            body =
                self.parse_body_deny_trailing(body_group.span(), &body_tokens);

            self.pop_scope();
            final_rest = &rest[1..];
            trailing_block = None;
        };

        Ok((
            Rc::new(MetaExpr::While {
                span: while_span,
                condition,
                body,
            }),
            final_rest,
            trailing_block,
        ))
    }

    fn parse_loop<'a>(
        &mut self,
        loop_span: Span,
        tokens: &'a [TokenTree],
        allow_trailing_block: bool,
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        self.push_dummy_scope(ScopeKind::Eval);
        let (body, rest, trailing_block);
        if tokens.is_empty() && allow_trailing_block {
            body = Vec::new();
            rest = tokens;
            trailing_block = Some(TrailingBlockKind::Loop);
        } else {
            // Parse body
            let Some(TokenTree::Group(body_group)) = tokens.first() else {
                self.error(
                    tokens.first().map(|t| t.span()).unwrap_or(loop_span),
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

            self.pop_scope();
            rest = &tokens[1..];
            trailing_block = None;
        };

        Ok((
            Rc::new(MetaExpr::Loop {
                span: loop_span,
                body,
            }),
            rest,
            trailing_block,
        ))
    }

    fn parse_break<'a>(
        &mut self,
        break_span: Span,
        tokens: &'a [TokenTree],
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        let mut followed_by_semi = false;
        if let Some(TokenTree::Punct(p)) = tokens.first() {
            if p.as_char() == ';' {
                followed_by_semi = true;
            }
        }
        let mut rest = tokens;
        let mut break_val = None;
        if !tokens.is_empty() && !followed_by_semi {
            let (val, rest_new) =
                self.parse_expr_deny_trailing_block(break_span, tokens, 1)?;
            break_val = Some(val);
            rest = rest_new;
        }

        Ok((
            Rc::new(MetaExpr::Break {
                span: break_span,
                expr: break_val,
            }),
            rest,
            None,
        ))
    }

    fn parse_continue<'a>(
        &mut self,
        continue_span: Span,
        tokens: &'a [TokenTree],
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree], Option<TrailingBlockKind>)>
    {
        Ok((
            Rc::new(MetaExpr::Continue {
                span: continue_span,
            }),
            tokens,
            None,
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
            self.parse_expr_deny_trailing_block(if_span, tokens, 1)?;

        self.push_dummy_scope(ScopeKind::Eval);

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

        self.pop_scope();

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
                let mut super_bound = false;
                if &*name == "super" {
                    if let Some(TokenTree::Ident(ident)) = rest.first() {
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
                    if let Some(TokenTree::Ident(ident)) = rest.first() {
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
                super_bound = super_bound || starts_with_uppercase(&name);

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
                self.parse_expr_deny_trailing_block(parent_span, rest, 1)?;

            if new_rest.is_empty() && exprs.is_empty() {
                return Ok(Rc::new(MetaExpr::Parenthesized {
                    span: parent_span,
                    expr,
                }));
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
                    "syntax error{}: expected `,` or `)`",
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
                self.parse_expr_deny_trailing_block(parent_span, rest, 1)?;
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
                    "syntax error{}: expected `,` or `{}`",
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
                self.parse_expr(parent_span, rest, allow_trailing_block, 0)
            else {
                break;
            };

            // we have delay this error by once iteration because if the
            // expression after the one with the missing semi
            // might be an `[</eval>]`, in qhich case it was fine
            // to drop the semi
            if let Some(tse) = pending_trailing_semi_error {
                if trailing_block != Some(TrailingBlockKind::Eval) {
                    self.error(tse, "missing semicolon after expression");
                }
                pending_trailing_semi_error = None;
            }

            rest = new_rest;
            let may_drop_semi = expr.may_drop_semicolon();
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
                if !is_semi {
                    if !may_drop_semi {
                        pending_trailing_semi_error = Some(first.span());
                    }
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
                let MetaExpr::For { body, .. } = expr else {
                    unreachable!()
                };
                *body = contents;
            }
            TrailingBlockKind::While => {
                let body = match expr {
                    MetaExpr::While { body, .. } => body,
                    MetaExpr::WhileLet { body, .. } => body,
                    _ => unreachable!(),
                };
                *body = contents;
            }
            TrailingBlockKind::Loop => {
                let MetaExpr::Loop { body, .. } = expr else {
                    unreachable!()
                };
                *body = contents;
            }
            TrailingBlockKind::If => {
                let MetaExpr::IfExpr { body, .. } = expr else {
                    unreachable!()
                };
                *body = contents;
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
            }
            TrailingBlockKind::Else => unreachable!(),
            TrailingBlockKind::Eval
            | TrailingBlockKind::Template
            | TrailingBlockKind::Raw => {
                let MetaExpr::Scope { body, .. } = expr else {
                    unreachable!()
                };
                *body = contents;
            }
        }
    }

    pub fn parse_raw_block_to_exprs(
        &mut self,
        parent_span: Span,
        tokens: &[TokenTree],
    ) -> Result<Vec<Rc<MetaExpr>>> {
        match self.parse_raw_block(parent_span, tokens)? {
            RawBodyParseResult::ParseRaw => {
                Ok(vec![Rc::new(MetaExpr::Literal {
                    span: parent_span,
                    value: Rc::new(MetaValue::Tokens(tokens.to_vec())),
                })])
            }
            RawBodyParseResult::Complete(exprs) => Ok(exprs),
            RawBodyParseResult::UnmatchedEnd { span, kind, .. } => {
                self.errror_unmatched_closing_tag(span, kind);
                Err(())
            }
        }
    }

    fn parse_raw_block_deny_unmatched(
        &mut self,
        parent_span: Span,
        tokens: &[TokenTree],
    ) -> Result<Option<Vec<Rc<MetaExpr>>>> {
        match self.parse_raw_block(parent_span, tokens) {
            Ok(RawBodyParseResult::ParseRaw) => Ok(None),
            Ok(RawBodyParseResult::Complete(exprs)) => Ok(Some(exprs)),
            Ok(RawBodyParseResult::UnmatchedEnd { span, kind, .. }) => {
                self.errror_unmatched_closing_tag(span, kind);
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
            "while" => TrailingBlockKind::While,
            "let" => TrailingBlockKind::Let,
            "fn" => TrailingBlockKind::Fn,
            "template" => TrailingBlockKind::Template,
            "eval" => TrailingBlockKind::Eval,
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
            self.pop_scope();
            return Err(());
        }

        let Some(trailing_block) = trailing_block else {
            if !errors_in_body {
                self.error(
                    exprs.last().map(|e| e.span()).unwrap_or(group.span()),
                    "expand ignores the attribute body",
                );
            }
            self.pop_scope();
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
            self.pop_scope();
            return Err(());
        };

        match expand_kind {
            ExpandKind::ExpandFull => {
                let contents = self.parse_raw_block_to_exprs(
                    group.span(),
                    &parent_rest[0..match_arm_ends.body_end],
                );
                self.close_expr_after_trailing_body(
                    &mut exprs,
                    trailing_block,
                    contents?,
                );
                self.pop_scope();
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
                self.pop_scope();

                let MetaExpr::For {
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
            match self.handle_template_in_raw_block(
                parent_span,
                tokens,
                &mut raw_token_list_start,
                &mut i,
                group,
                inner_tokens,
                &mut exprs,
            )? {
                TemplateInRawParseResult::Ignore => continue,
                TemplateInRawParseResult::ExprsAdded => continue,
                TemplateInRawParseResult::UnmatchedEnd {
                    span,
                    kind,
                    offset,
                    contents,
                } => {
                    return Ok(RawBodyParseResult::UnmatchedEnd {
                        span,
                        kind,
                        offset,
                        contents,
                    });
                }
            }
        }
        if raw_token_list_start == 0 {
            debug_assert!(exprs.is_empty());
            return Ok(RawBodyParseResult::ParseRaw);
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
        raw_token_list_start: &mut usize,
        block_continuation: &mut usize,
        group: &Group,
        template_inner_tokens: &[TokenTree],
        exprs: &mut Vec<Rc<MetaExpr>>,
    ) -> Result<TemplateInRawParseResult> {
        let offset_in_block = *block_continuation - 1;
        let parent_scope = self.scopes.last().as_ref().unwrap().kind;
        let is_raw_block = parent_scope == ScopeKind::Raw;
        let first_tok = template_inner_tokens.first();
        if let Some(TokenTree::Ident(ident)) = first_tok {
            if !is_raw_block && ident.to_string() == "else" {
                append_token_list(
                    exprs,
                    block_tokens,
                    *raw_token_list_start,
                    offset_in_block,
                );
                return Ok(TemplateInRawParseResult::UnmatchedEnd {
                    span: group.span(),
                    kind: TrailingBlockKind::Else,
                    offset: offset_in_block,
                    contents: std::mem::take(exprs),
                });
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
                    return Ok(TemplateInRawParseResult::UnmatchedEnd {
                        span: group.span(),
                        kind: end_tag,
                        offset: offset_in_block,
                        contents: std::mem::take(exprs),
                    });
                }
            }
        }
        if is_raw_block {
            // raw blocks only look for closing tags
            return Ok(TemplateInRawParseResult::Ignore);
        }
        append_token_list(
            exprs,
            block_tokens,
            *raw_token_list_start,
            offset_in_block,
        );
        *raw_token_list_start = *block_continuation;

        let (template_exprs, rest, trailing_block) =
            self.parse_body(group.span(), template_inner_tokens, true);
        debug_assert!(rest.is_empty());
        exprs.extend(template_exprs);
        let Some(trailing_block) = trailing_block else {
            return Ok(TemplateInRawParseResult::ExprsAdded);
        };

        let continuation = &block_tokens[*block_continuation..];

        if trailing_block == TrailingBlockKind::Eval {
            let (contents, rest, trailing_block_kind) =
                self.parse_body(block_parent_span, continuation, false);
            self.pop_scope();

            let tokens_consumed = continuation.len() - rest.len();

            let Some(trailing_block_kind) = trailing_block_kind else {
                self.error(group.span(), "`eval` tag is never terminated");
                return Err(());
            };
            if trailing_block_kind != TrailingBlockKind::Eval {
                let continuation_tag_span =
                    continuation[tokens_consumed - 1].span();
                self.error_unexpected_end_tag(
                    continuation_tag_span,
                    trailing_block_kind,
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
            return Ok(TemplateInRawParseResult::ExprsAdded);
        }
        let res = self.parse_raw_block(block_parent_span, continuation);
        self.pop_scope();
        match res? {
            RawBodyParseResult::ParseRaw | RawBodyParseResult::Complete(_) => {
                self.error_no_closing_tag(group.span(), trailing_block);
                Err(())
            }
            RawBodyParseResult::UnmatchedEnd {
                span,
                kind,
                offset,
                contents,
            } => {
                let is_else_block = trailing_block == TrailingBlockKind::If
                    && kind == TrailingBlockKind::Else;
                if !is_else_block {
                    if kind != trailing_block {
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
                    return Ok(TemplateInRawParseResult::ExprsAdded);
                }
                let else_offset = *block_continuation + offset;
                *block_continuation = else_offset + 1;
                *raw_token_list_start = *block_continuation;
                let TokenTree::Group(else_tag) = &block_tokens[else_offset]
                else {
                    unreachable!()
                };
                let else_tag_tokens = else_tag.stream().into_vec();
                debug_assert!(
                    has_template_angle_backets(&else_tag_tokens)
                        && else_tag_tokens[1].to_string() == "else"
                );

                let if_expr_index = exprs.len() - 1;
                let else_expr_toks =
                    &else_tag_tokens[2..else_tag_tokens.len() - 1];

                let else_expr_new = if else_expr_toks.is_empty() {
                    debug_assert!(matches!(
                        parent_scope,
                        ScopeKind::Template | ScopeKind::Quote
                    ));
                    self.push_dummy_scope(parent_scope);
                    let res = self.parse_raw_block(
                        else_tag.span(),
                        &block_tokens[*block_continuation..],
                    );
                    self.pop_scope();
                    let RawBodyParseResult::UnmatchedEnd {
                        span,
                        kind,
                        offset,
                        contents,
                    } = res?
                    else {
                        self.error_no_closing_tag(
                            group.span(),
                            trailing_block,
                        );
                        return Err(());
                    };

                    if kind != TrailingBlockKind::If {
                        self.error_unexpected_end_tag(span, kind);
                        return Err(());
                    }
                    *block_continuation += offset + 1;
                    *raw_token_list_start = *block_continuation;
                    Rc::new(MetaExpr::Scope {
                        span,
                        body: contents,
                    })
                } else {
                    let if_expr = exprs.pop().unwrap();
                    let res = self.handle_template_in_raw_block(
                        block_parent_span,
                        block_tokens,
                        raw_token_list_start,
                        block_continuation,
                        else_tag,
                        else_expr_toks,
                        exprs,
                    );
                    match res? {
                        TemplateInRawParseResult::Ignore => unreachable!(),
                        TemplateInRawParseResult::UnmatchedEnd {
                            span,
                            kind,
                            ..
                        } => {
                            self.errror_unmatched_closing_tag(span, kind);
                            return Err(());
                        }
                        TemplateInRawParseResult::ExprsAdded => (),
                    };

                    std::mem::replace(&mut exprs[if_expr_index], if_expr)
                };

                let if_expr = Rc::get_mut(&mut exprs[if_expr_index]).unwrap();
                let MetaExpr::IfExpr {
                    body, else_expr, ..
                } = if_expr
                else {
                    unreachable!()
                };
                *body = contents;
                *else_expr = Some(else_expr_new);

                Ok(TemplateInRawParseResult::ExprsAdded)
            }
        }
    }

    fn errror_unmatched_closing_tag(
        &mut self,
        span: Span,
        trailing_block: TrailingBlockKind,
    ) {
        self.error(
            span,
            format!("unmatched closing tag `/{}`", trailing_block.to_str()),
        );
    }

    fn error_no_closing_tag(
        &mut self,
        span: Span,
        trailing_block: TrailingBlockKind,
    ) {
        self.error(
            span,
            format!("missing closing tag for `{}`", trailing_block.to_str()),
        );
    }

    fn error_unexpected_end_tag(
        &mut self,
        span: Span,
        kind: TrailingBlockKind,
    ) {
        self.error(span, format!("unexpected end tag `{}`", kind.to_str()));
    }
}
