use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream,
    TokenTree,
};
use std::{collections::HashMap, rc::Rc};

trait IntoIterIntoVec {
    type Item;
    fn into_vec(self) -> Vec<Self::Item>;
}
impl<I: IntoIterator> IntoIterIntoVec for I {
    type Item = I::Item;

    fn into_vec(self) -> Vec<Self::Item> {
        self.into_iter().collect()
    }
}

type Result<T, E = ()> = std::result::Result<T, E>;

struct MetaError {
    span: Span,
    message: String,
}

struct BindingParameter {
    span: Span,
    name: Rc<str>,
}

struct BuiltinFn {
    param_count: usize,
    #[allow(clippy::type_complexity)]
    builtin: Box<
        dyn Fn(&mut Context, Span, &[Rc<MetaValue>]) -> Result<Rc<MetaValue>>,
    >,
}

#[derive(Clone)]
enum MetaValue {
    Token(TokenTree),
    Tokens(Vec<TokenTree>),
    Int {
        value: i64,
        // For literals that come straight from source code this
        // is set and will be used for the span in case occurs in output.
        // For 'generated' literals like from a computed expression or
        // an `enumerate` this is not set, and we use the span
        // of the final expression that caused the output instead.
        // Because this language is fuzzy about the types of things it can
        // happen that the user really intended a *token* but ended up
        // with an integer. In order to mimic the token semantics more closely
        // and prevent users from having to use `raw!` everwhere this
        // seems like a worthwile tradeoff.
        span: Option<Span>,
    },
    Bool {
        value: bool,
        span: Option<Span>, // same reasoning as `Int`
    },
    String {
        value: Rc<str>,
        span: Option<Span>, // same reasoning as `Int`
    },
    Fn(Rc<Function>),
    BuiltinFn(Rc<BuiltinFn>),
    List(Vec<Rc<MetaValue>>),
    Tuple(Vec<Rc<MetaValue>>),
}

enum Pattern {
    Ident(BindingParameter),
    Tuple { span: Span, elems: Vec<Pattern> },
    List { span: Span, elems: Vec<Pattern> },
}

impl Default for MetaValue {
    fn default() -> Self {
        Self::Tokens(Vec::new())
    }
}

struct Function {
    span: Span,
    name: Rc<str>,
    params: Vec<BindingParameter>,
    body: Vec<Rc<MetaExpr>>,
}

enum MetaExpr {
    Literal {
        span: Span,
        value: Rc<MetaValue>,
    },
    Ident {
        span: Span,
        name: Rc<str>,
    },
    LetBinding {
        span: Span,
        name: Rc<str>,
        expr: Option<Rc<MetaExpr>>,
    },
    FnCall {
        span: Span,
        name: Rc<str>,
        args: Vec<Rc<MetaExpr>>,
    },
    FnDecl(Rc<Function>),
    RawOutputGroup {
        span: Span,
        delimiter: Delimiter,
        contents: Vec<Rc<MetaExpr>>,
    },
    IfExpr {
        span: Span,
        condition: Rc<MetaExpr>,
        body: Vec<Rc<MetaExpr>>,
        else_expr: Option<Rc<MetaExpr>>,
    },
    ForExpansion {
        span: Span,
        pattern: Pattern,
        variants_expr: Rc<MetaExpr>,
        body: Vec<Rc<MetaExpr>>,
    },
    Scope {
        span: Span,
        body: Vec<Rc<MetaExpr>>,
    },
    List {
        span: Span,
        exprs: Vec<Rc<MetaExpr>>,
    },
    Tuple {
        span: Span,
        exprs: Vec<Rc<MetaExpr>>,
    },
}

struct Binding {
    #[allow(unused)] // Todo: im sure we will need this at some point?
    span: Span,
    value: Rc<MetaValue>,
}

struct Context {
    empty_token_list: Rc<MetaValue>,
    scopes: Vec<HashMap<Rc<str>, Binding>>,
    errors: Vec<MetaError>,
}

#[derive(PartialEq, Eq)]
enum TrailingBlockKind {
    For,
    If,
    Else,
    Let,
    Unquote,
    Fn,
}

enum RawBodyParseResult {
    Plain,
    Complete(Vec<Rc<MetaExpr>>),
    UnmatchedEnd {
        span: Span,
        kind: TrailingBlockKind,
        offset: usize,
        contents: Vec<Rc<MetaExpr>>,
    },
}

impl MetaValue {
    fn type_id(&self) -> &'static str {
        match self {
            MetaValue::Token(_) => "token",
            MetaValue::Tokens(_) => "tokens",
            MetaValue::Int { .. } => "int",
            MetaValue::String { .. } => "string",
            MetaValue::Bool { .. } => "bool",
            MetaValue::Fn(_) | MetaValue::BuiltinFn(_) => "fn",
            MetaValue::List(_) => "list",
            MetaValue::Tuple(_) => "tuple",
        }
    }
}

impl MetaExpr {
    fn span(&self) -> Span {
        *match self {
            MetaExpr::Literal { span, .. } => span,
            MetaExpr::Ident { span, .. } => span,
            MetaExpr::LetBinding { span, .. } => span,
            MetaExpr::FnCall { span, .. } => span,
            MetaExpr::FnDecl(function) => &function.span,
            MetaExpr::RawOutputGroup { span, .. } => span,
            MetaExpr::IfExpr { span, .. } => span,
            MetaExpr::ForExpansion { span, .. } => span,
            MetaExpr::Scope { span, .. } => span,
            MetaExpr::List { span, .. } => span,
            MetaExpr::Tuple { span, .. } => span,
        }
    }
}

impl TrailingBlockKind {
    fn to_str(&self) -> &'static str {
        match self {
            TrailingBlockKind::For => "for",
            TrailingBlockKind::If => "if",
            TrailingBlockKind::Else => "else",
            TrailingBlockKind::Let => "let",
            TrailingBlockKind::Fn => "fn",
            TrailingBlockKind::Unquote => "unquote",
        }
    }
}

pub fn eval(body: TokenStream) -> TokenStream {
    let body = body.into_vec();
    let mut ctx = Context::default();

    let expr = ctx.parse_body_deny_trailing(Span::call_site(), &body);

    ctx.eval_to_token_stream(Span::call_site(), &expr)
}

pub fn template(body: TokenStream) -> TokenStream {
    let body = body.into_vec();
    let mut ctx = Context::default();

    let Ok(exprs) = ctx.parse_raw_body_to_exprs(Span::call_site(), &body)
    else {
        return ctx.expand_errors();
    };

    ctx.eval_to_token_stream(Span::call_site(), &exprs)
}

fn comma_token(span: Span) -> TokenTree {
    let mut punct = Punct::new(',', Spacing::Alone);
    punct.set_span(span);
    TokenTree::Punct(punct)
}

fn append_compile_error(tgt: &mut Vec<TokenTree>, message: &str, span: Span) {
    tgt.extend_from_slice(&[
        TokenTree::Ident(Ident::new("compile_error", span)),
        TokenTree::Punct({
            let mut punct = Punct::new('!', Spacing::Alone);
            punct.set_span(span);
            punct
        }),
        TokenTree::Group({
            let mut group = Group::new(Delimiter::Brace, {
                TokenStream::from_iter(vec![TokenTree::Literal({
                    let mut string = Literal::string(message);
                    string.set_span(span);
                    string
                })])
            });
            group.set_span(span);
            group
        }),
    ]);
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

impl Default for Context {
    fn default() -> Self {
        let mut ctx = Self {
            empty_token_list: Default::default(),
            scopes: vec![Default::default()],
            errors: Default::default(),
        };
        ctx.insert_builtins();
        ctx
    }
}

// evaluation
impl Context {
    fn insert_builtins(&mut self) {
        self.insert_builtin_str_fn("lowercase", |s| s.to_lowercase());
        self.insert_builtin_str_fn("uppercase", |s| s.to_uppercase());
        self.insert_builtin_str_fn("capitalize", |s| {
            if let Some(first) = s.chars().next() {
                format!("{}{}", first.to_uppercase(), &s[first.len_utf8()..])
            } else {
                String::new()
            }
        });
        self.insert_builtin_list_fn("enumerate", |l| {
            l.iter()
                .enumerate()
                .map(|(i, v)| {
                    Rc::new(MetaValue::Tuple(vec![
                        Rc::new(MetaValue::Int {
                            value: i as i64,
                            span: None,
                        }),
                        v.clone(),
                    ]))
                })
                .collect::<Vec<_>>()
        });
        self.insert_builtin_fn("len", 1, |ctx, callsite, args| {
            let v = match &*args[0] {
                MetaValue::Token(_) => 1,
                MetaValue::Tokens(token_trees) => token_trees.len(),
                MetaValue::List(list) => list.len(),
                MetaValue::Tuple(tup) => tup.len(),
                MetaValue::String { value: s, span: _ } => s.len(),
                MetaValue::Bool { .. }
                | MetaValue::Int { .. }
                | MetaValue::Fn(_)
                | MetaValue::BuiltinFn(_) => {
                    ctx.error(
                        callsite,
                        format!(
                            "value of type {} has no len()",
                            args[0].type_id()
                        ),
                    );
                    return Err(());
                }
            };
            Ok(Rc::new(MetaValue::Int {
                value: v as i64,
                span: None,
            }))
        });
        // TODO: camel_case, pascal_case, ...
    }
    fn attempt_pattern_match(
        &mut self,
        pat: &Pattern,
        val: Rc<MetaValue>,
    ) -> Result<()> {
        match pat {
            Pattern::Ident(bind) => {
                self.insert_binding(bind.span, bind.name.clone(), val);
                Ok(())
            }
            Pattern::Tuple {
                span,
                elems: pat_bindings,
            } => {
                let MetaValue::Tuple(val_elems) = &*val else {
                    // TODO: more context
                    self.error(
                        *span,
                        format!(
                            "tuple pattern does not match {}",
                            val.type_id()
                        ),
                    );
                    return Err(());
                };
                if pat_bindings.len() != val_elems.len() {
                    self.error( *span,
                                 format!(
                                    "tuple pattern missmatch: expected length {}, got {}",
                                    pat_bindings.len(),
                                    val_elems.len()
                                ),
                            );
                    return Err(());
                }
                for i in 0..val_elems.len() {
                    self.attempt_pattern_match(
                        &pat_bindings[i],
                        val_elems[i].clone(),
                    )?;
                }
                Ok(())
            }
            Pattern::List {
                span,
                elems: pat_bindings,
            } => {
                let MetaValue::List(val_elems) = &*val else {
                    // TODO: more context
                    self.error(
                        *span,
                        format!(
                            "list pattern does not match {}",
                            val.type_id()
                        ),
                    );
                    return Err(());
                };
                if pat_bindings.len() != val_elems.len() {
                    self.error(  *span,
                                 format!(
                                    "list pattern missmatch: expected length {}, got {}",
                                    pat_bindings.len(),
                                    val_elems.len()
                                ),
                            );
                    return Err(());
                }
                for i in 0..val_elems.len() {
                    self.attempt_pattern_match(
                        &pat_bindings[i],
                        val_elems[i].clone(),
                    )?;
                }
                Ok(())
            }
        }
    }

    fn insert_builtin_fn(
        &mut self,
        name: &'static str,
        param_count: usize,
        f: impl 'static
            + Fn(&mut Context, Span, &[Rc<MetaValue>]) -> Result<Rc<MetaValue>>,
    ) {
        let builtin_fn_v = Rc::new(MetaValue::BuiltinFn(Rc::new(BuiltinFn {
            param_count,
            builtin: Box::new(f),
        })));
        self.insert_binding(Span::call_site(), Rc::from(name), builtin_fn_v);
    }

    fn insert_builtin_str_fn(
        &mut self,
        name: &'static str,
        f: impl 'static + Fn(&str) -> String,
    ) {
        let str_fn = move |ctx: &mut Context,
                           span: Span,
                           args: &[Rc<MetaValue>]|
              -> Result<Rc<MetaValue>> {
            let MetaValue::String { value: s, span: _ } = &*args[0] else {
                ctx.error(
                    span,
                    format!(
                        "builtin function `{name}` expects a string, got a {}",
                        args[0].type_id()
                    ),
                );
                return Err(());
            };
            Ok(Rc::new(MetaValue::String {
                value: Rc::from(f(s)),
                span: None,
            }))
        };
        self.insert_builtin_fn(name, 1, str_fn);
    }
    fn insert_builtin_list_fn(
        &mut self,
        name: &'static str,
        f: impl 'static + Fn(&[Rc<MetaValue>]) -> Vec<Rc<MetaValue>>,
    ) {
        let list_fn = move |ctx: &mut Context,
                            span: Span,
                            args: &[Rc<MetaValue>]|
              -> Result<Rc<MetaValue>> {
            let MetaValue::List(list) = &*args[0] else {
                ctx.error(
                    span,
                    format!(
                        "builtin function `{name}` expects a list, got a {}",
                        args[0].type_id()
                    ),
                );
                return Err(());
            };
            Ok(Rc::new(MetaValue::List(f(list))))
        };
        self.insert_builtin_fn(name, 1, list_fn);
    }
    fn append_value_to_stream(
        &mut self,
        tgt: &mut Vec<TokenTree>,
        eval_span: Span,
        value: &MetaValue,
    ) -> Result<()> {
        match value {
            MetaValue::Token(t) => {
                tgt.push(t.clone());
            }
            MetaValue::Tokens(list) => {
                tgt.extend(list.iter().cloned());
            }
            MetaValue::Int { value, span } => {
                let mut lit = Literal::i64_unsuffixed(*value);
                lit.set_span(span.unwrap_or(eval_span));
                tgt.push(TokenTree::Literal(lit));
            }
            MetaValue::Bool { value, span } => {
                let ident = Ident::new(
                    if *value { "true" } else { "false" },
                    span.unwrap_or(eval_span),
                );
                tgt.push(TokenTree::Ident(ident));
            }
            MetaValue::String { value, span } => {
                let mut lit = Literal::string(value);
                lit.set_span(span.unwrap_or(eval_span));
                tgt.push(TokenTree::Literal(lit));
            }
            MetaValue::Fn(_) | MetaValue::BuiltinFn(_) => {
                self.error(eval_span, "function cannot be tokenized");
                return Err(());
            }
            MetaValue::List(vals) => {
                let mut list = Vec::new();
                for (i, e) in vals.iter().enumerate() {
                    if i > 0 {
                        list.push(comma_token(eval_span));
                    }
                    self.append_value_to_stream(&mut list, eval_span, e)?;
                }
                tgt.push(TokenTree::Group(Group::new(
                    Delimiter::Bracket,
                    TokenStream::from_iter(list),
                )));
            }
            MetaValue::Tuple(vals) => {
                let mut list = Vec::new();
                for (i, e) in vals.iter().enumerate() {
                    if i > 0 {
                        list.push(comma_token(eval_span));
                    }
                    self.append_value_to_stream(&mut list, eval_span, e)?;
                }
                if vals.len() == 1 {
                    list.push(comma_token(eval_span));
                }
                tgt.push(TokenTree::Group(Group::new(
                    Delimiter::Parenthesis,
                    TokenStream::from_iter(list),
                )));
            }
        }
        Ok(())
    }
    fn lookup(&self, name: &str) -> Option<Rc<MetaValue>> {
        for scope in &self.scopes {
            if let Some(binding) = scope.get(name) {
                return Some(binding.value.clone());
            }
        }
        None
    }
    fn eval_to_token_stream(
        &mut self,
        eval_span: Span,
        exprs: &[Rc<MetaExpr>],
    ) -> TokenStream {
        if self.errors.is_empty() {
            let mut res = Vec::new();
            if self
                .eval_stmt_list_to_stream(&mut res, eval_span, exprs)
                .is_ok()
                && self.errors.is_empty()
            {
                return TokenStream::from_iter(res);
            }
        }
        self.expand_errors()
    }
    fn eval_stmt_list_to_stream(
        &mut self,
        tgt: &mut Vec<TokenTree>,
        eval_span: Span,
        exprs: &[Rc<MetaExpr>],
    ) -> Result<()> {
        for expr in exprs {
            let Ok(v) = self.eval(expr) else {
                continue;
            };
            self.append_value_to_stream(tgt, eval_span, &v)?;
        }
        Ok(())
    }
    fn eval_stmt_list_to_meta_val(
        &mut self,
        eval_span: Span,
        exprs: &[Rc<MetaExpr>],
    ) -> Result<Rc<MetaValue>> {
        let mut res = Vec::new();
        self.eval_stmt_list_to_stream(&mut res, eval_span, exprs)?;
        Ok(Rc::new(MetaValue::Tokens(res)))
    }
    fn eval(&mut self, expr: &MetaExpr) -> Result<Rc<MetaValue>> {
        match expr {
            MetaExpr::Literal { span: _, value } => Ok(value.clone()),
            MetaExpr::Ident { span, name } => {
                if let Some(expr) = self.lookup(name) {
                    return Ok(expr);
                }
                Ok(Rc::new(MetaValue::Token(TokenTree::Ident(Ident::new(
                    name, *span,
                )))))
            }
            MetaExpr::LetBinding { span, expr, name } => {
                let val = self.eval(expr.as_ref().unwrap())?;
                self.insert_binding(*span, name.clone(), val);
                Ok(self.empty_token_list.clone())
            }
            MetaExpr::FnCall { span, name, args } => {
                self.eval_fn_call(span, name, args)
            }
            MetaExpr::RawOutputGroup {
                span,
                delimiter,
                contents,
            } => {
                let mut res = Vec::new();
                self.eval_stmt_list_to_stream(&mut res, *span, contents)?;
                Ok(Rc::new(MetaValue::Token(TokenTree::Group(Group::new(
                    *delimiter,
                    TokenStream::from_iter(res),
                )))))
            }
            MetaExpr::ForExpansion {
                span,
                pattern,
                variants_expr,
                body,
            } => {
                let input_list = self.eval(variants_expr)?;
                let MetaValue::List(list_elems) = &*input_list else {
                    self.error(
                        *span,
                        format!(
                            "cannot iterate over {}",
                            input_list.type_id()
                        ),
                    );
                    return Err(());
                };
                let mut res = Vec::new();
                self.scopes.push(Default::default());
                for elem in list_elems {
                    if self
                        .attempt_pattern_match(pattern, elem.clone())
                        .is_err()
                    {
                        self.scopes.pop();
                        return Err(());
                    }
                    if self
                        .eval_stmt_list_to_stream(&mut res, *span, body)
                        .is_err()
                    {
                        self.scopes.pop();
                        return Err(());
                    }
                }
                Ok(Rc::new(MetaValue::Tokens(res)))
            }
            MetaExpr::Scope {
                span,
                body: contents,
            } => {
                self.scopes.push(Default::default());
                let res = self.eval_stmt_list_to_meta_val(*span, contents);
                self.scopes.pop();
                res
            }
            MetaExpr::FnDecl(f) => {
                self.insert_binding(
                    f.span,
                    f.name.clone(),
                    Rc::new(MetaValue::Fn(f.clone())),
                );
                Ok(self.empty_token_list.clone())
            }
            MetaExpr::List { span: _, exprs } => {
                let mut elements = Vec::new();
                for e in exprs {
                    elements.push(self.eval(e)?);
                }
                Ok(Rc::new(MetaValue::List(elements)))
            }
            MetaExpr::Tuple { span: _, exprs } => {
                let mut elements = Vec::new();
                for e in exprs {
                    elements.push(self.eval(e)?);
                }
                Ok(Rc::new(MetaValue::Tuple(elements)))
            }
            MetaExpr::IfExpr {
                span,
                condition,
                body,
                else_expr,
            } => {
                let condition_val = self.eval(condition)?;
                let MetaValue::Bool {
                    value: condition,
                    span: _,
                } = &*condition_val
                else {
                    self.error(
                        condition.span(),
                        format!(
                            "if expression must result in `bool`, not `{}`",
                            condition_val.type_id()
                        ),
                    );
                    return Err(());
                };
                if *condition {
                    self.eval_stmt_list_to_meta_val(*span, body)
                } else if let Some(else_expr) = else_expr {
                    self.eval(else_expr)
                } else {
                    Ok(self.empty_token_list.clone())
                }
            }
        }
    }

    fn eval_fn_call(
        &mut self,
        span: &Span,
        name: &Rc<str>,
        args: &Vec<Rc<MetaExpr>>,
    ) -> std::result::Result<Rc<MetaValue>, ()> {
        let Some(binding) = self.lookup(name) else {
            self.error(*span, format!("undefined function `{name}`"));
            return Err(());
        };
        match &*binding {
            MetaValue::Fn(function) => {
                if function.params.len() != args.len() {
                    self.error(
                        *span,
                        format!(
                            "function `{name}` with {} parameters called with {} arguments",
                            function.params.len(),
                            args.len()
                        ),
                    );
                    return Err(());
                }
                self.scopes.push(Default::default());
                for (i, arg) in args.iter().enumerate() {
                    let Ok(val) = self.eval(arg) else {
                        self.scopes.pop();
                        return Err(());
                    };
                    let param = &function.params[i];
                    self.insert_binding(param.span, param.name.clone(), val);
                }
                let res =
                    self.eval_stmt_list_to_meta_val(*span, &function.body);
                self.scopes.pop();
                res
            }
            MetaValue::BuiltinFn(builtin_fn) => {
                if builtin_fn.param_count != args.len() {
                    self.error(
                        *span,
                        format!(
                            "function `{name}` with {} parameters called with {} arguments",
                            builtin_fn.param_count,
                            args.len()
                        ),
                    );
                    return Err(());
                }
                let mut param_bindings = Vec::new();
                for arg in args {
                    let Ok(val) = self.eval(arg) else {
                        return Err(());
                    };
                    param_bindings.push(val);
                }
                (builtin_fn.builtin)(self, *span, &param_bindings)
            }
            other => {
                self.error(
                    *span,
                    format!(
                        "value of type {} is not callable",
                        other.type_id()
                    ),
                );
                Err(())
            }
        }
    }

    fn insert_binding(
        &mut self,
        span: Span,
        name: Rc<str>,
        value: Rc<MetaValue>,
    ) {
        let binding = Binding { span, value };
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.clone(), binding);
    }
    fn expand_errors(&self) -> TokenStream {
        let mut errors = Vec::new();
        for err in &self.errors {
            append_compile_error(&mut errors, &err.message, err.span);
        }
        TokenStream::from_iter(errors)
    }
}

// parsing utils
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

fn parse_literal(token: &TokenTree) -> Option<(Span, Rc<MetaValue>)> {
    if let TokenTree::Literal(lit) = token {
        let span = lit.span();
        let s = lit.to_string();
        if s.starts_with('"') {
            Some((
                span,
                Rc::new(MetaValue::String {
                    value: Rc::from(s[1..s.len() - 1].to_string()),
                    span: Some(token.span()),
                }),
            ))
        } else if let Ok(n) = s.parse::<i64>() {
            Some((
                span,
                Rc::new(MetaValue::Int {
                    value: n,
                    span: Some(token.span()),
                }),
            ))
        } else {
            None
        }
    } else {
        None
    }
}

impl Context {
    fn parse_expr<'a>(
        &mut self,
        parent_span: Span,
        tokens: &'a [TokenTree],
    ) -> Result<(Rc<MetaExpr>, &'a [TokenTree])> {
        let (expr, rest, _trailing_block) =
            self.parse_stmt_or_expr(parent_span, tokens, false)?;
        Ok((expr, rest))
    }
    fn parse_stmt_or_expr<'a>(
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
                        let list = self.parse_comma_separated(
                            group.delimiter(),
                            group.span(),
                            &group_tokens,
                        )?;
                        Ok((
                            Rc::new(MetaExpr::Tuple {
                                span: group.span(),
                                exprs: list,
                            }),
                            &tokens[1..],
                            None,
                        ))
                    }
                    Delimiter::Bracket => {
                        let list = self.parse_comma_separated(
                            group.delimiter(),
                            group.span(),
                            &group_tokens,
                        )?;
                        Ok((
                            Rc::new(MetaExpr::List {
                                span: group.span(),
                                exprs: list,
                            }),
                            &tokens[1..],
                            None,
                        ))
                    }
                    Delimiter::None => {
                        let (expr, rest) =
                            self.parse_expr(group.span(), &group_tokens)?;
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
                    "fn" => {
                        return self.parse_fn(
                            span,
                            &tokens[1..],
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
                    "if" => {
                        return self.parse_if(
                            span,
                            &tokens[1..],
                            allow_trailing_block,
                        );
                    }
                    "raw" => return self.parse_raw_expr(span, &tokens[1..]),
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

            TokenTree::Literal(lit) => {
                let span = lit.span();
                if let Some((_, value)) = parse_literal(&tokens[0]) {
                    Ok((
                        Rc::new(MetaExpr::Literal { span, value }),
                        &tokens[1..],
                        None,
                    ))
                } else {
                    self.error(span, "invalid literal".to_owned());
                    Err(())
                }
            }

            token => {
                self.error(token.span(), "unexpected token");
                Err(())
            }
        }
    }

    fn error(&mut self, span: Span, message: impl Into<String>) {
        self.errors.push(MetaError {
            span,
            message: message.into(),
        });
    }

    fn insert_dummy_binding(&mut self, name: Rc<str>) {
        // dummy binding during parsing so raw blocks can detect identifiers
        self.scopes.last_mut().unwrap().insert(
            name.clone(),
            Binding {
                span: Span::call_site(),
                value: self.empty_token_list.clone(),
            },
        );
    }

    fn insert_dummy_bindings_for_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Ident(i) => self.insert_dummy_binding(i.name.clone()),
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
            self.error(let_span, "expected identifier after `let`");
            return Err(());
        }

        let (name_span, name) = parse_ident(&tokens[0]).ok_or_else(|| {
            self.error(tokens[0].span(), "expected identifier");
        })?;

        self.insert_dummy_binding(name.clone());

        if tokens.len() == 1 && allow_trailing_block {
            return Ok((
                Rc::new(MetaExpr::LetBinding {
                    span: let_span,
                    name,
                    expr: None,
                }),
                &[],
                Some(TrailingBlockKind::Let),
            ));
        }

        let mut eq_span = None;
        if let Some(TokenTree::Punct(p)) = tokens.get(1) {
            if p.as_char() == '=' {
                eq_span = Some(p.span());
            }
        }

        let Some(eq_span) = eq_span else {
            self.error(
                tokens.get(1).map(|t| t.span()).unwrap_or(name_span),
                "expected = after let identifier",
            );
            return Err(());
        };

        let rest = &tokens[2..];

        if tokens.is_empty() {
            self.error(eq_span, "expected expression after `=`");
            return Err(());
        }
        let (expr, rest) = self.parse_expr(let_span, rest)?;

        Ok((
            Rc::new(MetaExpr::LetBinding {
                span: let_span,
                name,
                expr: Some(expr),
            }),
            rest,
            None,
        ))
    }

    fn parse_raw_expr<'a>(
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
                "expected `!` after `raw`",
            );
            return Err(());
        }

        let tokens = &tokens[1..];

        // for dummy bindings
        self.scopes.push(Default::default());

        let Some(TokenTree::Group(p)) = tokens.first() else {
            self.error(
                tokens.first().map(|t| t.span()).unwrap_or(raw_span),
                "expected block after `raw!`",
            );
            return Err(());
        };

        let raw_block_contents = p.stream().into_vec();

        let contents = self
            .parse_raw_body_to_exprs(raw_span, &raw_block_contents)
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
                "`eval` is only allowed as a template block",
            );
            return Err(());
        }

        // for dummy bindings
        self.scopes.push(Default::default());

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
        let mut rest = &param_tokens[..];

        // for dummy bindings
        self.scopes.push(Default::default());

        while !rest.is_empty() {
            let (param_span, param_name) =
                parse_ident(&rest[0]).ok_or_else(|| {
                    self.error(
                        rest[0].span(),
                        "expected parameter name".to_owned(),
                    );
                })?;
            self.insert_dummy_binding(param_name.clone());

            params.push(BindingParameter {
                span: param_span,
                name: param_name,
            });

            rest = &rest[1..];
            if !rest.is_empty() {
                if let TokenTree::Punct(p) = &rest[0] {
                    if p.as_char() == ',' {
                        rest = &rest[1..];
                        continue;
                    }
                }
                if !rest.is_empty() {
                    self.error(
                        rest[0].span(),
                        "expected comma between parameters".to_owned(),
                    );
                    return Err(());
                }
            }
        }

        let rest = &tokens[3..];

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

        let (variants_expr, rest) = self.parse_expr(for_span, &rest[1..])?;

        // for dummy bindings
        self.scopes.push(Default::default());

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
        let (condition, rest) = self.parse_expr(if_span, tokens)?;

        // for dummy bindings
        self.scopes.push(Default::default());

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
                    "expected comma or {}",
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
            TokenTree::Ident(ident) => Ok((
                Pattern::Ident(BindingParameter {
                    span: ident.span(),
                    name: Rc::from(ident.to_string()),
                }),
                &tokens[1..],
            )),
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

    fn parse_comma_separated(
        &mut self,
        delimiter: Delimiter,
        parent_span: Span,
        tokens: &[TokenTree],
    ) -> Result<Vec<Rc<MetaExpr>>> {
        let (list, _final_comma_span) = self
            .parse_comma_separated_with_final_comma(
                delimiter,
                parent_span,
                tokens,
            )?;
        Ok(list)
    }

    fn parse_comma_separated_with_final_comma(
        &mut self,
        delimiter: Delimiter,
        parent_span: Span,
        tokens: &[TokenTree],
    ) -> Result<(Vec<Rc<MetaExpr>>, Option<Span>)> {
        let mut exprs = Vec::new();
        let mut rest = tokens;
        let mut final_comma = None;

        while !rest.is_empty() {
            let (expr, new_rest) = self.parse_expr(parent_span, rest)?;
            exprs.push(expr);
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
                format!("expected comma or {}", delimiter_chars(delimiter).1),
            );
            return Err(());
        }

        Ok((exprs, final_comma))
    }

    fn parse_body_deny_trailing(
        &mut self,
        parent_span: Span,
        tokens: &[TokenTree],
    ) -> Vec<Rc<MetaExpr>> {
        self.parse_body(parent_span, tokens, false).0
    }

    // syntax errors are contained withing the body
    // we return all the exprs that we were able to parse
    fn parse_body(
        &mut self,
        parent_span: Span,
        tokens: &[TokenTree],
        allow_trailing_block: bool,
    ) -> (Vec<Rc<MetaExpr>>, Option<TrailingBlockKind>) {
        let mut exprs = Vec::new();
        let mut rest = tokens;

        while !rest.is_empty() {
            let Ok((expr, new_rest, trailing_block)) = self
                .parse_stmt_or_expr(parent_span, rest, allow_trailing_block)
            else {
                break;
            };

            rest = new_rest;
            exprs.push(expr);

            if trailing_block.is_some() {
                debug_assert!(allow_trailing_block && rest.is_empty());
                return (exprs, trailing_block);
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
                    self.error(
                        first.span(),
                        "missing semicolon after expression",
                    );
                } else {
                    rest = &rest[1..];
                }
            }
        }
        (exprs, None)
    }

    fn close_expr_after_trailing_body(
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
            TrailingBlockKind::Unquote => {
                let MetaExpr::Scope { body, .. } = expr else {
                    unreachable!()
                };
                *body = contents;
                self.scopes.pop();
            }
        }
    }

    fn parse_raw_body_to_exprs(
        &mut self,
        parent_span: Span,
        tokens: &[TokenTree],
    ) -> Result<Vec<Rc<MetaExpr>>> {
        match self.parse_raw_body_deny_unmatched(parent_span, tokens)? {
            None => Ok(vec![Rc::new(MetaExpr::Literal {
                span: parent_span,
                value: Rc::new(MetaValue::Tokens(tokens.to_vec())),
            })]),
            Some(exprs) => Ok(exprs),
        }
    }

    fn parse_raw_body_deny_unmatched(
        &mut self,
        parent_span: Span,
        tokens: &[TokenTree],
    ) -> Result<Option<Vec<Rc<MetaExpr>>>> {
        match self.parse_raw_body(parent_span, tokens) {
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

    fn parse_raw_body(
        &mut self,
        parent_span: Span,
        tokens: &[TokenTree],
    ) -> Result<RawBodyParseResult> {
        fn append_previous(
            exprs: &mut Vec<Rc<MetaExpr>>,
            tokens: &[TokenTree],
            raw_token_list_start: usize,
            offset: usize,
        ) {
            if raw_token_list_start != offset {
                exprs.push(Rc::new(MetaExpr::Literal {
                    span: tokens[raw_token_list_start].span(),
                    value: Rc::new(MetaValue::Tokens(
                        tokens[raw_token_list_start..offset].to_vec(),
                    )),
                }));
            }
        }

        let mut exprs = Vec::new();

        let mut raw_token_list_start = 0;

        let mut i = 0;

        while i < tokens.len() {
            let offset = i;
            i += 1;
            let t = &tokens[offset];
            if let TokenTree::Ident(ident) = t {
                let name = ident.to_string();
                if self.lookup(&name).is_some() {
                    append_previous(
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
            let TokenTree::Group(group) = t else {
                continue;
            };
            let group_tokens = group.stream().into_vec();

            let is_template = group.delimiter() == Delimiter::Bracket
                && has_template_angle_backets(&group_tokens);

            if !is_template {
                match self
                    .parse_raw_body_deny_unmatched(group.span(), &group_tokens)
                {
                    Err(()) | Ok(None) => (),
                    Ok(Some(body_exprs)) => {
                        append_previous(
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
            let first_tok = group_tokens.get(1);
            if let Some(TokenTree::Ident(ident)) = first_tok {
                if ident.to_string() == "else" {
                    append_previous(
                        &mut exprs,
                        tokens,
                        raw_token_list_start,
                        offset,
                    );
                    return Ok(RawBodyParseResult::UnmatchedEnd {
                        span: group.span(),
                        kind: TrailingBlockKind::Else,
                        offset,
                        contents: exprs,
                    });
                }
            }
            if let Some(TokenTree::Punct(p)) = first_tok {
                if p.as_char() == '/' {
                    // this is a closing tag
                    let Some(TokenTree::Ident(ident)) = group_tokens.get(2)
                    else {
                        self.error(
                            group_tokens
                                .get(2)
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
                        "unquote" => TrailingBlockKind::Unquote,
                        "if" => TrailingBlockKind::If,
                        "else" => TrailingBlockKind::Else,
                        _ => {
                            self.error(
                                group_tokens
                                    .get(2)
                                    .map(|t| t.span())
                                    .unwrap_or(group.span()),
                                format!("unknown closing tag `{tag}`"),
                            );
                            return Err(());
                        }
                    };
                    append_previous(
                        &mut exprs,
                        tokens,
                        raw_token_list_start,
                        offset,
                    );
                    return Ok(RawBodyParseResult::UnmatchedEnd {
                        span: group.span(),
                        kind,
                        offset,
                        contents: exprs,
                    });
                }
            }
            let (expr, trailing_block) = self.parse_body(
                parent_span,
                &group_tokens[1..group_tokens.len() - 1],
                true,
            );
            exprs.extend(expr);
            let Some(trailing_block) = trailing_block else {
                raw_token_list_start = i;
                continue;
            };
            match self.parse_raw_body(parent_span, &tokens[i..tokens.len()])? {
                RawBodyParseResult::Plain
                | RawBodyParseResult::Complete(_) => {
                    self.error(
                        group.span(),
                        format!(
                            "missing closing tag for `{}`",
                            trailing_block.to_str()
                        ),
                    );
                    return Err(());
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
                        let TokenTree::Group(_else_tag) = &tokens[i + offset]
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
                    self.close_expr_after_trailing_body(
                        &mut exprs, kind, contents,
                    );
                    i += offset + 1;
                    raw_token_list_start = i;
                }
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
}
