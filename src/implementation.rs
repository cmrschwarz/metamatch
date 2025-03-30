use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream,
    TokenTree,
};
use std::{collections::HashMap, fmt::format, rc::Rc};

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
    TokenList(Vec<TokenTree>),
    TokenTree(TokenTree),
    Int(i64),
    String(Rc<str>),
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
        Self::TokenList(Vec::new())
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
        expr: Rc<MetaExpr>,
    },
    FnCall {
        span: Span,
        name: Rc<str>,
        args: Vec<Rc<MetaExpr>>,
    },
    FnDecl(Rc<Function>),
    Group {
        span: Span,
        delimiter: Delimiter,
        contents: Vec<Rc<MetaExpr>>,
    },
    ForExpansion {
        span: Span,
        pattern: Pattern,
        variants_expr: Rc<MetaExpr>,
        body: Vec<Rc<MetaExpr>>,
    },
    Scope {
        span: Span,
        contents: Vec<Rc<MetaExpr>>,
    },
    List {
        span: Span,
        exprs: Vec<Rc<MetaExpr>>,
    },
    Tuple {
        span: Span,
        exprs: Vec<Rc<MetaExpr>>,
    },
    StatementList {
        exprs: Vec<Rc<MetaExpr>>,
    },
}

struct Binding {
    span: Span,
    value: Rc<MetaValue>,
}

#[derive(Default)]
struct Context {
    empty_token_list: Rc<MetaValue>,
    scopes: Vec<HashMap<Rc<str>, Binding>>,
    errors: Vec<MetaError>,
}

impl MetaValue {
    fn type_id(&self) -> &'static str {
        match self {
            MetaValue::TokenList(_) => "token_list",
            MetaValue::TokenTree(_) => "token_tree",
            MetaValue::Int(_) => "int",
            MetaValue::String(_) => "string",
            MetaValue::Fn(_) => "fn",
            MetaValue::BuiltinFn(_) => "builtin_fn",
            MetaValue::List(_) => "list",
            MetaValue::Tuple(_) => "tuple",
        }
    }
}

pub fn expand(body: TokenStream) -> TokenStream {
    let body = body.into_iter().collect::<Vec<_>>();
    let mut ctx = Context::default();
    ctx.insert_builtins();
    match ctx.parse_body(Span::call_site(), &body) {
        Ok(expr) => ctx.evaluate_to_token_stream(Span::call_site(), &expr),
        Err(()) => ctx.expand_errors(),
    }
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
                    self.errors.push(MetaError {
                        span: *span,
                        message: format!(
                            "tuple pattern does not match {}",
                            val.type_id()
                        ),
                    });
                    return Err(());
                };
                if pat_bindings.len() != val_elems.len() {
                    self.errors.push(MetaError {
                        span: *span,
                        message: format!(
                            "tuple pattern missmatch: expected length {}, got {}",
                            pat_bindings.len(),
                            val_elems.len()
                        ),
                    });
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
            // Essentially the same as the Tuple case.
            // Would love myself some metamatch here... :D.
            Pattern::List {
                span,
                elems: pat_bindings,
            } => {
                let MetaValue::List(val_elems) = &*val else {
                    // TODO: more context
                    self.errors.push(MetaError {
                        span: *span,
                        message: format!(
                            "list pattern does not match {}",
                            val.type_id()
                        ),
                    });
                    return Err(());
                };
                if pat_bindings.len() != val_elems.len() {
                    self.errors.push(MetaError {
                        span: *span,
                        message: format!(
                            "list pattern missmatch: expected length {}, got {}",
                            pat_bindings.len(),
                            val_elems.len()
                        ),
                    });
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
    fn insert_builtin_str_fn(
        &mut self,
        name: &'static str,
        f: impl 'static + Fn(&str) -> String,
    ) {
        let builtin_fn = move |ctx: &mut Context,
                               span: Span,
                               args: &[Rc<MetaValue>]|
              -> Result<Rc<MetaValue>> {
            let MetaValue::String(s) = &*args[0] else {
                ctx.errors.push(MetaError {
                    span,
                    message: format!(
                        "builtin function `{name}` expects a string, got a {}",
                        args[0].type_id()
                    ),
                });
                return Err(());
            };
            Ok(Rc::new(MetaValue::String(Rc::from(f(s)))))
        };
        let builtin_fn_v = Rc::new(MetaValue::BuiltinFn(Rc::new(BuiltinFn {
            param_count: 1,
            builtin: Box::new(builtin_fn),
        })));
        self.insert_binding(Span::call_site(), Rc::from(name), builtin_fn_v);
    }
    fn append_value_to_stream(
        &mut self,
        tgt: &mut Vec<TokenTree>,
        eval_span: Span,
        value: &MetaValue,
    ) -> Result<()> {
        match value {
            MetaValue::TokenTree(t) => {
                tgt.push(t.clone());
            }
            MetaValue::TokenList(list) => {
                tgt.extend(list.iter().cloned());
            }
            MetaValue::Int(value) => {
                let mut lit = Literal::i64_unsuffixed(*value);
                lit.set_span(eval_span);
                tgt.push(TokenTree::Literal(lit));
            }
            MetaValue::String(value) => {
                let mut lit = Literal::string(value);
                lit.set_span(eval_span);
                tgt.push(TokenTree::Literal(lit));
            }
            MetaValue::Fn(_) | MetaValue::BuiltinFn(_) => {
                self.errors.push(MetaError {
                    span: eval_span,
                    message: "function cannot be tokenized".to_owned(),
                });
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
    fn lookup_throw(
        &mut self,
        loc: Span,
        name: &str,
    ) -> Result<Rc<MetaValue>> {
        if let Some(val) = self.lookup(name) {
            return Ok(val);
        }
        self.errors.push(MetaError {
            span: loc,
            message: format!("undefined identifier `{name}`"),
        });
        Err(())
    }
    fn eval_stmt_list_to_stream(
        &mut self,
        tgt: &mut Vec<TokenTree>,
        eval_span: Span,
        exprs: &[Rc<MetaExpr>],
    ) -> Result<()> {
        for expr in exprs {
            let Ok(v) = self.eval(eval_span, expr) else {
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
        Ok(Rc::new(MetaValue::TokenList(res)))
    }
    fn eval(
        &mut self,
        eval_span: Span,
        expr: &MetaExpr,
    ) -> Result<Rc<MetaValue>> {
        match expr {
            MetaExpr::Literal { span: _, value } => Ok(value.clone()),
            MetaExpr::Ident { span, name } => self.lookup_throw(*span, name),
            MetaExpr::LetBinding { span, expr, name } => {
                let val = self.eval(*span, expr)?;
                self.insert_binding(*span, name.clone(), val);
                Ok(self.empty_token_list.clone())
            }
            MetaExpr::FnCall { span, name, args } => {
                self.eval_fn_call(span, name, args)
            }
            MetaExpr::Group {
                span,
                delimiter,
                contents,
            } => {
                let mut res = Vec::new();
                self.eval_stmt_list_to_stream(&mut res, *span, contents)?;
                Ok(Rc::new(MetaValue::TokenTree(TokenTree::Group(
                    Group::new(*delimiter, TokenStream::from_iter(res)),
                ))))
            }
            MetaExpr::StatementList { exprs } => {
                self.eval_stmt_list_to_meta_val(eval_span, exprs)
            }
            MetaExpr::ForExpansion {
                span,
                pattern,
                variants_expr,
                body,
            } => {
                let input_list = self.eval(*span, variants_expr)?;
                let MetaValue::List(list_elems) = &*input_list else {
                    self.errors.push(MetaError {
                        span: *span,
                        message: format!(
                            "cannot iterate over {}",
                            input_list.type_id()
                        ),
                    });
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
                Ok(Rc::new(MetaValue::TokenList(res)))
            }
            MetaExpr::Scope { span, contents } => {
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
            MetaExpr::List { span, exprs } => {
                let mut elements = Vec::new();
                for e in exprs {
                    elements.push(self.eval(*span, e)?);
                }
                Ok(Rc::new(MetaValue::List(elements)))
            }
            MetaExpr::Tuple { span, exprs } => {
                let mut elements = Vec::new();
                for e in exprs {
                    elements.push(self.eval(*span, e)?);
                }
                Ok(Rc::new(MetaValue::Tuple(elements)))
            }
        }
    }

    fn eval_fn_call(
        &mut self,
        span: &Span,
        name: &Rc<str>,
        args: &Vec<Rc<MetaExpr>>,
    ) -> std::result::Result<Rc<MetaValue>, ()> {
        match &*self.lookup_throw(*span, name)? {
            MetaValue::Fn(function) => {
                if function.params.len() != args.len() {
                    self.errors.push(MetaError {
                                        span: *span,
                                        message: format!(
                                            "function `{name}` with {} parameters called with {} arguments",
                                            function.params.len(),
                                            args.len()
                                        ),
                                    });
                    return Err(());
                }
                self.scopes.push(Default::default());
                for (i, arg) in args.iter().enumerate() {
                    let Ok(val) = self.eval(*span, arg) else {
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
                    let Ok(val) = self.eval(*span, arg) else {
                        return Err(());
                    };
                    param_bindings.push(val);
                }
                (builtin_fn.builtin)(self, *span, &param_bindings)
            }
            other => {
                self.errors.push(MetaError {
                    span: *span,
                    message: format!(
                        "value of type {} is not callable",
                        other.type_id()
                    ),
                });
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
    fn evaluate_to_token_stream(
        &mut self,
        eval_span: Span,
        expr: &MetaExpr,
    ) -> TokenStream {
        match self.eval(eval_span, expr) {
            Ok(val) => {
                let mut res = Vec::new();
                if self
                    .append_value_to_stream(&mut res, eval_span, &val)
                    .is_err()
                {
                    return self.expand_errors();
                }
                TokenStream::from_iter(res)
            }
            Err(()) => self.expand_errors(),
        }
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

fn parse_literal(token: &TokenTree) -> Option<(Span, Rc<MetaValue>)> {
    if let TokenTree::Literal(lit) = token {
        let span = lit.span();
        let s = lit.to_string();
        if s.starts_with('"') {
            Some((
                span,
                Rc::new(MetaValue::String(Rc::from(
                    s[1..s.len() - 1].to_string(),
                ))),
            ))
        } else if let Ok(n) = s.parse::<i64>() {
            Some((span, Rc::new(MetaValue::Int(n))))
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
        if tokens.is_empty() {
            self.errors.push(MetaError {
                span: parent_span,
                message: "unexpected end of input".to_owned(),
            });
            return Err(());
        }
        todo!()
    }
    fn parse_value<'a>(
        &mut self,
        parent_span: Span,
        token: &TokenTree,
    ) -> Result<Rc<MetaExpr>> {
        match token {
            TokenTree::Group(group) => {
                let content = group.stream().into_iter().collect::<Vec<_>>();
                match group.delimiter() {
                    Delimiter::Parenthesis => {
                        let list = self
                            .parse_comma_separated(group.span(), &content)?;
                        return Ok(Rc::new(MetaExpr::Tuple {
                            span: group.span(),
                            exprs: list,
                        }));
                    }
                    Delimiter::Brace => {
                        self.parse_scope(group.span(), &content)
                    }
                    Delimiter::Bracket => {
                        let list = self
                            .parse_comma_separated(group.span(), &content)?;
                        return Ok(Rc::new(MetaExpr::List {
                            span: group.span(),
                            exprs: list,
                        }));
                    }
                    Delimiter::None => {
                        let (expr, rest) =
                            self.parse_expr(group.span(), &content)?;
                        if !rest.is_empty() {
                            self.error(
                                rest[0].span(),
                                "stay token after end of expression",
                            );
                            return Err(());
                        }
                        Ok(expr)
                    }
                }
            }

            TokenTree::Ident(ident) => {
                let span = ident.span();
                let name = Rc::from(ident.to_string());

                if &*name == "let" && tokens.len() > 1 {
                    return self.parse_let(&tokens[1..], span);
                }
                if &*name == "fn" && tokens.len() > 1 {
                    return self.parse_fn(&tokens[1..], span);
                }
                if &*name == "for" && tokens.len() > 1 {
                    return self.parse_for(&tokens[1..], span);
                }

                // Check if it's a function call
                if tokens.len() > 1 {
                    if let TokenTree::Group(group) = &tokens[1] {
                        if group.delimiter() == Delimiter::Parenthesis {
                            let args =
                                group.stream().into_iter().collect::<Vec<_>>();
                            let (parsed_args, _) =
                                self.parse_comma_separated(&args)?;
                            return Ok((
                                Rc::new(MetaExpr::FnCall {
                                    span,
                                    name,
                                    args: parsed_args,
                                }),
                                &tokens[2..],
                            ));
                        }
                    }
                }

                Ok((Rc::new(MetaExpr::Ident { span, name }), &tokens[1..]))
            }

            TokenTree::Literal(lit) => {
                let span = lit.span();
                if let Some((_, value)) = parse_literal(&tokens[0]) {
                    Ok((
                        Rc::new(MetaExpr::Literal { span, value }),
                        &tokens[1..],
                    ))
                } else {
                    self.errors.push(MetaError {
                        span,
                        message: "invalid literal".to_owned(),
                    });
                    Err(())
                }
            }

            token => {
                self.error(token.span(), "unexpected token");
                Err(())
            }
        }
        // TODO:
    }

    fn error(&mut self, span: Span, message: impl Into<String>) {
        self.errors.push(MetaError {
            span,
            message: message.into(),
        });
    }

    fn parse_scope(
        &self,
        span: Span,
        content: &[TokenTree],
    ) -> std::result::Result<Rc<MetaExpr>, ()> {
        todo!()
    }

    fn parse_let(
        &mut self,
        tokens: &[TokenTree],
        let_span: Span,
    ) -> Result<(Rc<MetaExpr>, &[TokenTree])> {
        if tokens.is_empty() {
            self.errors.push(MetaError {
                span: let_span,
                message: "expected identifier after let".to_owned(),
            });
            return Err(());
        }

        let (name_span, name) = parse_ident(&tokens[0]).ok_or_else(|| {
            self.errors.push(MetaError {
                span: tokens[0].span(),
                message: "expected identifier".to_owned(),
            });
        })?;

        let mut is_eq = false;
        if let Some(TokenTree::Punct(p)) = tokens.get(2) {
            if p.as_char() == '=' {
                is_eq = true;
            }
        }

        if !is_eq {
            self.errors.push(MetaError {
                span: tokens.get(2).map(|t| t.span()).unwrap_or(name_span),
                message: "expected = after identifier".to_owned(),
            });
            return Err(());
        }

        let (expr, rest) = self.parse_expr(&tokens[2..])?;

        Ok((
            Rc::new(MetaExpr::LetBinding {
                span: let_span,
                name,
                expr,
            }),
            rest,
        ))
    }

    fn parse_fn(
        &mut self,
        tokens: &[TokenTree],
        fn_span: Span,
    ) -> Result<(Rc<MetaExpr>, &[TokenTree])> {
        // Parse function name
        let (name_span, name) = parse_ident(&tokens[0]).ok_or_else(|| {
            self.errors.push(MetaError {
                span: tokens[0].span(),
                message: "expected function name".to_owned(),
            });
        })?;

        // Parse parameter list
        if tokens.len() < 2 {
            self.errors.push(MetaError {
                span: name_span,
                message: "expected parameter list".to_owned(),
            });
            return Err(());
        }

        let TokenTree::Group(param_group) = &tokens[1] else {
            self.errors.push(MetaError {
                span: tokens[1].span(),
                message: "expected parameter list".to_owned(),
            });
            return Err(());
        };

        if param_group.delimiter() != Delimiter::Parenthesis {
            self.errors.push(MetaError {
                span: param_group.span(),
                message: "expected parentheses around parameter list"
                    .to_owned(),
            });
            return Err(());
        }

        // Parse parameters
        let param_tokens =
            param_group.stream().into_iter().collect::<Vec<_>>();
        let mut params = Vec::new();
        let mut rest = &param_tokens[..];

        while !rest.is_empty() {
            let (param_span, param_name) =
                parse_ident(&rest[0]).ok_or_else(|| {
                    self.errors.push(MetaError {
                        span: rest[0].span(),
                        message: "expected parameter name".to_owned(),
                    });
                })?;

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
                    self.errors.push(MetaError {
                        span: rest[0].span(),
                        message: "expected comma between parameters"
                            .to_owned(),
                    });
                    return Err(());
                }
            }
        }

        // Parse function body
        if tokens.len() < 3 {
            self.errors.push(MetaError {
                span: param_group.span(),
                message: "expected function body".to_owned(),
            });
            return Err(());
        }

        let TokenTree::Group(body_group) = &tokens[2] else {
            self.errors.push(MetaError {
                span: tokens[2].span(),
                message: "expected function body".to_owned(),
            });
            return Err(());
        };

        if body_group.delimiter() != Delimiter::Brace {
            self.errors.push(MetaError {
                span: body_group.span(),
                message: "expected braces around function body".to_owned(),
            });
            return Err(());
        }

        let body_tokens = body_group.stream().into_iter().collect::<Vec<_>>();
        let body = match self.parse_body(&body_tokens) {
            Ok(MetaExpr::StatementList { exprs }) => exprs,
            _ => return Err(()),
        };

        Ok((
            Rc::new(MetaExpr::FnDecl(Rc::new(Function {
                span: fn_span,
                name: name.clone(),
                params,
                body,
            }))),
            &tokens[3..],
        ))
    }

    fn parse_for(
        &mut self,
        tokens: &[TokenTree],
        for_span: Span,
    ) -> Result<(Rc<MetaExpr>, &[TokenTree])> {
        // Parse pattern
        let (pattern, rest) = self.parse_pattern(&tokens[0])?;

        // Expect "in" keyword
        if rest.is_empty() {
            self.errors.push(MetaError {
                span: for_span,
                message: "expected 'in' after pattern".to_owned(),
            });
            return Err(());
        }

        let TokenTree::Ident(in_ident) = &rest[0] else {
            self.errors.push(MetaError {
                span: rest[0].span(),
                message: "expected 'in'".to_owned(),
            });
            return Err(());
        };

        if in_ident.to_string() != "in" {
            self.errors.push(MetaError {
                span: in_ident.span(),
                message: "expected 'in'".to_owned(),
            });
            return Err(());
        }

        // Parse variants expression
        if rest.len() < 2 {
            self.errors.push(MetaError {
                span: in_ident.span(),
                message: "expected expression after 'in'".to_owned(),
            });
            return Err(());
        }

        let (variants_expr, rest) = self.parse_expr(&rest[1..])?;

        // Parse body
        if rest.is_empty() {
            self.errors.push(MetaError {
                span: variants_expr.span(),
                message: "expected for loop body".to_owned(),
            });
            return Err(());
        }

        let TokenTree::Group(body_group) = &rest[0] else {
            self.errors.push(MetaError {
                span: rest[0].span(),
                message: "expected for loop body".to_owned(),
            });
            return Err(());
        };

        if body_group.delimiter() != Delimiter::Brace {
            self.errors.push(MetaError {
                span: body_group.span(),
                message: "expected braces around for loop body".to_owned(),
            });
            return Err(());
        }

        let body_tokens = body_group.stream().into_iter().collect::<Vec<_>>();
        let body = match self.parse_body(&body_tokens) {
            Ok(MetaExpr::StatementList { exprs }) => exprs,
            _ => return Err(()),
        };

        Ok((
            Rc::new(MetaExpr::ForExpansion {
                span: for_span,
                pattern,
                variants_expr,
                body,
            }),
            &rest[1..],
        ))
    }

    fn parse_pattern(
        &mut self,
        token: &TokenTree,
    ) -> Result<(Pattern, &[TokenTree])> {
        match token {
            TokenTree::Ident(ident) => Ok((
                Pattern::Ident(BindingParameter {
                    span: ident.span(),
                    name: Rc::from(ident.to_string()),
                }),
                &[],
            )),
            TokenTree::Group(group) => {
                let tokens = group.stream().into_iter().collect::<Vec<_>>();
                let mut patterns = Vec::new();
                let mut rest = &tokens[..];

                while !rest.is_empty() {
                    let (pattern, new_rest) = self.parse_pattern(&rest[0])?;
                    patterns.push(pattern);
                    rest = new_rest;

                    if !rest.is_empty() {
                        if let TokenTree::Punct(p) = &rest[0] {
                            if p.as_char() == ',' {
                                rest = &rest[1..];
                                continue;
                            }
                        }
                    }
                }

                match group.delimiter() {
                    Delimiter::Parenthesis => Ok((
                        Pattern::Tuple {
                            span: group.span(),
                            elems: patterns,
                        },
                        &[],
                    )),
                    Delimiter::Bracket => Ok((
                        Pattern::List {
                            span: group.span(),
                            elems: patterns,
                        },
                        &[],
                    )),
                    _ => {
                        self.errors.push(MetaError {
                            span: group.span(),
                            message: "invalid pattern delimiter".to_owned(),
                        });
                        Err(())
                    }
                }
            }
            _ => {
                self.errors.push(MetaError {
                    span: token.span(),
                    message: "invalid pattern".to_owned(),
                });
                Err(())
            }
        }
    }

    fn parse_comma_separated(
        &mut self,
        parent_span: Span,
        tokens: &[TokenTree],
    ) -> Result<Vec<Rc<MetaExpr>>> {
        let mut exprs = Vec::new();
        let mut rest = tokens;

        while !rest.is_empty() {
            let (expr, new_rest) = self.parse_expr(parent_span, rest)?;
            exprs.push(expr);
            rest = new_rest;

            if rest.is_empty() {
                break;
            }
            if let TokenTree::Punct(p) = &rest[0] {
                if p.as_char() == ',' {
                    rest = &rest[1..];
                    continue;
                }
            }
            break;
        }

        Ok(exprs)
    }

    fn parse_body(
        &mut self,
        parent_span: Span,
        tokens: &[TokenTree],
    ) -> Result<MetaExpr> {
        let mut exprs = Vec::new();
        let mut rest = tokens;

        while !rest.is_empty() {
            let (expr, new_rest) = self.parse_expr(parent_span, rest)?;
            exprs.push(expr);
            rest = new_rest;
        }

        Ok(MetaExpr::StatementList { exprs })
    }
}
