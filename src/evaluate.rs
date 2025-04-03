use std::{collections::HashMap, rc::Rc};

use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream,
    TokenTree,
};

use super::ast::{
    Binding, BuiltinFn, Context, MetaExpr, MetaValue, Pattern, Scope,
    ScopeKind,
};

type Result<T> = std::result::Result<T, ()>;

fn comma_token(span: Span) -> TokenTree {
    let mut punct = Punct::new(',', Spacing::Alone);
    punct.set_span(span);
    TokenTree::Punct(punct)
}

impl Context {
    pub fn insert_builtins(&mut self) {
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
    fn match_and_bind_pattern(
        &mut self,
        pat: &Pattern,
        val: Rc<MetaValue>,
    ) -> Result<()> {
        match pat {
            Pattern::Ident(bind) => {
                self.insert_binding(
                    bind.span,
                    bind.name.clone(),
                    bind.super_bound,
                    val,
                );
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
                    self.error(
                        *span,
                        format!(
                            "tuple pattern missmatch: expected length {}, got {}",
                            pat_bindings.len(),
                            val_elems.len()
                        ),
                    );
                    return Err(());
                }
                for i in 0..val_elems.len() {
                    self.match_and_bind_pattern(
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
                    self.error(
                        *span,
                        format!(
                            "list pattern missmatch: expected length {}, got {}",
                            pat_bindings.len(),
                            val_elems.len()
                        ),
                    );
                    return Err(());
                }
                for i in 0..val_elems.len() {
                    self.match_and_bind_pattern(
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
        self.insert_binding(
            Span::call_site(),
            Rc::from(name),
            false,
            builtin_fn_v,
        );
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
            match &*args[0] {
                MetaValue::String { value: s, span: _ } => {
                    Ok(Rc::new(MetaValue::String {
                        value: Rc::from(f(s)),
                        span: None,
                    }))
                }
                MetaValue::Token(t) => Ok(Rc::new(MetaValue::Token(
                    TokenTree::Ident(Ident::new(&f(&t.to_string()), t.span())),
                ))),
                _ => {
                    ctx.error(
                        span,
                        format!(
                            "builtin function `{name}` expects a string, got a {}",
                            args[0].type_id()
                        ),
                    );
                    Err(())
                }
            }
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
    pub fn lookup(
        &self,
        name: &str,
        super_only: bool,
    ) -> Option<Rc<MetaValue>> {
        for scope in &self.scopes {
            if let Some(binding) = scope.bindings.get(name) {
                if binding.super_bound || !super_only {
                    return Some(binding.value.clone());
                }
            }
        }
        None
    }
    pub fn eval_to_token_stream(
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
    fn push_eval_scope(&mut self) {
        self.scopes.push(Scope {
            kind: ScopeKind::Evaluation,
            bindings: HashMap::new(),
        });
    }
    fn eval(&mut self, expr: &MetaExpr) -> Result<Rc<MetaValue>> {
        match expr {
            MetaExpr::Literal { span: _, value } => Ok(value.clone()),
            MetaExpr::Ident { span, name } => {
                if let Some(expr) = self.lookup(name, false) {
                    return Ok(expr);
                }
                Ok(Rc::new(MetaValue::Token(TokenTree::Ident(Ident::new(
                    name, *span,
                )))))
            }
            MetaExpr::LetBinding {
                span: _,
                pattern,
                expr,
            } => {
                let val = self.eval(expr.as_ref().unwrap())?;
                self.match_and_bind_pattern(pattern, val)?;
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
                        variants_expr.span(),
                        format!(
                            "cannot iterate over {}",
                            input_list.type_id()
                        ),
                    );
                    return Err(());
                };
                let mut res = Vec::new();

                for elem in list_elems {
                    self.push_eval_scope();
                    if self
                        .match_and_bind_pattern(pattern, elem.clone())
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
                    self.scopes.pop();
                }
                Ok(Rc::new(MetaValue::Tokens(res)))
            }
            MetaExpr::Scope {
                span,
                body: contents,
            } => {
                self.push_eval_scope();
                let res = self.eval_stmt_list_to_meta_val(*span, contents);
                self.scopes.pop();
                res
            }
            MetaExpr::FnDecl(f) => {
                self.insert_binding(
                    f.span,
                    f.name.clone(),
                    false,
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
            MetaExpr::Range {
                span: _,
                inclusive,
                lhs,
                rhs,
            } => {
                fn eval_expr(ctx: &mut Context, x: &MetaExpr) -> Result<i64> {
                    let mv = ctx.eval(x)?;
                    let MetaValue::Int { value, .. } = &*mv else {
                        ctx.error(
                            x.span(),
                            format!(
                                "range expression bound must be `int`, not `{}`",
                                mv.type_id()
                            ),
                        );
                        return Err(());
                    };
                    Ok(*value)
                }

                // TODO: for now these are simply eager similar to python2
                // we eventually want to change that probably, and create
                // a python3 debacle only saved by the fact that we dont have
                // users
                let mut res = Vec::new();
                let lhs_v = if let Some(lhs) = lhs {
                    eval_expr(self, lhs)?
                } else {
                    0
                };
                let rhs_v = if let Some(rhs) = rhs {
                    let rhs_v = eval_expr(self, rhs)?;
                    if *inclusive {
                        rhs_v + 1
                    } else {
                        rhs_v
                    }
                } else {
                    todo!()
                };
                for i in lhs_v..rhs_v {
                    res.push(Rc::new(MetaValue::Int {
                        value: i,
                        span: None,
                    }));
                }
                Ok(Rc::new(MetaValue::List(res)))
            }
            MetaExpr::ExpandPattern(ep) => {
                let input_list = self.eval(&ep.for_expr)?;
                let MetaValue::List(list_elems) = &*input_list else {
                    self.error(
                        ep.for_expr.span(),
                        format!(
                            "cannot iterate over {}",
                            input_list.type_id()
                        ),
                    );
                    return Err(());
                };
                let mut res = Vec::new();
                for (i, elem) in list_elems.iter().enumerate() {
                    self.push_eval_scope();
                    if i != 0 {
                        res.push(TokenTree::Punct(Punct::new(
                            '|',
                            Spacing::Alone,
                        )));
                    }
                    if self
                        .match_and_bind_pattern(&ep.for_pattern, elem.clone())
                        .is_err()
                    {
                        self.scopes.pop();
                        return Err(());
                    }
                    if self
                        .eval_stmt_list_to_stream(
                            &mut res,
                            ep.span,
                            &ep.match_arm_patterns,
                        )
                        .is_err()
                    {
                        self.scopes.pop();
                        return Err(());
                    }

                    self.scopes.pop();
                }
                self.eval_stmt_list_to_stream(
                    &mut res,
                    ep.span,
                    &ep.match_arm_guard,
                )?;
                self.eval_stmt_list_to_stream(
                    &mut res,
                    ep.span,
                    &ep.match_arm_body,
                )?;
                Ok(Rc::new(MetaValue::Tokens(res)))
            }
        }
    }

    fn eval_fn_call(
        &mut self,
        span: &Span,
        name: &Rc<str>,
        args: &Vec<Rc<MetaExpr>>,
    ) -> std::result::Result<Rc<MetaValue>, ()> {
        let Some(binding) = self.lookup(name, false) else {
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
                self.push_eval_scope();
                for (i, arg) in args.iter().enumerate() {
                    let Ok(val) = self.eval(arg) else {
                        self.scopes.pop();
                        return Err(());
                    };
                    let param = &function.params[i];
                    self.insert_binding(
                        param.span,
                        param.name.clone(),
                        param.super_bound,
                        val,
                    );
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
        super_bound: bool,
        value: Rc<MetaValue>,
    ) {
        let binding = Binding {
            span,
            super_bound,
            value,
        };
        self.scopes
            .last_mut()
            .unwrap()
            .bindings
            .insert(name.clone(), binding);
    }
    pub fn expand_errors(&self) -> TokenStream {
        let mut errors = Vec::new();
        for err in &self.errors {
            errors.extend_from_slice(&[
                TokenTree::Ident(Ident::new("compile_error", err.span)),
                TokenTree::Punct({
                    let mut punct = Punct::new('!', Spacing::Alone);
                    punct.set_span(err.span);
                    punct
                }),
                TokenTree::Group({
                    let mut group = Group::new(Delimiter::Brace, {
                        TokenStream::from_iter(vec![TokenTree::Literal({
                            let mut string = Literal::string(&err.message);
                            string.set_span(err.span);
                            string
                        })])
                    });
                    group.set_span(err.span);
                    group
                }),
            ]);
        }
        TokenStream::from_iter(errors)
    }
}
