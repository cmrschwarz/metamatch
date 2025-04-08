use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    rc::Rc,
};

use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream,
    TokenTree,
};

use super::ast::{
    BinaryOpKind, Binding, BuiltinFn, Context, EvalError, Function, Lambda,
    MetaExpr, MetaValue, Pattern, Scope, ScopeKind, UnaryOpKind,
};

type Result<T> = std::result::Result<T, EvalError>;

fn comma_token(span: Span) -> TokenTree {
    let mut punct = Punct::new(',', Spacing::Alone);
    punct.set_span(span);
    TokenTree::Punct(punct)
}

enum Iterable<'a> {
    Tuple(&'a [Rc<MetaValue>]),
    List(Ref<'a, Vec<Rc<MetaValue>>>),
    Other(Vec<Rc<MetaValue>>),
}
impl<'a> Iterable<'a> {
    fn len(&self) -> usize {
        match self {
            Iterable::Tuple(l) => l.len(),
            Iterable::List(l) => l.len(),
            Iterable::Other(l) => l.len(),
        }
    }
    fn index(&self, i: usize) -> Rc<MetaValue> {
        match self {
            Iterable::Tuple(l) => l[i].clone(),
            Iterable::List(l) => l[i].clone(),
            Iterable::Other(l) => l[i].clone(),
        }
    }
    fn from_value(v: &'a MetaValue) -> Option<Self> {
        let res = match v {
            MetaValue::Tokens(token_trees) => {
                let mut src_list = Vec::new();
                for t in token_trees {
                    src_list.push(Rc::new(MetaValue::Token(t.clone())));
                }
                Iterable::Other(src_list)
            }

            MetaValue::List(list) => Iterable::List(list.borrow()),
            MetaValue::Tuple(meta_values) => Iterable::Tuple(meta_values),
            MetaValue::Token(_)
            | MetaValue::String { .. }
            | MetaValue::Int { .. }
            | MetaValue::Char { .. }
            | MetaValue::Float { .. }
            | MetaValue::Bool { .. }
            | MetaValue::Fn(_)
            | MetaValue::Lambda(_)
            | MetaValue::BuiltinFn(_) => {
                return None;
            }
        };
        Some(res)
    }
}
impl<'a, 'b> IntoIterator for &'b Iterable<'a> {
    type Item = &'b Rc<MetaValue>;

    type IntoIter = std::slice::Iter<'b, Rc<MetaValue>>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Iterable::Tuple(l) => l.iter(),
            Iterable::List(l) => l.as_slice().iter(),
            Iterable::Other(l) => l.as_slice().iter(),
        }
    }
}

enum Callable<'a> {
    BuiltinFn(&'a BuiltinFn),
    Fn(&'a Function),
    Lambda(&'a Lambda),
}

impl<'a> Callable<'a> {
    fn from_value(v: &'a MetaValue) -> Option<Self> {
        let res = match v {
            MetaValue::BuiltinFn(f) => Callable::BuiltinFn(f),
            MetaValue::Fn(f) => Callable::Fn(f),
            MetaValue::Lambda(l) => Callable::Lambda(l),
            MetaValue::Token(_)
            | MetaValue::Tokens(_)
            | MetaValue::Int { .. }
            | MetaValue::Char { .. }
            | MetaValue::Float { .. }
            | MetaValue::Bool { .. }
            | MetaValue::String { .. }
            | MetaValue::List(_)
            | MetaValue::Tuple(_) => return None,
        };
        Some(res)
    }

    fn call(
        &self,
        ctx: &mut Context,
        span: Span,
        args: &[Rc<MetaValue>],
    ) -> Result<Rc<MetaValue>> {
        match self {
            Callable::Fn(function) => {
                if function.params.len() != args.len() {
                    ctx.error(
                        span,
                        format!(
                        "function `{}` expects {} parameter{}, called with {}",
                        function.name,
                        function.params.len(),
                        if function.params.len() > 1 {"s"} else {""},
                        args.len()
                    ),
                    );
                    return Err(EvalError::Error);
                }
                ctx.push_eval_scope();
                for (i, arg) in args.iter().enumerate() {
                    ctx.match_and_bind_pattern(
                        &function.params[i],
                        arg.clone(),
                        false,
                    )?;
                }
                let res = ctx.eval_stmt_list_to_meta_val(span, &function.body);
                ctx.scopes.pop();
                res
            }
            Callable::Lambda(lambda) => {
                if lambda.params.len() != args.len() {
                    ctx.error(
                        span,
                        format!(
                            "lambda expects {} parameter{}, called with {}",
                            lambda.params.len(),
                            if lambda.params.len() > 1 { "s" } else { "" },
                            args.len()
                        ),
                    );
                    return Err(EvalError::Error);
                }
                ctx.push_eval_scope();
                for (i, arg) in args.iter().enumerate() {
                    ctx.match_and_bind_pattern(
                        &lambda.params[i],
                        arg.clone(),
                        false,
                    )?;
                }
                let res = ctx.eval(&lambda.body);
                ctx.scopes.pop();
                res
            }
            Callable::BuiltinFn(builtin_fn) => {
                if let Some(param_count) = builtin_fn.param_count {
                    if param_count != args.len() {
                        ctx.error(
                        span,
                        format!(
                            "function `{}` expects {} parameter{}, called with {}",
                            builtin_fn.name,
                            param_count,
                            if param_count > 1 {"s"} else {""},
                            args.len()
                        ),
                    );
                        return Err(EvalError::Error);
                    }
                }
                (builtin_fn.builtin)(ctx, span, args)
            }
        }
    }
}

fn builtin_fn_zip(
    ctx: &mut Context,
    callsite: Span,
    args: &[Rc<MetaValue>],
) -> Result<Rc<MetaValue>> {
    let mut source_lists: Vec<Iterable> = Vec::new();
    for (i, arg) in args.iter().enumerate() {
        let Some(iterable) = Iterable::from_value(arg) else {
            ctx.error(
                callsite,
                format!(
                    "`zip()` arguments must be iterable, `{}` is not (argument {})",
                    arg.kind(),
                    i + 1
                ),
            );
            return Err(EvalError::Error);
        };
        source_lists.push(iterable);
    }

    let first_len = source_lists.first().map(|l| l.len()).unwrap_or_default();
    for (i, sl) in source_lists.iter().enumerate().skip(1) {
        let len = sl.len();
        if len != first_len {
            ctx.error(
                    callsite,
                    format!(
                        "`zip()` argument {} has length {}, the previous arguments have length {}",
                       first_len,
                       i + 1,
                       len
                    ),
                );
            return Err(EvalError::Error);
        }
    }
    let mut res = Vec::new();
    for i in 0..first_len {
        let mut tup = Vec::new();
        for src in &source_lists {
            tup.push(src.index(i));
        }
        res.push(Rc::new(MetaValue::Tuple(tup)));
    }

    Ok(Rc::new(MetaValue::List(RefCell::new(res))))
}

fn builtin_fn_len(
    ctx: &mut Context,
    callsite: Span,
    args: &[Rc<MetaValue>],
) -> Result<Rc<MetaValue>> {
    let v = match &*args[0] {
        MetaValue::Tokens(token_trees) => token_trees.len(),
        MetaValue::List(list) => list.borrow().len(),
        MetaValue::Tuple(tup) => tup.len(),
        MetaValue::String { value: s, span: _ } => s.len(),
        MetaValue::Bool { .. }
        | MetaValue::Token(_)
        | MetaValue::Int { .. }
        | MetaValue::Char { .. }
        | MetaValue::Float { .. }
        | MetaValue::Fn(_)
        | MetaValue::Lambda(_)
        | MetaValue::BuiltinFn(_) => {
            ctx.error(
                callsite,
                format!("value of type `{}` has no len()", args[0].kind()),
            );
            return Err(EvalError::Error);
        }
    };
    Ok(Rc::new(MetaValue::Int {
        value: v as i64,
        span: None,
    }))
}

fn builtin_fn_chars(
    ctx: &mut Context,
    callsite: Span,
    args: &[Rc<MetaValue>],
) -> Result<Rc<MetaValue>> {
    fn to_char_list(
        chars: impl IntoIterator<Item = char>,
    ) -> Vec<Rc<MetaValue>> {
        let mut res = Vec::new();
        for c in chars.into_iter() {
            res.push(Rc::new(MetaValue::Char {
                value: c,
                span: None,
            }));
        }
        res
    }
    let chars = match &*args[0] {
        MetaValue::Token(t) => match t {
            TokenTree::Group(_) => {
                ctx.error(callsite, "cannot call `chars()` on a Token Group");
                return Err(EvalError::Error);
            }
            TokenTree::Ident(ident) => to_char_list(ident.to_string().chars()),
            TokenTree::Punct(punct) => to_char_list([punct.as_char()]),
            TokenTree::Literal(literal) => {
                to_char_list(literal.to_string().chars())
            }
        },
        MetaValue::String { value, span: _ } => to_char_list(value.chars()),
        MetaValue::Tokens(..)
        | MetaValue::List(..)
        | MetaValue::Tuple(..)
        | MetaValue::Bool { .. }
        | MetaValue::Int { .. }
        | MetaValue::Char { .. }
        | MetaValue::Float { .. }
        | MetaValue::Fn(_)
        | MetaValue::Lambda(_)
        | MetaValue::BuiltinFn(_) => {
            ctx.error(
                callsite,
                format!(
                    "function `chars()` expects a string, got a {}",
                    args[0].kind()
                ),
            );
            return Err(EvalError::Error);
        }
    };
    Ok(Rc::new(MetaValue::List(RefCell::new(chars))))
}

fn builtin_fn_bytes(
    ctx: &mut Context,
    callsite: Span,
    args: &[Rc<MetaValue>],
) -> Result<Rc<MetaValue>> {
    fn to_byte_list(
        bytes: impl IntoIterator<Item = u8>,
    ) -> Vec<Rc<MetaValue>> {
        let mut res = Vec::new();
        for b in bytes.into_iter() {
            res.push(Rc::new(MetaValue::Int {
                value: b as i64,
                span: None,
            }));
        }
        res
    }
    let chars = match &*args[0] {
        MetaValue::Token(t) => match t {
            TokenTree::Group(_) => {
                ctx.error(callsite, "cannot call `bytes()` on a Token Group");
                return Err(EvalError::Error);
            }
            TokenTree::Ident(ident) => to_byte_list(ident.to_string().bytes()),
            TokenTree::Punct(punct) => to_byte_list([punct.as_char() as u8]),
            TokenTree::Literal(literal) => {
                to_byte_list(literal.to_string().bytes())
            }
        },
        MetaValue::String { value, span: _ } => to_byte_list(value.bytes()),
        MetaValue::Char { value, span: _ } => {
            let mut bytes = [0; 4];
            to_byte_list(value.encode_utf8(&mut bytes).bytes())
        }
        MetaValue::Tokens(..)
        | MetaValue::List(..)
        | MetaValue::Tuple(..)
        | MetaValue::Bool { .. }
        | MetaValue::Int { .. }
        | MetaValue::Float { .. }
        | MetaValue::Fn(_)
        | MetaValue::Lambda(_)
        | MetaValue::BuiltinFn(_) => {
            ctx.error(
                callsite,
                format!(
                    "function `bytes()` is not applicable to `{}`",
                    args[0].kind()
                ),
            );
            return Err(EvalError::Error);
        }
    };
    Ok(Rc::new(MetaValue::List(RefCell::new(chars))))
}

fn builtin_fn_map(
    ctx: &mut Context,
    callsite: Span,
    args: &[Rc<MetaValue>],
) -> Result<Rc<MetaValue>> {
    let Some(iterable) = Iterable::from_value(&args[0]) else {
        ctx.error(
            callsite,
            format!(
                "first argument to `map()` must be iterable, `{}` is not",
                args[0].kind()
            ),
        );
        return Err(EvalError::Error);
    };

    let Some(callable) = Callable::from_value(&args[1]) else {
        ctx.error(
            callsite,
            format!(
                "second argument to `map()` must be callable, `{}` is not",
                args[0].kind()
            ),
        );
        return Err(EvalError::Error);
    };

    let mut res = Vec::new();

    for elem in &iterable {
        res.push(callable.call(ctx, callsite, &[elem.clone()])?);
    }

    Ok(Rc::new(MetaValue::List(RefCell::new(res))))
}

fn value_to_str(v: &MetaValue) -> Option<Rc<str>> {
    match v {
        MetaValue::String { value, span: _ } => Some(value.clone()),
        MetaValue::Char { value, span: _ } => {
            let mut data = [0; 4];
            Some(Rc::from(value.encode_utf8(&mut data)))
        }
        MetaValue::Token(t) => match t {
            TokenTree::Ident(i) => Some(i.to_string().into()),
            TokenTree::Literal(literal) => Some(literal.to_string().into()),
            TokenTree::Group(_) => None, // TODO:?
            TokenTree::Punct(punct) => {
                Some(punct.as_char().encode_utf8(&mut [0; 4]).into())
            }
        },
        MetaValue::List(..) => None,  // TODO: ?
        MetaValue::Tuple(..) => None, // TODO: ?
        MetaValue::Int { value, span: _ } => Some(Rc::from(value.to_string())),
        MetaValue::Tokens(token_trees) => Some(Rc::from(
            token_trees
                .iter()
                .map(|t| t.to_string())
                .collect::<String>(),
        )),
        MetaValue::Float { value, span: _ } => {
            Some(Rc::from(value.to_string()))
        }
        MetaValue::Bool { value, span: _ } => {
            Some(Rc::from(value.to_string()))
        }

        MetaValue::Fn(..)
        | MetaValue::Lambda(..)
        | MetaValue::BuiltinFn(..) => None,
    }
}

fn builtin_fn_ident(
    ctx: &mut Context,
    callsite: Span,
    args: &[Rc<MetaValue>],
) -> Result<Rc<MetaValue>> {
    let Some(name) = value_to_str(&args[0]) else {
        ctx.error(
            callsite,
            format!("cannot cast `{}` to `ident`", args[0].kind()),
        );
        return Err(EvalError::Error);
    };
    let span = args[0].span();
    // HACK //TODO: better impl of this
    let Ok(ident) = std::panic::catch_unwind(|| Ident::new(&name, span))
    else {
        ctx.error(
            callsite,
            format!("cast failed: invalid identifier `{name}`",),
        );
        return Err(EvalError::Error);
    };
    Ok(Rc::new(MetaValue::Token(TokenTree::Ident(ident))))
}

fn builtin_fn_str(
    ctx: &mut Context,
    callsite: Span,
    args: &[Rc<MetaValue>],
) -> Result<Rc<MetaValue>> {
    let Some(s) = value_to_str(&args[0]) else {
        ctx.error(
            callsite,
            format!("cannot cast `{}` to `str`", args[0].kind()),
        );
        return Err(EvalError::Error);
    };

    Ok(Rc::new(MetaValue::String {
        value: s,
        span: args[0].get_span(),
    }))
}

impl Context {
    pub fn insert_builtins(&mut self) {
        self.insert_builtin_str_fn("lowercase", |s| s.to_lowercase());
        self.insert_builtin_str_fn("uppercase", |s| s.to_uppercase());
        // TODO: camel_case, pascal_case, ...
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
        self.insert_builtin_fn("len", Some(1), builtin_fn_len);
        self.insert_builtin_fn("zip", None, builtin_fn_zip);
        self.insert_builtin_fn("map", Some(2), builtin_fn_map);
        self.insert_builtin_fn("chars", Some(1), builtin_fn_chars);
        self.insert_builtin_fn("bytes", Some(1), builtin_fn_bytes);
        self.insert_builtin_fn("ident", Some(1), builtin_fn_ident);
        self.insert_builtin_fn("str", Some(1), builtin_fn_str);
    }
    fn match_and_bind_pattern(
        &mut self,
        pat: &Pattern,
        val: Rc<MetaValue>,
        suppress_missmatch_error: bool,
    ) -> Result<()> {
        match pat {
            Pattern::Ident(bind) => {
                self.insert_binding(
                    bind.span,
                    bind.name.clone(),
                    bind.mutable,
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
                    if !suppress_missmatch_error {
                        // TODO: more context
                        self.error(
                            *span,
                            format!(
                                "tuple pattern does not match `{}`",
                                val.kind()
                            ),
                        );
                    }
                    return Err(EvalError::PatternMissmatch);
                };
                if pat_bindings.len() != val_elems.len() {
                    if !suppress_missmatch_error {
                        self.error(
                            *span,
                            format!(
                                "tuple pattern missmatch: expected length {}, got {}",
                                pat_bindings.len(),
                                val_elems.len()
                            ),
                        );
                    }
                    return Err(EvalError::PatternMissmatch);
                }
                for i in 0..val_elems.len() {
                    self.match_and_bind_pattern(
                        &pat_bindings[i],
                        val_elems[i].clone(),
                        suppress_missmatch_error,
                    )?;
                }
                Ok(())
            }
            Pattern::List {
                span,
                elems: pat_bindings,
            } => {
                let MetaValue::List(list) = &*val else {
                    if !suppress_missmatch_error {
                        // TODO: more context
                        self.error(
                            *span,
                            format!(
                                "list pattern does not match {}",
                                val.kind()
                            ),
                        );
                    }
                    return Err(EvalError::PatternMissmatch);
                };
                let list = list.borrow();
                if pat_bindings.len() != list.len() {
                    if !suppress_missmatch_error {
                        self.error(
                        *span,
                        format!(
                            "list pattern missmatch: expected length {}, got {}",
                            pat_bindings.len(),
                            list.len()
                        ),
                    );
                    }
                    return Err(EvalError::PatternMissmatch);
                }
                for i in 0..list.len() {
                    self.match_and_bind_pattern(
                        &pat_bindings[i],
                        list[i].clone(),
                        suppress_missmatch_error,
                    )?;
                }
                Ok(())
            }
        }
    }

    fn insert_builtin_fn(
        &mut self,
        name: &'static str,
        param_count: Option<usize>,
        f: impl 'static
            + Fn(&mut Context, Span, &[Rc<MetaValue>]) -> Result<Rc<MetaValue>>,
    ) {
        let name: Rc<str> = Rc::from(name);
        let builtin_fn_v = Rc::new(MetaValue::BuiltinFn(Rc::new(BuiltinFn {
            param_count,
            name: name.clone(),
            builtin: Box::new(f),
        })));
        self.insert_binding(
            Span::call_site(),
            name,
            false,
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
                            "function `{name}` expects a string, got a {}",
                            args[0].kind()
                        ),
                    );
                    Err(EvalError::Error)
                }
            }
        };
        self.insert_builtin_fn(name, Some(1), str_fn);
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
                        "function `{name}` expects a list, got a {}",
                        args[0].kind()
                    ),
                );
                return Err(EvalError::Error);
            };
            Ok(Rc::new(MetaValue::List(RefCell::new(f(&list.borrow())))))
        };
        self.insert_builtin_fn(name, Some(1), list_fn);
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
            MetaValue::Float { value, span } => {
                let mut lit = Literal::f64_unsuffixed(*value);
                lit.set_span(span.unwrap_or(eval_span));
                tgt.push(TokenTree::Literal(lit));
            }
            MetaValue::Char { value, span } => {
                let mut lit = Literal::character(*value);
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
            MetaValue::Fn(_)
            | MetaValue::Lambda(_)
            | MetaValue::BuiltinFn(_) => {
                self.error(eval_span, "function cannot be tokenized");
                return Err(EvalError::Error);
            }
            MetaValue::List(vals) => {
                let mut list = Vec::new();
                for (i, e) in vals.borrow().iter().enumerate() {
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
        debug_assert_eq!(self.scopes.len(), 1, "scopes");
        if self.errors.is_empty() {
            self.push_eval_scope();
            let mut res_stream = Vec::new();
            let res = self.eval_stmt_list_to_stream(
                &mut res_stream,
                eval_span,
                exprs,
            );
            self.pop_scope();
            if res.is_ok() && self.errors.is_empty() {
                return TokenStream::from_iter(res_stream);
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
            let v = self.eval(expr)?;
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
        let mut last_expr = self.empty_token_list.clone();
        for expr in exprs {
            let v = self.eval(expr)?;
            self.append_value_to_stream(&mut res, eval_span, &last_expr)?;
            last_expr = v;
        }
        if res.is_empty() {
            return Ok(last_expr);
        }
        self.append_value_to_stream(&mut res, eval_span, &last_expr)?;
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
            MetaExpr::Break { span, expr } => Err(EvalError::Break {
                value: expr.as_ref().map(|x| self.eval(x)).transpose()?,
                span: *span,
            }),
            MetaExpr::Continue { .. } => Err(EvalError::Continue),
            MetaExpr::Parenthesized { span: _, expr } => self.eval(expr),
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
                self.match_and_bind_pattern(pattern, val, false)?;
                Ok(self.empty_token_list.clone())
            }
            MetaExpr::Call { span, lhs, args } => {
                self.eval_fn_call(*span, lhs, args)
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
            MetaExpr::For {
                span,
                pattern,
                variants_expr,
                body,
            } => {
                let input_list = self.eval(variants_expr)?;
                let MetaValue::List(list_elems) = &*input_list else {
                    self.error(
                        variants_expr.span(),
                        format!("cannot iterate over {}", input_list.kind()),
                    );
                    return Err(EvalError::Error);
                };
                let mut res_tokens = Vec::new();

                for elem in &*list_elems.borrow() {
                    self.push_eval_scope();
                    let mut res = self.match_and_bind_pattern(
                        pattern,
                        elem.clone(),
                        false,
                    );
                    if res.is_ok() {
                        res = self.eval_stmt_list_to_stream(
                            &mut res_tokens,
                            *span,
                            body,
                        );
                    }
                    self.scopes.pop();

                    match res {
                        Ok(()) => continue,
                        Err(EvalError::Continue) => continue,
                        Err(EvalError::Break { value, span }) => {
                            if value.is_some() {
                                self.error(span, "`break` values are not supported in `for`");
                                return Err(EvalError::Error);
                            }
                            break;
                        }
                        Err(EvalError::Error)
                        | Err(EvalError::PatternMissmatch) => {
                            return Err(EvalError::Error);
                        }
                    }
                }
                Ok(Rc::new(MetaValue::Tokens(res_tokens)))
            }
            MetaExpr::Loop { span, body } => {
                let mut res = Vec::new();
                loop {
                    self.push_eval_scope();
                    let v =
                        self.eval_stmt_list_to_stream(&mut res, *span, body);
                    self.pop_scope();

                    match v {
                        Ok(()) => continue,
                        Err(EvalError::Continue) => continue,
                        Err(EvalError::Break { value, span }) => {
                            if let Some(value) = value {
                                if !res.is_empty() {
                                    self.error(span, "`break` value in `loop` with non empty output token list");
                                    return Err(EvalError::Error);
                                }
                                return Ok(value);
                            }
                            break;
                        }
                        Err(EvalError::Error)
                        | Err(EvalError::PatternMissmatch) => {
                            return Err(EvalError::Error);
                        }
                    }
                }
                Ok(Rc::new(MetaValue::Tokens(res)))
            }
            MetaExpr::While {
                condition,
                span,
                body,
            } => {
                let mut res_tokens = Vec::new();
                loop {
                    let condition_val = self.eval(condition)?;
                    let MetaValue::Bool {
                        value: condition,
                        span: _,
                    } = &*condition_val
                    else {
                        self.error(
                            condition.span(),
                            format!(
                                "`while` expression must evaluate as `bool`, not `{}`",
                                condition_val.kind()
                            ),
                        );
                        return Err(EvalError::Error);
                    };
                    if !*condition {
                        break;
                    }
                    self.push_eval_scope();
                    let res = self.eval_stmt_list_to_stream(
                        &mut res_tokens,
                        *span,
                        body,
                    );
                    self.scopes.pop();

                    match res {
                        Ok(()) => continue,
                        Err(EvalError::Continue) => continue,
                        Err(EvalError::Break { value, span }) => {
                            if value.is_some() {
                                self.error(span, "`break` values are not supported in `while`");
                                return Err(EvalError::Error);
                            }
                            break;
                        }
                        Err(EvalError::Error)
                        | Err(EvalError::PatternMissmatch) => {
                            return Err(EvalError::Error);
                        }
                    }
                }
                Ok(Rc::new(MetaValue::Tokens(res_tokens)))
            }
            MetaExpr::WhileLet {
                pattern,
                expr,
                span,
                body,
            } => {
                let input_list = self.eval(expr)?;
                let MetaValue::List(list_elems) = &*input_list else {
                    self.error(
                        expr.span(),
                        format!("cannot iterate over {}", input_list.kind()),
                    );
                    return Err(EvalError::Error);
                };
                let mut res_tokens = Vec::new();

                for elem in &*list_elems.borrow() {
                    self.push_eval_scope();
                    let mut res = self.match_and_bind_pattern(
                        pattern,
                        elem.clone(),
                        true,
                    );
                    let let_pattern_missmatch =
                        matches!(res, Err(EvalError::PatternMissmatch));
                    if res.is_ok() {
                        res = self.eval_stmt_list_to_stream(
                            &mut res_tokens,
                            *span,
                            body,
                        );
                    }
                    self.scopes.pop();

                    match res {
                        Ok(()) => continue,
                        Err(EvalError::Continue) => continue,
                        Err(EvalError::Break { value, span }) => {
                            if value.is_some() {
                                self.error(span, "`break` values are not supported in `while`");
                                return Err(EvalError::Error);
                            }
                            break;
                        }
                        Err(EvalError::PatternMissmatch) => {
                            if let_pattern_missmatch {
                                break;
                            }
                            return Err(EvalError::Error);
                        }
                        Err(EvalError::Error) => {
                            return Err(EvalError::Error);
                        }
                    }
                }
                Ok(Rc::new(MetaValue::Tokens(res_tokens)))
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
                    false,
                    Rc::new(MetaValue::Fn(f.clone())),
                );
                Ok(self.empty_token_list.clone())
            }
            MetaExpr::Lambda(f) => Ok(Rc::new(MetaValue::Lambda(f.clone()))),
            MetaExpr::List { span: _, exprs } => {
                let mut elements = Vec::new();
                for e in exprs {
                    elements.push(self.eval(e)?);
                }
                Ok(Rc::new(MetaValue::List(RefCell::new(elements))))
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
                            condition_val.kind()
                        ),
                    );
                    return Err(EvalError::Error);
                };
                if *condition {
                    self.eval_stmt_list_to_meta_val(*span, body)
                } else if let Some(else_expr) = else_expr {
                    self.eval(else_expr)
                } else {
                    Ok(self.empty_token_list.clone())
                }
            }
            MetaExpr::OpUnary {
                kind,
                span,
                operand,
            } => self.eval_op_unary(kind, span, operand),
            MetaExpr::OpBinary {
                kind,
                span,
                lhs,
                rhs,
            } => self.eval_op_binary(*kind, *span, lhs, rhs),
            MetaExpr::ExpandPattern(ep) => {
                let input_list = self.eval(&ep.for_expr)?;
                let MetaValue::List(list_elems) = &*input_list else {
                    self.error(
                        ep.for_expr.span(),
                        format!("cannot iterate over {}", input_list.kind()),
                    );
                    return Err(EvalError::Error);
                };
                let mut res_tokens = Vec::new();
                for (i, elem) in list_elems.borrow().iter().enumerate() {
                    self.push_eval_scope();
                    if i != 0 {
                        res_tokens.push(TokenTree::Punct(Punct::new(
                            '|',
                            Spacing::Alone,
                        )));
                    }
                    let mut res = self.match_and_bind_pattern(
                        &ep.for_pattern,
                        elem.clone(),
                        false,
                    );

                    if res.is_ok() {
                        res = self.eval_stmt_list_to_stream(
                            &mut res_tokens,
                            ep.span,
                            &ep.match_arm_patterns,
                        );
                    };
                    self.scopes.pop();
                    res?;
                }
                self.eval_stmt_list_to_stream(
                    &mut res_tokens,
                    ep.span,
                    &ep.match_arm_guard,
                )?;
                self.eval_stmt_list_to_stream(
                    &mut res_tokens,
                    ep.span,
                    &ep.match_arm_body,
                )?;
                Ok(Rc::new(MetaValue::Tokens(res_tokens)))
            }
            MetaExpr::ListAccess {
                span,
                list: lhs,
                index: rhs,
            } => {
                let (idx, list) = self.access_list(span, lhs, rhs)?;
                if let MetaValue::List(list) = &*list {
                    return Ok(list.borrow()[idx].clone());
                };
                unreachable!()
            }
        }
    }

    fn access_list(
        &mut self,
        span: &Span,
        list_expr: &MetaExpr,
        index: &MetaExpr,
    ) -> Result<(usize, Rc<MetaValue>)> {
        let list_val = self.eval(list_expr)?;
        let MetaValue::List(list) = &*list_val else {
            self.error(
                *span,
                format!("cannot index into `{}`", list_val.kind()),
            );
            return Err(EvalError::Error);
        };

        let index_val = self.eval(index)?;
        let MetaValue::Int {
            value: index,
            span: _,
        } = &*index_val
        else {
            self.error(
                list_expr.span(),
                format!("cannot index into `{}`", index_val.kind()),
            );
            return Err(EvalError::Error);
        };

        let list = list.borrow();

        let idx = usize::try_from(*index).unwrap_or(usize::MAX);

        if idx >= list.len() {
            self.error(
                *span,
                format!(
                    "index `{index}` is out of bounds for list of length {}",
                    list.len()
                ),
            );
            return Err(EvalError::Error);
        };
        drop(list);
        Ok((idx, list_val))
    }

    fn eval_op_unary(
        &mut self,
        op_kind: &UnaryOpKind,
        span: &Span,
        operand: &Rc<MetaExpr>,
    ) -> Result<Rc<MetaValue>> {
        let operand = self.eval(operand)?;
        match op_kind {
            UnaryOpKind::Minus => match &*operand {
                MetaValue::Int { value, span: _ } => {
                    Ok(Rc::new(MetaValue::Int {
                        value: -*value,
                        span: None,
                    }))
                }
                MetaValue::Float { value, span: _ } => {
                    Ok(Rc::new(MetaValue::Float {
                        value: -value,
                        span: None,
                    }))
                }
                MetaValue::Token(..)
                | MetaValue::Tokens(..)
                | MetaValue::Bool { .. }
                | MetaValue::Char { .. }
                | MetaValue::String { .. }
                | MetaValue::Fn(..)
                | MetaValue::Lambda(..)
                | MetaValue::BuiltinFn(..)
                | MetaValue::List(..)
                | MetaValue::Tuple(..) => {
                    self.error(
                        *span,
                        format!(
                            "{} `{}` is not applicable to `{}`",
                            op_kind.to_str(),
                            op_kind.symbol(),
                            operand.kind()
                        ),
                    );
                    Err(EvalError::Error)
                }
            },
            UnaryOpKind::Not => match &*operand {
                MetaValue::Int { value, span: _ } => {
                    Ok(Rc::new(MetaValue::Int {
                        value: !value,
                        span: None,
                    }))
                }
                MetaValue::Bool { value, span: _ } => {
                    Ok(Rc::new(MetaValue::Bool {
                        value: !value,
                        span: None,
                    }))
                }
                MetaValue::Token(..)
                | MetaValue::Tokens(..)
                | MetaValue::Float { .. }
                | MetaValue::String { .. }
                | MetaValue::Char { .. }
                | MetaValue::Fn(..)
                | MetaValue::Lambda(..)
                | MetaValue::BuiltinFn(..)
                | MetaValue::List(..)
                | MetaValue::Tuple(..) => {
                    self.error(
                        *span,
                        format!(
                            "unary not is not applicable to `{}`",
                            operand.kind(),
                        ),
                    );
                    Err(EvalError::Error)
                }
            },
        }
    }

    fn eval_binary_op_int(
        &mut self,
        span: Span,
        op_kind: BinaryOpKind,
        lhs: i64,
        rhs: i64,
    ) -> Result<Rc<MetaValue>> {
        let res_bool = |value| -> Result<Rc<MetaValue>> {
            Ok(Rc::new(MetaValue::Bool { value, span: None }))
        };
        let res_int = |value| -> Result<Rc<MetaValue>> {
            Ok(Rc::new(MetaValue::Int { value, span: None }))
        };

        match op_kind {
            BinaryOpKind::Equal => res_bool(lhs == rhs),
            BinaryOpKind::NotEqual => res_bool(lhs != rhs),
            BinaryOpKind::LessThan => res_bool(lhs < rhs),
            BinaryOpKind::LessThanOrEqual => res_bool(lhs <= rhs),
            BinaryOpKind::GreaterThan => res_bool(lhs > rhs),
            BinaryOpKind::GreaterThanOrEqual => res_bool(lhs >= rhs),
            BinaryOpKind::Add => res_int(lhs + rhs),
            BinaryOpKind::Sub => res_int(lhs - rhs),
            BinaryOpKind::Mul => res_int(lhs * rhs),
            BinaryOpKind::Div => res_int(lhs / rhs),
            BinaryOpKind::Rem => res_int(lhs % rhs),
            BinaryOpKind::ShiftLeft => res_int(lhs << rhs),
            BinaryOpKind::ShiftRight => res_int(lhs >> rhs),
            BinaryOpKind::BinaryAnd => res_int(lhs & rhs),
            BinaryOpKind::BinaryOr => res_int(lhs | rhs),
            BinaryOpKind::BinaryXor => res_int(lhs ^ rhs),
            BinaryOpKind::RangeExclusive | BinaryOpKind::RangeInclusive => {
                let mut rhs = rhs;
                if op_kind == BinaryOpKind::RangeInclusive {
                    rhs += 1;
                }

                // if its good enough for python 2 it's good enough for us?
                // TODO: maybe not
                let mut res = Vec::new();
                for i in lhs..rhs {
                    res.push(Rc::new(MetaValue::Int {
                        value: i,
                        span: None,
                    }));
                }
                Ok(Rc::new(MetaValue::List(RefCell::new(res))))
            }
            BinaryOpKind::LogicalAnd | BinaryOpKind::LogicalOr => {
                self.error(
                    span,
                    format!(
                        "{} `{}` is not supported on integers",
                        op_kind.to_str(),
                        op_kind.symbol(),
                    ),
                );
                Err(EvalError::Error)
            }
            BinaryOpKind::Assign
            | BinaryOpKind::AddAssign
            | BinaryOpKind::SubAssign
            | BinaryOpKind::MulAssign
            | BinaryOpKind::DivAssign
            | BinaryOpKind::RemAssign
            | BinaryOpKind::BinaryAndAssign
            | BinaryOpKind::BinaryOrAssign
            | BinaryOpKind::BinaryXorAssign
            | BinaryOpKind::ShiftLeftAssign
            | BinaryOpKind::ShiftRightAssign => unreachable!(),
        }
    }

    fn eval_binary_op_float(
        &mut self,
        span: Span,
        op_kind: BinaryOpKind,
        lhs: f64,
        rhs: f64,
    ) -> Result<Rc<MetaValue>> {
        let res_bool = |value| -> Result<Rc<MetaValue>> {
            Ok(Rc::new(MetaValue::Bool { value, span: None }))
        };
        let res_float = |value| -> Result<Rc<MetaValue>> {
            Ok(Rc::new(MetaValue::Float { value, span: None }))
        };

        match op_kind {
            BinaryOpKind::Equal => res_bool(lhs == rhs),
            BinaryOpKind::NotEqual => res_bool(lhs != rhs),
            BinaryOpKind::LessThan => res_bool(lhs < rhs),
            BinaryOpKind::LessThanOrEqual => res_bool(lhs <= rhs),
            BinaryOpKind::GreaterThan => res_bool(lhs > rhs),
            BinaryOpKind::GreaterThanOrEqual => res_bool(lhs >= rhs),

            BinaryOpKind::Add => res_float(lhs + rhs),
            BinaryOpKind::Sub => res_float(lhs - rhs),
            BinaryOpKind::Mul => res_float(lhs * rhs),
            BinaryOpKind::Div => res_float(lhs / rhs),
            BinaryOpKind::Rem => res_float(lhs % rhs),

            BinaryOpKind::RangeExclusive
            | BinaryOpKind::RangeInclusive
            | BinaryOpKind::ShiftLeft
            | BinaryOpKind::ShiftRight
            | BinaryOpKind::BinaryAnd
            | BinaryOpKind::BinaryOr
            | BinaryOpKind::BinaryXor
            | BinaryOpKind::LogicalAnd
            | BinaryOpKind::LogicalOr => {
                self.error(
                    span,
                    format!(
                        "{} `{}` is not supported on floats",
                        op_kind.to_str(),
                        op_kind.symbol(),
                    ),
                );
                Err(EvalError::Error)
            }

            BinaryOpKind::Assign
            | BinaryOpKind::AddAssign
            | BinaryOpKind::SubAssign
            | BinaryOpKind::MulAssign
            | BinaryOpKind::DivAssign
            | BinaryOpKind::RemAssign
            | BinaryOpKind::BinaryAndAssign
            | BinaryOpKind::BinaryOrAssign
            | BinaryOpKind::BinaryXorAssign
            | BinaryOpKind::ShiftLeftAssign
            | BinaryOpKind::ShiftRightAssign => unreachable!(),
        }
    }

    fn assign_to_expr(
        &mut self,
        op_base_version: Option<BinaryOpKind>,
        span: Span,
        lhs: &MetaExpr,
        rhs: &MetaExpr,
    ) -> Result<Rc<MetaValue>> {
        match lhs {
            MetaExpr::Parenthesized { expr, span: _ } => {
                self.assign_to_expr(op_base_version, span, expr, rhs)
            }
            MetaExpr::Ident { span: _, name } => {
                for scope_idx in 0..self.scopes.len() {
                    if let Some(binding) =
                        self.scopes[scope_idx].bindings.get(name)
                    {
                        if !binding.mutable {
                            self.error(
                                span,
                                format!("cannot assign to immutable variable `{name}`"),
                            );
                            return Err(EvalError::Error);
                        }
                        let binding_value = binding.value.clone();

                        let rhs_val = self.eval(rhs)?;
                        let new_value = if let Some(base) = op_base_version {
                            self.eval_binary_op_from_vals(
                                base,
                                span,
                                &binding_value,
                                &rhs_val,
                            )?
                        } else {
                            rhs_val
                        };

                        self.scopes[scope_idx]
                            .bindings
                            .get_mut(name)
                            .unwrap()
                            .value = new_value;
                        return Ok(self.empty_token_list.clone());
                    }
                }
                self.error(
                    span,
                    format!("cannot find `{name}` in this scope"),
                );
                Err(EvalError::Error)
            }
            MetaExpr::Literal { .. }
            | MetaExpr::LetBinding { .. }
            | MetaExpr::Call { .. }
            | MetaExpr::FnDecl(..)
            | MetaExpr::Lambda(..)
            | MetaExpr::RawOutputGroup { .. }
            | MetaExpr::IfExpr { .. }
            | MetaExpr::For { .. }
            | MetaExpr::Loop { .. }
            | MetaExpr::While { .. }
            | MetaExpr::WhileLet { .. }
            | MetaExpr::ExpandPattern(..)
            | MetaExpr::Scope { .. }
            | MetaExpr::List { .. }
            | MetaExpr::Tuple { .. }
            | MetaExpr::Break { .. }
            | MetaExpr::Continue { .. }
            | MetaExpr::OpUnary { .. }
            | MetaExpr::OpBinary { .. } => {
                self.error(
                    lhs.span(),
                    format!("{} is not assignable", lhs.kind_str()),
                );
                Err(EvalError::Error)
            }
            MetaExpr::ListAccess { span, list, index } => {
                let (idx, list) = self.access_list(span, list, index)?;
                let MetaValue::List(list) = &*list else {
                    unreachable!();
                };
                let mut list = list.borrow_mut();
                let v = &mut list[idx];
                let rhs_val = self.eval(rhs)?;
                if let Some(base) = op_base_version {
                    *v = self
                        .eval_binary_op_from_vals(base, *span, v, &rhs_val)?;
                } else {
                    *v = rhs_val;
                }
                Ok(self.empty_token_list.clone())
            }
        }
    }

    fn eval_op_binary(
        &mut self,
        op_kind: BinaryOpKind,
        span: Span,
        lhs: &MetaExpr,
        rhs: &MetaExpr,
    ) -> Result<Rc<MetaValue>> {
        if let Some(base) = op_kind.non_assigning_version() {
            return self.assign_to_expr(Some(base), span, lhs, rhs);
        }
        if op_kind == BinaryOpKind::Assign {
            return self.assign_to_expr(None, span, lhs, rhs);
        }

        let lhs_val = self.eval(lhs)?;
        let rhs_val = self.eval(rhs)?;

        self.eval_binary_op_from_vals(op_kind, span, &lhs_val, &rhs_val)
    }

    fn eval_binary_op_from_vals(
        &mut self,
        op_kind: BinaryOpKind,
        op_span: Span,
        lhs: &MetaValue,
        rhs: &MetaValue,
    ) -> Result<Rc<MetaValue>> {
        match (lhs, rhs) {
            (
                MetaValue::Int { value: lhs, .. },
                MetaValue::Int { value: rhs, .. },
            ) => self.eval_binary_op_int(op_span, op_kind, *lhs, *rhs),
            (
                MetaValue::Float { value: lhs, .. },
                MetaValue::Float { value: rhs, .. },
            ) => self.eval_binary_op_float(op_span, op_kind, *lhs, *rhs),
            (
                MetaValue::String { value: lhs, .. },
                MetaValue::String { value: rhs, .. },
            ) if op_kind == BinaryOpKind::Add => {
                let mut res = lhs.to_string();
                res.push_str(rhs);
                Ok(Rc::new(MetaValue::String {
                    value: res.into(),
                    span: None,
                }))
            }
            _ => {
                let lhs_kind = lhs.kind();
                let rhs_kind = rhs.kind();
                if lhs_kind != rhs_kind {
                    self.error(
                        op_span,
                        format!(
                            "operands for `{}` differ in type: `{}` {} `{}`",
                            op_kind.to_str(),
                            lhs_kind,
                            op_kind.symbol(),
                            rhs_kind
                        ),
                    );
                } else {
                    self.error(
                        op_span,
                        format!(
                            "invalid operand types for `{}`: `{}` {} `{}`",
                            op_kind.to_str(),
                            lhs_kind,
                            op_kind.symbol(),
                            rhs_kind
                        ),
                    );
                }
                Err(EvalError::Error)
            }
        }
    }

    fn eval_fn_call(
        &mut self,
        span: Span,
        lhs: &MetaExpr,
        arg_exprs: &Vec<Rc<MetaExpr>>,
    ) -> Result<Rc<MetaValue>> {
        let lhs = if let MetaExpr::Ident { span, name } = lhs {
            // We could just evaluate but we want a better error message
            // than "`token` is not callable".
            if let Some(val) = self.lookup(name, false) {
                val
            } else {
                self.error(*span, format!("undefined function `{name}`"));
                return Err(EvalError::Error);
            }
        } else {
            self.eval(lhs)?
        };

        let Some(callable) = Callable::from_value(&lhs) else {
            self.error(
                span,
                format!("value of type `{}` is not callable", lhs.kind()),
            );
            return Err(EvalError::Error);
        };

        let mut args = Vec::new();
        for arg in arg_exprs {
            let Ok(val) = self.eval(arg) else {
                return Err(EvalError::Error);
            };
            args.push(val);
        }

        callable.call(self, span, &args)
    }

    fn insert_binding(
        &mut self,
        span: Span,
        name: Rc<str>,
        mutable: bool,
        super_bound: bool,
        value: Rc<MetaValue>,
    ) {
        let binding = Binding {
            span,
            mutable,
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
