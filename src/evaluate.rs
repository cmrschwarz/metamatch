use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    rc::Rc,
};

use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream,
    TokenTree,
};

use super::{
    ast::{
        BinaryOpKind, Binding, BuiltinFn, Context, EvalError, ExprBlock,
        ExternDecl, Function, Lambda, MetaExpr, MetaIdent, MetaValue, Pattern,
        Scope, ScopeKind, UnaryOpKind, UsePath, UseTree, Visibility,
    },
    token_sink::TokenSink,
};

pub type Result<T> = std::result::Result<T, EvalError>;

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
impl<'b> IntoIterator for &'b Iterable<'_> {
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
        let res = match self {
            Callable::Fn(function) => {
                if function.params.len() != args.len() {
                    ctx.error(
                        span,
                        format!(
                        "function `{}` expects {} parameter{}, called with {}",
                        function.ident.name,
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
                let res =
                    ctx.eval_block_list_to_meta_val(span, &function.body);
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
                let res = ctx.eval(&lambda.body, true);
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
        };
        if let Err(EvalError::Return { span: _, value }) = res {
            return Ok(value.unwrap_or_else(|| ctx.empty_token_list.clone()));
        };
        res
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
            Some(Rc::from(value.encode_utf8(&mut data).to_string()))
        }
        MetaValue::Token(t) => match t {
            TokenTree::Ident(i) => Some(Rc::from(i.to_string())),
            TokenTree::Literal(literal) => Some(Rc::from(literal.to_string())),
            TokenTree::Group(_) => None, // TODO:?
            TokenTree::Punct(punct) => Some(Rc::from(
                punct.as_char().encode_utf8(&mut [0; 4]).to_string(),
            )),
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

pub fn string_to_ident(s: &str, raw: bool, span: Span) -> Option<Ident> {
    std::panic::catch_unwind(|| {
        if raw {
            Ident::new_raw(s, span)
        } else {
            Ident::new(s, span)
        }
    })
    .ok()
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

    let Some(ident) = string_to_ident(&name, false, span) else {
        ctx.error(
            callsite,
            format!("cast failed: invalid identifier `{name}`"),
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

fn append_group(
    tgt: &mut TokenSink,
    delim: Delimiter,
    span: Span,
    map_inner: impl FnOnce(&mut TokenSink) -> Result<()>,
) -> Result<()> {
    tgt.append_group(delim, span, map_inner)
}

fn append_pattern(tgt: &mut TokenSink, pat: &Pattern) -> Result<()> {
    match pat {
        Pattern::Ident(binding) => {
            if binding.super_bound {
                append_ident_str(tgt, "super", Span::call_site());
            }
            if binding.mutable {
                append_ident_str(tgt, "mut", Span::call_site());
            }
            append_ident_str_dyn(tgt, &binding.ident);

            Ok(())
        }
        Pattern::Tuple { span, elems } => append_comma_separated_list(
            tgt,
            Delimiter::Parenthesis,
            *span,
            elems,
            append_pattern,
        ),
        Pattern::List { span, elems } => append_comma_separated_list(
            tgt,
            Delimiter::Bracket,
            *span,
            elems,
            append_pattern,
        ),
    }
}

fn append_visibility(tgt: &mut TokenSink, visibility: Visibility, span: Span) {
    match visibility {
        Visibility::Extern => {
            append_ident_str(tgt, "extern", span);
        }
        Visibility::PubExtern => {
            append_ident_str(tgt, "pub", span);
            append_ident_str(tgt, "extern", span);
        }
        Visibility::Regular => (),
    }
}

fn append_quoted_use_tree_binding(tgt: &mut TokenSink, tree: &UseTree) {
    match tree {
        UseTree::Path {
            replacement: rep, ..
        } => {
            append_ident_str_dyn(
                tgt,
                &MetaIdent {
                    name: rep.binding.clone(),
                    raw: false,
                    span: rep.ident.span,
                },
            );
        }
        UseTree::Group { span, items, .. } => {
            _ = append_comma_separated_list(
                tgt,
                Delimiter::Parenthesis,
                *span,
                items,
                |tgt, item| {
                    append_quoted_use_tree_binding(tgt, item);
                    Ok(())
                },
            );
        }
        UseTree::Rename {
            replacement: rep, ..
        } => {
            append_ident_str_dyn(
                tgt,
                &MetaIdent {
                    name: rep.binding.clone(),
                    raw: false,
                    span: rep.ident.span,
                },
            );
        }
    }
}

fn append_comma_separated_list<T>(
    tgt: &mut TokenSink,
    delim: Delimiter,
    span: Span,
    elements: impl IntoIterator<Item = T>,
    mut map_inner: impl FnMut(&mut TokenSink, T) -> Result<()>,
) -> Result<()> {
    tgt.append_group(delim, span, |tgt| {
        for (i, e) in elements.into_iter().enumerate() {
            if i > 0 {
                tgt.push(TokenTree::Punct(Punct::new(',', Spacing::Alone)));
            }
            map_inner(tgt, e)?;
        }
        Ok(())
    })
}

fn append_lambda_params(
    tgt: &mut TokenSink,
    params: &[Pattern],
) -> Result<()> {
    tgt.push(TokenTree::Punct(Punct::new('|', Spacing::Alone)));
    for (i, p) in params.iter().enumerate() {
        if i > 0 {
            tgt.push(TokenTree::Punct(Punct::new(',', Spacing::Alone)));
        }
        append_pattern(tgt, p)?;
    }
    tgt.push(TokenTree::Punct(Punct::new('|', Spacing::Alone)));
    Ok(())
}

pub fn append_ident_str(tgt: &mut TokenSink, name: &'static str, span: Span) {
    tgt.push(TokenTree::Ident(
        string_to_ident(name, false, span).unwrap(),
    ));
}

fn append_ident_str_dyn(tgt: &mut TokenSink, ident: &MetaIdent) {
    tgt.push(TokenTree::Ident(ident.to_ident()));
}

fn append_use_path(tgt: &mut TokenSink, path: &UsePath) {
    for (i, seg) in path.segments.iter().enumerate() {
        if i > 0 || path.leading_double_colon {
            append_double_colon(tgt, seg.span);
        }
        tgt.push(TokenTree::Ident(seg.to_ident()));
    }
}

pub fn append_double_colon(tgt: &mut TokenSink, span: Span) {
    append_punct(tgt, ':', Spacing::Joint, span);
    append_punct(tgt, ':', Spacing::Alone, span);
}
pub fn append_punct(
    tgt: &mut TokenSink,
    ch: char,
    spacing: Spacing,
    span: Span,
) {
    let mut p = Punct::new(ch, spacing);
    p.set_span(span);
    tgt.push(TokenTree::Punct(p));
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
                    bind.ident.clone(),
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
            MetaIdent {
                name,
                raw: false,
                span: Span::call_site(),
            },
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

    fn append_quoted_stmt_list(
        &mut self,
        tgt: &mut TokenSink,
        stmts: &[Rc<MetaExpr>],
        trailing_semi: bool,
    ) -> Result<()> {
        for (i, stmt) in stmts.iter().enumerate() {
            if i > 0 {
                tgt.push(TokenTree::Punct(Punct::new(';', Spacing::Alone)));
            }
            self.append_quoted_expression(
                tgt,
                stmt,
                !trailing_semi && i + 1 == stmts.len(),
            )?;
        }

        if trailing_semi {
            tgt.push(TokenTree::Punct(Punct::new(';', Spacing::Alone)));
        }
        Ok(())
    }

    fn append_quoted_block(
        &mut self,
        tgt: &mut TokenSink,
        span: Span,
        block: &ExprBlock,
        surround_with_eval_scope: bool,
    ) -> Result<()> {
        if surround_with_eval_scope {
            self.push_dummy_scope(ScopeKind::Eval);
        }

        let res = tgt.append_group(Delimiter::Brace, span, |tgt| {
            self.append_quoted_stmt_list(
                tgt,
                &block.stmts,
                block.trailing_semi,
            )
        });

        if surround_with_eval_scope {
            self.pop_scope();
        }

        res
    }

    fn append_quoted_use_tree_pattern(
        &mut self,
        tgt: &mut TokenSink,
        tree: &UseTree,
    ) {
        match tree {
            UseTree::Path { replacement, .. } => {
                self.insert_dummy_binding(&replacement.ident, false);
                append_ident_str_dyn(tgt, &replacement.ident);
            }
            UseTree::Group { span, items, .. } => {
                _ = append_comma_separated_list(
                    tgt,
                    Delimiter::Parenthesis,
                    *span,
                    items,
                    |tgt, item| {
                        self.append_quoted_use_tree_pattern(tgt, item);
                        Ok(())
                    },
                );
            }
            UseTree::Rename { replacement, .. } => {
                self.insert_dummy_binding(&replacement.ident, false);
                append_ident_str_dyn(tgt, &replacement.ident);
            }
        }
    }

    fn append_quoted_fn_decl(
        &mut self,
        tgt: &mut TokenSink,
        function: &Function,
        suppress_vis: bool,
    ) -> Result<()> {
        if !suppress_vis {
            append_visibility(tgt, function.visibility, function.ident.span);
        }
        append_ident_str(tgt, "fn", function.ident.span);
        append_ident_str_dyn(tgt, &function.ident);
        append_comma_separated_list(
            tgt,
            Delimiter::Parenthesis,
            function.ident.span,
            &function.params,
            append_pattern,
        )?;
        self.push_dummy_scope(ScopeKind::Lambda);
        for pat in &function.params {
            self.insert_dummy_bindings_for_pattern(pat);
        }
        self.append_quoted_block(
            tgt,
            function.ident.span,
            &function.body,
            false,
        )?;
        self.pop_scope();
        Ok(())
    }

    fn append_quoted_expression(
        &mut self,
        tgt: &mut TokenSink,
        expr: &MetaExpr,
        is_expression_context: bool,
    ) -> Result<()> {
        match expr {
            MetaExpr::Break { span, expr } => {
                append_ident_str(tgt, "break", *span);
                if let Some(expr) = expr {
                    self.append_quoted_expression(tgt, expr, true)?;
                }
            }
            MetaExpr::Return { span, expr } => {
                append_ident_str(tgt, "return", *span);
                if let Some(expr) = expr {
                    self.append_quoted_expression(tgt, expr, true)?;
                }
            }
            MetaExpr::Continue { span } => {
                append_ident_str(tgt, "continue", *span);
            }
            MetaExpr::Literal {
                span,
                value,
                from_raw_block: _,
            } => {
                if tgt.quote_for_rust {
                    return self.append_value_to_stream(tgt, *span, value);
                }

                tgt.append_raw(|tgt| {
                    let q4r = tgt.quote_for_rust;
                    tgt.quote_for_rust = false;
                    self.append_value_to_stream(tgt, *span, value)?;
                    tgt.quote_for_rust = q4r;
                    Ok(())
                })?;
                // make sure this gets outputed even if it is an empty raw!{}
                tgt.force_raw_flush = true;
            }
            MetaExpr::Ident(ident) => {
                if self.errors.is_empty() && self.extern_uses.is_empty() {
                    if let Some(val) =
                        self.lookup_as_external_identifier(&ident.name)
                    {
                        return self
                            .append_value_to_stream(tgt, ident.span, &val);
                    }
                }
                append_ident_str_dyn(tgt, ident);
            }
            MetaExpr::LetBinding {
                visibility,
                span,
                pattern,
                expr,
            } => {
                // we need this to allow function quotes to
                // know which identifiers are bound to external vars
                self.insert_dummy_bindings_for_pattern(pattern);
                append_visibility(tgt, *visibility, *span);
                append_ident_str(tgt, "let", *span);
                append_pattern(tgt, pattern)?;

                if let Some(expr) = expr {
                    // TODO: should we store a span for this?
                    tgt.push(TokenTree::Punct(Punct::new(
                        '=',
                        Spacing::Alone,
                    )));

                    self.append_quoted_expression(tgt, expr, true)?;
                }
            }
            MetaExpr::Call { span, lhs, args } => {
                self.append_quoted_expression(tgt, lhs, true)?;
                append_comma_separated_list(
                    tgt,
                    Delimiter::Parenthesis,
                    *span,
                    args,
                    |tgt, e| self.append_quoted_expression(tgt, e, true),
                )?;
            }
            MetaExpr::FnDecl(function) => {
                self.append_quoted_fn_decl(tgt, function, false)?;
            }
            MetaExpr::Lambda(lambda) => {
                append_lambda_params(tgt, &lambda.params)?;
                self.append_quoted_expression(tgt, &lambda.body, true)?;
            }
            MetaExpr::UseDecl(use_decl) => {
                if is_expression_context {
                    append_quoted_use_tree_binding(tgt, &use_decl.tree)
                } else {
                    append_ident_str(tgt, "let", use_decl.span);

                    self.append_quoted_use_tree_pattern(tgt, &use_decl.tree);

                    append_punct(tgt, '=', Spacing::Alone, use_decl.span);

                    append_quoted_use_tree_binding(tgt, &use_decl.tree);
                }
            }
            MetaExpr::RawOutputGroup {
                span,
                delimiter,
                contents,
            } => {
                if tgt.quote_for_rust {
                    append_group(tgt, *delimiter, *span, |inner| {
                        for e in contents {
                            self.append_quoted_expression(inner, e, true)?;
                        }
                        Ok(())
                    })?;
                } else {
                    append_ident_str(tgt, "group", *span);
                    append_punct(tgt, '!', Spacing::Alone, *span);
                    append_group(tgt, *delimiter, *span, |tgt| {
                        self.append_quoted_stmt_list(tgt, contents, true)
                    })?;
                }
            }
            MetaExpr::Group {
                span,
                delimiter,
                body,
            } => {
                append_ident_str(tgt, "group", *span);
                append_punct(tgt, '!', Spacing::Alone, *span);
                append_group(tgt, *delimiter, *span, |tgt| {
                    self.append_quoted_stmt_list(
                        tgt,
                        &body.stmts,
                        body.trailing_semi,
                    )
                })?;
            }
            MetaExpr::IfExpr {
                span,
                condition,
                body,
                else_expr,
            } => {
                append_ident_str(tgt, "if", *span);
                self.append_quoted_expression(tgt, condition, true)?;
                self.append_quoted_block(tgt, *span, body, true)?;
                if let Some(else_expr) = else_expr {
                    append_ident_str(tgt, "else", else_expr.span());
                    self.append_quoted_expression(tgt, else_expr, true)?;
                }
            }
            MetaExpr::For {
                span,
                pattern,
                variants_expr,
                body,
            } => {
                append_ident_str(tgt, "for", *span);
                append_pattern(tgt, pattern)?;
                append_ident_str(tgt, "in", *span);
                self.append_quoted_expression(tgt, variants_expr, true)?;
                self.push_dummy_scope(ScopeKind::Eval);
                self.insert_dummy_bindings_for_pattern(pattern);
                self.append_quoted_block(tgt, *span, body, false)?;
                self.pop_scope();
            }
            MetaExpr::Loop { span, body } => {
                append_ident_str(tgt, "loop", *span);
                self.append_quoted_block(tgt, *span, body, true)?;
            }
            MetaExpr::While {
                condition,
                span,
                body,
            } => {
                append_ident_str(tgt, "while", *span);
                self.append_quoted_expression(tgt, condition, true)?;
                self.append_quoted_block(tgt, *span, body, true)?;
            }
            MetaExpr::WhileLet {
                pattern,
                expr,
                span,
                body,
            } => {
                append_ident_str(tgt, "while", *span);
                append_ident_str(tgt, "let", *span);
                append_pattern(tgt, pattern)?;
                tgt.push(TokenTree::Punct(Punct::new('=', Spacing::Alone)));
                self.append_quoted_expression(tgt, expr, true)?;
                self.push_dummy_scope(ScopeKind::Eval);
                self.insert_dummy_bindings_for_pattern(pattern);
                self.append_quoted_block(tgt, *span, body, false)?;
                self.pop_scope();
            }
            MetaExpr::Parenthesized { span, expr } => {
                append_group(tgt, Delimiter::Parenthesis, *span, |inner| {
                    self.append_quoted_expression(inner, expr, true)
                })?;
            }
            MetaExpr::ExpandPattern(expand_pattern) => {
                tgt.push(TokenTree::Punct(Punct::new('#', Spacing::Alone)));
                append_group(
                    tgt,
                    Delimiter::Bracket,
                    expand_pattern.span,
                    |tgt| {
                        append_ident_str(
                            tgt,
                            "expand_pattern",
                            expand_pattern.span,
                        );

                        append_group(
                            tgt,
                            Delimiter::Parenthesis,
                            expand_pattern.span,
                            |tgt| {
                                append_ident_str(
                                    tgt,
                                    "for",
                                    expand_pattern.span,
                                );
                                append_pattern(
                                    tgt,
                                    &expand_pattern.for_pattern,
                                )?;
                                append_ident_str(
                                    tgt,
                                    "in",
                                    expand_pattern.span,
                                );
                                self.append_quoted_expression(
                                    tgt,
                                    &expand_pattern.for_expr,
                                    true,
                                )?;
                                Ok(())
                            },
                        )?;
                        Ok(())
                    },
                )?;
                for expr in &expand_pattern.match_arm_patterns {
                    self.append_quoted_expression(tgt, expr, true)?;
                }
                for expr in &expand_pattern.match_arm_guard {
                    self.append_quoted_expression(tgt, expr, true)?;
                }
                for expr in &expand_pattern.match_arm_body {
                    self.push_dummy_scope(ScopeKind::Quote);
                    self.insert_dummy_bindings_for_pattern(
                        &expand_pattern.for_pattern,
                    );
                    self.append_quoted_expression(tgt, expr, false)?;
                    self.pop_scope();
                }
            }
            MetaExpr::Block(block) => {
                self.append_quoted_block(tgt, block.span, block, true)?;
            }
            MetaExpr::List { span, exprs } => {
                append_comma_separated_list(
                    tgt,
                    Delimiter::Bracket,
                    *span,
                    exprs,
                    |tgt, e| self.append_quoted_expression(tgt, e, true),
                )?;
            }
            MetaExpr::Tuple { span, exprs } => {
                append_comma_separated_list(
                    tgt,
                    Delimiter::Parenthesis,
                    *span,
                    exprs,
                    |tgt, e| self.append_quoted_expression(tgt, e, true),
                )?;
            }
            MetaExpr::OpUnary {
                kind,
                span,
                operand,
            } => {
                let symbol = kind.symbol();
                for (i, c) in symbol.chars().enumerate() {
                    let mut p = Punct::new(
                        c,
                        if i + 1 == symbol.len() {
                            Spacing::Alone
                        } else {
                            Spacing::Joint
                        },
                    );
                    p.set_span(*span);
                    tgt.push(TokenTree::Punct(p));
                }
                self.append_quoted_expression(tgt, operand, true)?;
            }
            MetaExpr::OpBinary {
                kind,
                span,
                lhs,
                rhs,
            } => {
                self.append_quoted_expression(tgt, lhs, true)?;
                let symbol = kind.symbol();
                for (i, c) in symbol.chars().enumerate() {
                    let mut p = Punct::new(
                        c,
                        if i + 1 == symbol.len() {
                            Spacing::Alone
                        } else {
                            Spacing::Joint
                        },
                    );
                    p.set_span(*span);
                    tgt.push(TokenTree::Punct(p));
                }
                self.append_quoted_expression(tgt, rhs, true)?;
            }
            MetaExpr::ListAccess { span, list, index } => {
                self.append_quoted_expression(tgt, list, true)?;
                append_group(tgt, Delimiter::Bracket, *span, |tgt| {
                    self.append_quoted_expression(tgt, index, true)
                })?;
            }
        }
        Ok(())
    }

    fn append_value_to_stream(
        &mut self,
        tgt: &mut TokenSink,
        eval_span: Span,
        value: &MetaValue,
    ) -> Result<()> {
        match value {
            MetaValue::Token(t) => {
                tgt.push(t.clone());
            }
            MetaValue::Tokens(list) => {
                if list.is_empty() && !tgt.quote_for_rust {
                    _ = tgt.append_raw(|_tgt| Ok(()));
                    tgt.force_raw_flush = true;
                } else {
                    tgt.extend(list.iter().cloned());
                }
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
                append_ident_str(
                    tgt,
                    if *value { "true" } else { "false" },
                    span.unwrap_or(eval_span),
                );
            }
            MetaValue::String { value, span } => {
                let mut lit = Literal::string(value);
                lit.set_span(span.unwrap_or(eval_span));
                tgt.push(TokenTree::Literal(lit));
            }
            MetaValue::Fn(f) => {
                append_lambda_params(tgt, &f.params)?;
                self.push_dummy_scope(ScopeKind::Lambda);
                for pat in &f.params {
                    self.insert_dummy_bindings_for_pattern(pat);
                }
                self.append_quoted_block(tgt, f.ident.span, &f.body, false)?;
                self.pop_scope();
            }
            MetaValue::Lambda(lambda) => {
                self.append_quoted_expression(
                    tgt,
                    &MetaExpr::Lambda(lambda.clone()),
                    true,
                )?;
            }
            MetaValue::BuiltinFn(f) => {
                append_ident_str_dyn(
                    tgt,
                    &MetaIdent {
                        name: f.name.clone(),
                        raw: false,
                        span: eval_span,
                    },
                );
            }
            MetaValue::List(vals) => {
                tgt.append_group(Delimiter::Bracket, eval_span, |tgt| {
                    for (i, e) in vals.borrow().iter().enumerate() {
                        if i > 0 {
                            tgt.push(comma_token(eval_span));
                        }
                        self.append_value_to_stream(tgt, eval_span, e)?;
                    }
                    Ok(())
                })?;
            }
            MetaValue::Tuple(vals) => {
                tgt.append_group(Delimiter::Parenthesis, eval_span, |tgt| {
                    for (i, e) in vals.iter().enumerate() {
                        if i > 0 {
                            tgt.push(comma_token(eval_span));
                        }
                        self.append_value_to_stream(tgt, eval_span, e)?;
                    }
                    if vals.len() == 1 {
                        tgt.push(comma_token(eval_span));
                    }
                    Ok(())
                })?;
            }
        }
        Ok(())
    }

    pub fn lookup_as_external_identifier(
        &self,
        name: &str,
    ) -> Option<Rc<MetaValue>> {
        let mut outside = false;
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.bindings.get(name) {
                if !outside {
                    return None;
                }
                return Some(binding.value.clone());
            }
            outside |= scope.kind == ScopeKind::Lambda;
        }
        None
    }

    pub fn lookup(
        &self,
        name: &str,
        super_only: bool,
    ) -> Option<Rc<MetaValue>> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.bindings.get(name) {
                if binding.super_bound || !super_only {
                    return Some(binding.value.clone());
                }
            }
        }
        None
    }

    fn expand_extern_decl(
        &mut self,
        tgt: &mut TokenSink,
        vis: Visibility,
        ident: &MetaIdent,
        decl_span: Span,
        mut append_value: impl FnMut(&mut Self, &mut TokenSink) -> Result<()>,
    ) -> Result<()> {
        if vis.is_pub() {
            append_punct(tgt, '#', Spacing::Alone, decl_span);
            append_group(tgt, Delimiter::Bracket, decl_span, |tgt| {
                append_ident_str(tgt, "macro_export", decl_span);
                Ok(())
            })?;
        }

        append_ident_str(tgt, "macro_rules", decl_span);
        append_punct(tgt, '!', Spacing::Alone, decl_span);
        append_ident_str_dyn(tgt, ident);

        // ($alias: ident, ($($chain: tt)*), [ $($prefix:tt)* ] $($rest: tt)* )
        append_group(tgt, Delimiter::Brace, decl_span, |tgt| {
            append_group(tgt, Delimiter::Parenthesis, decl_span, |tgt| {
                // $alias: ident,
                append_punct(tgt, '$', Spacing::Alone, decl_span);
                append_ident_str(tgt, "alias", decl_span);
                append_punct(tgt, ':', Spacing::Alone, decl_span);
                append_ident_str(tgt, "ident", decl_span);
                append_punct(tgt, ',', Spacing::Alone, decl_span);

                // ($($chain: tt)*),
                append_group(tgt, Delimiter::Parenthesis, decl_span, |tgt| {
                    append_punct(tgt, '$', Spacing::Alone, decl_span);
                    append_group(
                        tgt,
                        Delimiter::Parenthesis,
                        decl_span,
                        |tgt| {
                            append_punct(tgt, '$', Spacing::Alone, decl_span);
                            append_ident_str(tgt, "chain", decl_span);
                            append_punct(tgt, ':', Spacing::Alone, decl_span);
                            append_ident_str(tgt, "tt", decl_span);
                            Ok(())
                        },
                    )?;
                    append_punct(tgt, '*', Spacing::Alone, decl_span);
                    Ok(())
                })?;
                append_punct(tgt, ',', Spacing::Alone, decl_span);

                // [ $($prefix:tt)* ],
                append_group(tgt, Delimiter::Bracket, decl_span, |tgt| {
                    append_punct(tgt, '$', Spacing::Alone, decl_span);
                    append_group(
                        tgt,
                        Delimiter::Parenthesis,
                        decl_span,
                        |tgt| {
                            append_punct(tgt, '$', Spacing::Alone, decl_span);
                            append_ident_str(tgt, "prefix", decl_span);
                            append_punct(tgt, ':', Spacing::Alone, decl_span);
                            append_ident_str(tgt, "tt", decl_span);
                            Ok(())
                        },
                    )?;
                    append_punct(tgt, '*', Spacing::Alone, decl_span);
                    Ok(())
                })?;
                append_punct(tgt, ',', Spacing::Alone, decl_span);

                // $($rest: tt)*
                append_punct(tgt, '$', Spacing::Alone, decl_span);
                append_group(tgt, Delimiter::Parenthesis, decl_span, |tgt| {
                    append_punct(tgt, '$', Spacing::Alone, decl_span);
                    append_ident_str(tgt, "rest", decl_span);
                    append_punct(tgt, ':', Spacing::Alone, decl_span);
                    append_ident_str(tgt, "tt", decl_span);
                    Ok(())
                })?;
                append_punct(tgt, '*', Spacing::Alone, decl_span);

                Ok(())
            })?;
            append_punct(tgt, '=', Spacing::Joint, decl_span);
            append_punct(tgt, '>', Spacing::Alone, decl_span);
            append_group(tgt, Delimiter::Brace, decl_span, |tgt| {
                // $($chain)*
                append_punct(tgt, '$', Spacing::Alone, decl_span);
                append_group(tgt, Delimiter::Parenthesis, decl_span, |tgt| {
                    append_punct(tgt, '$', Spacing::Alone, decl_span);
                    append_ident_str(tgt, "chain", decl_span);
                    Ok(())
                })?;
                append_punct(tgt, '*', Spacing::Alone, decl_span);

                append_group(tgt, Delimiter::Brace, decl_span, |tgt| {
                    // $($prefix)*
                    append_punct(tgt, '$', Spacing::Alone, decl_span);
                    append_group(
                        tgt,
                        Delimiter::Parenthesis,
                        decl_span,
                        |tgt| {
                            append_punct(tgt, '$', Spacing::Alone, decl_span);
                            append_ident_str(tgt, "prefix", decl_span);
                            Ok(())
                        },
                    )?;
                    append_punct(tgt, '*', Spacing::Alone, decl_span);

                    // let super $alias = ..;
                    append_ident_str(tgt, "let", decl_span);
                    append_punct(tgt, '$', Spacing::Alone, decl_span);
                    append_ident_str(tgt, "alias", decl_span);
                    append_punct(tgt, '=', Spacing::Alone, decl_span);
                    append_value(self, tgt)?;
                    append_punct(tgt, ';', Spacing::Alone, decl_span);

                    // $($rest)*
                    append_punct(tgt, '$', Spacing::Alone, decl_span);
                    append_group(
                        tgt,
                        Delimiter::Parenthesis,
                        decl_span,
                        |tgt| {
                            append_punct(tgt, '$', Spacing::Alone, decl_span);
                            append_ident_str(tgt, "rest", decl_span);
                            Ok(())
                        },
                    )?;
                    append_punct(tgt, '*', Spacing::Alone, decl_span);
                    Ok(())
                })
            })
        })
    }

    fn expand_extern_decls(&mut self, tgt: &mut TokenSink) -> Result<()> {
        for decl in std::mem::take(&mut self.extern_decls).into_iter() {
            match decl {
                ExternDecl::Let {
                    span,
                    visibility,
                    bindings,
                    values,
                } => {
                    self.push_eval_scope();
                    self.match_and_bind_pattern(&bindings, values, false)?;
                    let scope = self.pop_scope().unwrap();
                    for (_name, binding) in scope.bindings {
                        // Store the extern binding permanently for use
                        // declarations
                        self.insert_binding(
                            binding.ident.clone(),
                            false,
                            false,
                            binding.value.clone(),
                        );

                        self.expand_extern_decl(
                            tgt,
                            visibility,
                            &binding.ident,
                            span,
                            |this, tgt| {
                                let q4r = tgt.quote_for_rust;
                                tgt.quote_for_rust = false;
                                this.append_value_to_stream(
                                    tgt,
                                    span,
                                    &binding.value,
                                )?;
                                tgt.quote_for_rust = q4r;
                                Ok(())
                            },
                        )?;
                    }
                }
                ExternDecl::Fn { decl, mut quoted } => {
                    self.expand_extern_decl(
                        tgt,
                        decl.visibility,
                        &decl.ident,
                        decl.ident.span, // is this fine?
                        |_this, tgt| {
                            tgt.extend(std::mem::take(&mut quoted));
                            Ok(())
                        },
                    )?;
                }
            };
        }
        Ok(())
    }

    pub fn expand_extern_uses(
        &mut self,
        tgt: &mut TokenSink,
        block: &ExprBlock,
    ) {
        let mut macro_path = TokenSink::new(true);
        let mut rep_ident_prev = None;
        let mut chain_target = TokenSink::new(true);
        let mut prefix = TokenSink::new(true);

        for rep in &self.extern_uses {
            let mut prev_prefix = prefix.take();
            _ = append_group(
                &mut prefix,
                Delimiter::Bracket,
                rep.ident.span,
                |inner| {
                    if let Some(rep_prev) = rep_ident_prev {
                        append_ident_str_dyn(inner, &rep_prev);
                        append_punct(
                            inner,
                            ',',
                            Spacing::Alone,
                            rep.ident.span,
                        );

                        append_group(
                            inner,
                            Delimiter::Parenthesis,
                            rep.ident.span,
                            |tgt| {
                                tgt.extend(chain_target.take());
                                Ok(())
                            },
                        )?;

                        append_punct(
                            inner,
                            ',',
                            Spacing::Alone,
                            rep.ident.span,
                        );
                        chain_target = macro_path.take_sink();
                        inner.extend(prev_prefix.drain(..));
                        append_punct(
                            inner,
                            ',',
                            Spacing::Alone,
                            rep.ident.span,
                        );
                    } else {
                        debug_assert!(chain_target.is_empty());
                        append_double_colon(&mut chain_target, rep.ident.span);
                        append_ident_str(
                            &mut chain_target,
                            "metamatch",
                            Span::call_site(),
                        );
                        append_double_colon(&mut chain_target, rep.ident.span);
                        append_ident_str(
                            &mut chain_target,
                            "eval",
                            Span::call_site(),
                        );
                        append_punct(
                            &mut chain_target,
                            '!',
                            Spacing::Alone,
                            Span::call_site(),
                        );
                    }
                    Ok(())
                },
            );
            rep_ident_prev = Some(MetaIdent {
                name: rep.binding.clone(),
                raw: false,
                span: rep.ident.span,
            });
            append_use_path(&mut macro_path, &rep.target_path);
            append_punct(&mut macro_path, '!', Spacing::Alone, rep.ident.span);
        }

        // we only expand uses if there's at least one
        let last_rep = rep_ident_prev.unwrap();

        tgt.extend(macro_path.into_vec());

        _ = append_group(tgt, Delimiter::Brace, last_rep.span, |tgt| {
            append_ident_str_dyn(tgt, &last_rep);
            append_punct(tgt, ',', Spacing::Alone, last_rep.span);

            append_group(tgt, Delimiter::Parenthesis, last_rep.span, |tgt| {
                tgt.extend_from_slice(&chain_target.into_vec());
                Ok(())
            })?;

            append_punct(tgt, ',', Spacing::Alone, last_rep.span);
            tgt.extend(prefix.take());
            append_punct(tgt, ',', Spacing::Alone, last_rep.span);
            let qfr = tgt.quote_for_rust;
            tgt.quote_for_rust = false;
            self.append_quoted_stmt_list(
                tgt,
                &block.stmts,
                block.trailing_semi,
            )?;
            tgt.quote_for_rust = qfr;
            Ok(())
        });
    }

    pub fn eval_to_token_stream(
        &mut self,
        eval_span: Span,
        block: &ExprBlock,
    ) -> TokenStream {
        debug_assert_eq!(self.scopes.len(), 1, "scopes");

        if !self.errors.is_empty() {
            return self.expand_errors();
        }

        self.push_eval_scope();
        let mut res_stream = TokenSink::new(true);

        if !self.extern_uses.is_empty() {
            self.expand_extern_uses(&mut res_stream, block);
            if !self.errors.is_empty() {
                return self.expand_errors();
            }
            return TokenStream::from_iter(res_stream.into_vec());
        }

        _ = self.eval_stmt_list_to_stream(
            &mut res_stream,
            eval_span,
            &block.stmts,
            block.trailing_semi,
        );

        if !self.extern_decls.is_empty() {
            let mut extern_decls = TokenSink::new(false);
            _ = self.expand_extern_decls(&mut extern_decls);
            extern_decls.extend(res_stream.into_vec());
            res_stream = extern_decls;
        }

        self.pop_scope();

        if !self.errors.is_empty() {
            return self.expand_errors();
        }

        TokenStream::from_iter(res_stream.into_vec())
    }

    fn eval_stmt_list_to_stream(
        &mut self,
        tgt: &mut TokenSink,
        eval_span: Span,
        stmts: &[Rc<MetaExpr>],
        trailing_semi: bool,
    ) -> Result<()> {
        for (i, expr) in stmts.iter().enumerate() {
            let v = self.eval(expr, !trailing_semi && i + 1 == stmts.len())?;
            self.append_value_to_stream(tgt, eval_span, &v)?;
        }
        Ok(())
    }

    fn eval_block_to_stream(
        &mut self,
        tgt: &mut TokenSink,
        eval_span: Span,
        block: &ExprBlock,
    ) -> Result<()> {
        self.eval_stmt_list_to_stream(
            tgt,
            eval_span,
            &block.stmts,
            block.trailing_semi,
        )
    }

    fn eval_block_list_to_meta_val(
        &mut self,
        eval_span: Span,
        block: &ExprBlock,
    ) -> Result<Rc<MetaValue>> {
        let mut res = TokenSink::new(true);
        let mut last_expr = self.empty_token_list.clone();
        for (i, expr) in block.stmts.iter().enumerate() {
            let is_last = i == block.stmts.len() - 1;
            let v = self.eval(expr, is_last)?;
            self.append_value_to_stream(&mut res, eval_span, &last_expr)?;
            last_expr = v;
        }
        if res.is_empty() {
            return Ok(last_expr);
        }
        self.append_value_to_stream(&mut res, eval_span, &last_expr)?;
        Ok(Rc::new(MetaValue::Tokens(res.into_vec())))
    }

    fn push_eval_scope(&mut self) {
        self.scopes.push(Scope {
            kind: ScopeKind::Evaluation,
            bindings: HashMap::new(),
        });
    }

    fn eval(
        &mut self,
        expr: &MetaExpr,
        is_expression_context: bool,
    ) -> Result<Rc<MetaValue>> {
        match expr {
            MetaExpr::Break { span, expr } => Err(EvalError::Break {
                value: expr
                    .as_ref()
                    .map(|x| self.eval(x, true))
                    .transpose()?,
                span: *span,
            }),
            MetaExpr::Return { span, expr } => Err(EvalError::Return {
                value: expr
                    .as_ref()
                    .map(|x| self.eval(x, true))
                    .transpose()?,
                span: *span,
            }),
            MetaExpr::Continue { .. } => Err(EvalError::Continue),
            MetaExpr::Parenthesized { span: _, expr } => {
                self.eval(expr, is_expression_context)
            }
            MetaExpr::Literal {
                span: _,
                value,
                from_raw_block: _,
            } => Ok(value.clone()),
            MetaExpr::Ident(ident) => {
                if let Some(expr) = self.lookup(&ident.name, false) {
                    return Ok(expr);
                }
                Ok(Rc::new(MetaValue::Token(TokenTree::Ident(
                    ident.to_ident(),
                ))))
            }
            MetaExpr::LetBinding {
                visibility,
                span,
                pattern,
                expr,
            } => {
                let val = self.eval(expr.as_ref().unwrap(), true)?;
                if visibility.is_extern() {
                    self.extern_decls.push(ExternDecl::Let {
                        span: *span,
                        visibility: *visibility,
                        bindings: pattern.clone(),
                        values: val.clone(),
                    });
                }
                self.match_and_bind_pattern(pattern, val, false)?;
                Ok(self.empty_token_list.clone())
            }
            MetaExpr::UseDecl(use_decl) => {
                println!("DEBUG: Evaluating UseDecl with is_expression_context={is_expression_context}");
                self.eval_use_decl(&use_decl.tree, is_expression_context)
            }
            MetaExpr::Call { span, lhs, args } => {
                self.eval_fn_call(*span, lhs, args)
            }
            MetaExpr::RawOutputGroup {
                span,
                delimiter,
                contents,
            } => {
                let mut res = TokenSink::new(true);
                self.eval_stmt_list_to_stream(
                    &mut res, *span, contents, true,
                )?;
                Ok(Rc::new(MetaValue::Token(TokenTree::Group(Group::new(
                    *delimiter,
                    TokenStream::from_iter(res.into_vec()),
                )))))
            }
            MetaExpr::Group {
                span,
                delimiter,
                body,
            } => {
                let mut res = TokenSink::new(true);
                self.eval_stmt_list_to_stream(
                    &mut res,
                    *span,
                    &body.stmts,
                    body.trailing_semi,
                )?;
                Ok(Rc::new(MetaValue::Token(TokenTree::Group(Group::new(
                    *delimiter,
                    TokenStream::from_iter(res.into_vec()),
                )))))
            }
            MetaExpr::For {
                span,
                pattern,
                variants_expr,
                body,
            } => {
                let input_list = self.eval(variants_expr, true)?;
                let MetaValue::List(list_elems) = &*input_list else {
                    self.error(
                        variants_expr.span(),
                        format!("cannot iterate over {}", input_list.kind()),
                    );
                    return Err(EvalError::Error);
                };
                let mut res_tokens = TokenSink::new(true);

                for elem in &*list_elems.borrow() {
                    self.push_eval_scope();
                    let mut res = self.match_and_bind_pattern(
                        pattern,
                        elem.clone(),
                        false,
                    );
                    if res.is_ok() {
                        res = self.eval_block_to_stream(
                            &mut res_tokens,
                            *span,
                            body,
                        );
                    }
                    self.scopes.pop();

                    match res {
                        Ok(()) => continue,
                        Err(EvalError::Continue) => continue,
                        Err(e @ EvalError::Return { .. }) => {
                            return Err(e);
                        }
                        Err(EvalError::Break { value, span }) => {
                            if value.is_some() {
                                self.error(
                                    span,
                                    "`break` values are not supported in `for`",
                                );
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
                Ok(Rc::new(MetaValue::Tokens(res_tokens.into_vec())))
            }
            MetaExpr::Loop { span, body } => {
                let mut res = TokenSink::new(true);
                loop {
                    self.push_eval_scope();
                    let v = self.eval_block_to_stream(&mut res, *span, body);
                    self.pop_scope();

                    match v {
                        Ok(()) => continue,
                        Err(EvalError::Continue) => continue,
                        Err(e @ EvalError::Return { .. }) => {
                            return Err(e);
                        }
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
                Ok(Rc::new(MetaValue::Tokens(res.into_vec())))
            }
            MetaExpr::While {
                condition,
                span,
                body,
            } => {
                let mut res_tokens = TokenSink::new(true);
                loop {
                    let condition_val = self.eval(condition, true)?;
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
                    let res = self.eval_block_to_stream(
                        &mut res_tokens,
                        *span,
                        body,
                    );
                    self.scopes.pop();

                    match res {
                        Ok(()) => continue,
                        Err(EvalError::Continue) => continue,
                        Err(e @ EvalError::Return { .. }) => {
                            return Err(e);
                        }
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
                Ok(Rc::new(MetaValue::Tokens(res_tokens.into_vec())))
            }
            MetaExpr::WhileLet {
                pattern,
                expr,
                span,
                body,
            } => {
                let input_list = self.eval(expr, true)?;
                let MetaValue::List(list_elems) = &*input_list else {
                    self.error(
                        expr.span(),
                        format!("cannot iterate over {}", input_list.kind()),
                    );
                    return Err(EvalError::Error);
                };
                let mut res_tokens = TokenSink::new(true);

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
                        res = self.eval_block_to_stream(
                            &mut res_tokens,
                            *span,
                            body,
                        );
                    }
                    self.scopes.pop();

                    match res {
                        Ok(()) => continue,
                        Err(EvalError::Continue) => continue,
                        Err(e @ EvalError::Return { .. }) => {
                            return Err(e);
                        }
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
                Ok(Rc::new(MetaValue::Tokens(res_tokens.into_vec())))
            }
            MetaExpr::Block(block) => {
                self.push_eval_scope();
                let res = self.eval_block_list_to_meta_val(block.span, block);
                self.scopes.pop();
                res
            }
            MetaExpr::FnDecl(f) => {
                self.insert_binding(
                    f.ident.clone(),
                    false,
                    false,
                    Rc::new(MetaValue::Fn(f.clone())),
                );

                if f.visibility.is_extern() {
                    let mut quoted = TokenSink::new(false);

                    self.append_quoted_fn_decl(&mut quoted, f, true)?;

                    self.extern_decls.push(ExternDecl::Fn {
                        decl: f.clone(),
                        quoted: quoted.into_vec(),
                    });
                }

                if is_expression_context {
                    Ok(Rc::new(MetaValue::Fn(f.clone())))
                } else {
                    Ok(self.empty_token_list.clone())
                }
            }
            MetaExpr::Lambda(f) => Ok(Rc::new(MetaValue::Lambda(f.clone()))),
            MetaExpr::List { span: _, exprs } => {
                let mut elements = Vec::new();
                for e in exprs {
                    elements.push(self.eval(e, true)?);
                }
                Ok(Rc::new(MetaValue::List(RefCell::new(elements))))
            }
            MetaExpr::Tuple { span: _, exprs } => {
                let mut elements = Vec::new();
                for e in exprs {
                    elements.push(self.eval(e, true)?);
                }
                Ok(Rc::new(MetaValue::Tuple(elements)))
            }
            MetaExpr::IfExpr {
                span,
                condition,
                body,
                else_expr,
            } => {
                let condition_val = self.eval(condition, true)?;
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
                    self.eval_block_list_to_meta_val(*span, body)
                } else if let Some(else_expr) = else_expr {
                    self.eval(else_expr, is_expression_context)
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
                let input_list = self.eval(&ep.for_expr, true)?;
                let MetaValue::List(list_elems) = &*input_list else {
                    self.error(
                        ep.for_expr.span(),
                        format!("cannot iterate over {}", input_list.kind()),
                    );
                    return Err(EvalError::Error);
                };
                let mut res_tokens = TokenSink::new(true);
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
                            true,
                        );
                    };
                    self.scopes.pop();
                    res?;
                }
                self.eval_stmt_list_to_stream(
                    &mut res_tokens,
                    ep.span,
                    &ep.match_arm_guard,
                    true,
                )?;
                self.eval_stmt_list_to_stream(
                    &mut res_tokens,
                    ep.span,
                    &ep.match_arm_body,
                    true,
                )?;
                Ok(Rc::new(MetaValue::Tokens(res_tokens.into_vec())))
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
        let list_val = self.eval(list_expr, true)?;
        let MetaValue::List(list) = &*list_val else {
            self.error(
                *span,
                format!("cannot index into `{}`", list_val.kind()),
            );
            return Err(EvalError::Error);
        };

        let index_val = self.eval(index, true)?;
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
        let operand = self.eval(operand, true)?;
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
            MetaExpr::Ident(ident) => {
                for scope_idx in 0..self.scopes.len() {
                    if let Some(binding) =
                        self.scopes[scope_idx].bindings.get(&ident.name)
                    {
                        if !binding.mutable {
                            self.error(
                                span,
                                format!(
                                    "cannot assign to immutable variable `{}`",
                                    &ident.name
                                ),
                            );
                            return Err(EvalError::Error);
                        }
                        let binding_value = binding.value.clone();

                        let rhs_val = self.eval(rhs, true)?;
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
                            .get_mut(&ident.name)
                            .unwrap()
                            .value = new_value;
                        return Ok(self.empty_token_list.clone());
                    }
                }
                self.error(
                    span,
                    format!("cannot find `{}` in this scope", ident.name),
                );
                Err(EvalError::Error)
            }
            MetaExpr::Literal { .. }
            | MetaExpr::LetBinding { .. }
            | MetaExpr::Call { .. }
            | MetaExpr::FnDecl(..)
            | MetaExpr::Lambda(..)
            | MetaExpr::RawOutputGroup { .. }
            | MetaExpr::Group { .. }
            | MetaExpr::IfExpr { .. }
            | MetaExpr::For { .. }
            | MetaExpr::Loop { .. }
            | MetaExpr::While { .. }
            | MetaExpr::WhileLet { .. }
            | MetaExpr::ExpandPattern(..)
            | MetaExpr::Block { .. }
            | MetaExpr::List { .. }
            | MetaExpr::Tuple { .. }
            | MetaExpr::Break { .. }
            | MetaExpr::Return { .. }
            | MetaExpr::Continue { .. }
            | MetaExpr::OpUnary { .. }
            | MetaExpr::UseDecl { .. }
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
                let rhs_val = self.eval(rhs, true)?;
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

        let lhs_val = self.eval(lhs, true)?;
        let rhs_val = self.eval(rhs, true)?;

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
            (MetaValue::List(lhs), MetaValue::List(rhs))
                if op_kind == BinaryOpKind::Add =>
            {
                Ok(Rc::new(MetaValue::List(RefCell::new(
                    (*lhs.borrow())
                        .iter()
                        .chain(&**rhs.borrow())
                        .cloned()
                        .collect(),
                ))))
            }
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
        let lhs =
            if let MetaExpr::Ident(MetaIdent { name, raw: _, span }) = lhs {
                // We could just evaluate but we want a better error message
                // than "`token` is not callable".
                if let Some(val) = self.lookup(name, false) {
                    val
                } else {
                    self.error(*span, format!("undefined function `{name}`"));
                    return Err(EvalError::Error);
                }
            } else {
                self.eval(lhs, true)?
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
            let Ok(val) = self.eval(arg, true) else {
                return Err(EvalError::Error);
            };
            args.push(val);
        }

        callable.call(self, span, &args)
    }

    fn insert_binding(
        &mut self,
        ident: MetaIdent,
        mutable: bool,
        super_bound: bool,
        value: Rc<MetaValue>,
    ) {
        let binding = Binding {
            ident,
            mutable,
            super_bound,
            value,
        };
        self.scopes
            .last_mut()
            .unwrap()
            .bindings
            .insert(binding.ident.name.clone(), binding);
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

    fn generate_let_bindings_for_use_tree(
        &mut self,
        tree: &UseTree,
    ) -> Result<()> {
        match tree {
            UseTree::Path { replacement, .. } => {
                // Generate a let binding: let <binding> = <original_name>;
                if let Some(value) =
                    self.lookup(&replacement.ident.name, false)
                {
                    self.insert_binding(
                        replacement.ident.clone(),
                        false,
                        false,
                        value,
                    );
                }
                Ok(())
            }
            UseTree::Group { items, .. } => {
                // Generate let bindings for all items in the group
                for item in items {
                    self.generate_let_bindings_for_use_tree(item)?;
                }
                Ok(())
            }
            UseTree::Rename { replacement, .. } => {
                // Generate a let binding: let <binding> = <original_name>;
                if let Some(value) =
                    self.lookup(&replacement.ident.name, false)
                {
                    self.insert_binding(
                        replacement.ident.clone(),
                        false,
                        false,
                        value,
                    );
                }
                Ok(())
            }
        }
    }

    fn eval_use_decl(
        &mut self,
        tree: &UseTree,
        is_expression_context: bool,
    ) -> Result<Rc<MetaValue>> {
        if !is_expression_context {
            self.generate_let_bindings_for_use_tree(tree)?;
        }

        // Always try to return the actual imported values
        match tree {
            UseTree::Path { replacement, .. } => {
                // Look up the extern binding by the original name
                let target_name = &replacement.ident.name;
                if let Some(value) = self.lookup(target_name, false) {
                    Ok(value)
                } else {
                    // If not found, return the binding as an identifier token
                    Ok(Rc::new(MetaValue::Token(TokenTree::Ident(
                        proc_macro::Ident::new(
                            &replacement.binding,
                            replacement.ident.span,
                        ),
                    ))))
                }
            }
            UseTree::Group { items, .. } => {
                // Return a tuple of all imported items
                let mut values = Vec::new();
                for item in items {
                    values.push(self.eval_use_decl(item, true)?); // Recursively get values
                }
                Ok(Rc::new(MetaValue::Tuple(values)))
            }
            UseTree::Rename { replacement, .. } => {
                // Look up the extern binding by the original name
                let target_name = &replacement.ident.name;
                if let Some(value) = self.lookup(target_name, false) {
                    Ok(value)
                } else {
                    // If not found, return the binding as an identifier token
                    Ok(Rc::new(MetaValue::Token(TokenTree::Ident(
                        proc_macro::Ident::new(
                            &replacement.binding,
                            replacement.ident.span,
                        ),
                    ))))
                }
            }
        }
    }
}
