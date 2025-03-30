use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream,
    TokenTree,
};
use std::{collections::HashMap, rc::Rc};

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
    match ctx.parse_body(&body) {
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
                    self.errors.push(MetaError {
                                span: *span,
                                message: format!(
                                    "function `{name}` with {} parameters called with {} arguments",
                                    builtin_fn.param_count,
                                    args.len()
                                ),
                            });
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

// parser
impl Context {
    fn parse_body(&mut self, body: &[TokenTree]) -> Result<MetaExpr> {
        Err(())
    }
}
