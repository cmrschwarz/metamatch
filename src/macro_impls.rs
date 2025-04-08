use proc_macro::{Span, TokenStream};

use super::ast::{Context, ScopeKind};

pub trait IntoIterIntoVec {
    type Item;
    fn into_vec(self) -> Vec<Self::Item>;
}
impl<I: IntoIterator> IntoIterIntoVec for I {
    type Item = I::Item;

    fn into_vec(self) -> Vec<Self::Item> {
        self.into_iter().collect()
    }
}

pub fn unquote(body: TokenStream) -> TokenStream {
    let body = body.into_vec();
    let mut ctx = Context::default();

    ctx.push_dummy_scope(ScopeKind::Unquoted);
    let expr = ctx.parse_body_deny_trailing(Span::call_site(), &body);
    ctx.scopes.pop();

    ctx.eval_to_token_stream(Span::call_site(), &expr)
}

pub fn quote(body: TokenStream) -> TokenStream {
    let body = body.into_vec();
    let mut ctx = Context::default();

    ctx.push_dummy_scope(ScopeKind::Quoted);
    let Ok(exprs) = ctx.parse_raw_block_to_exprs(Span::call_site(), &body)
    else {
        return ctx.expand_errors();
    };
    ctx.scopes.pop();

    ctx.eval_to_token_stream(Span::call_site(), &exprs)
}

pub fn replicate(attrib: TokenStream, body: TokenStream) -> TokenStream {
    let attrib = attrib.into_vec();
    let mut ctx = Context::default();
    ctx.push_dummy_scope(ScopeKind::Unquoted);
    let (mut exprs, rest, trailing_block) =
        ctx.parse_body(Span::call_site(), &attrib, true);

    if !rest.is_empty() && ctx.errors.is_empty() {
        let tb = trailing_block.expect("rest without trailing block");
        ctx.error(
            attrib[attrib.len() - rest.len() - 1].span(),
            format!("template tag `{}` is never closed", tb.to_str()),
        );
        return ctx.expand_errors();
    }

    let Some(trailing_block) = trailing_block else {
        ctx.error(
            exprs.last().map(|e| e.span()).unwrap_or(Span::call_site()),
            "replicate ignores the attribute body",
        );
        return ctx.expand_errors();
    };

    let body = body.into_vec();
    let Ok(contents) = ctx.parse_raw_block_to_exprs(Span::call_site(), &body)
    else {
        return ctx.expand_errors();
    };

    ctx.close_expr_after_trailing_body(&mut exprs, trailing_block, contents);

    ctx.pop_dummy_scope();
    ctx.pop_dummy_scope();

    ctx.eval_to_token_stream(Span::call_site(), &exprs)
}

pub fn metamatch(body: TokenStream) -> TokenStream {
    let body = body.into_vec();
    let mut ctx = Context::default();

    ctx.push_dummy_scope(ScopeKind::Metamatch);

    let Ok(exprs) = ctx.parse_raw_block_to_exprs(Span::call_site(), &body)
    else {
        return ctx.expand_errors();
    };
    ctx.scopes.pop();

    ctx.eval_to_token_stream(Span::call_site(), &exprs)
}
