use proc_macro::{Span, TokenTree};

use super::ast::{Context, ExprBlock, ScopeKind};

pub trait IntoVec {
    type Item;
    fn into_vec(self) -> Vec<Self::Item>;
}
impl<I: IntoIterator> IntoVec for I {
    type Item = I::Item;

    fn into_vec(self) -> Vec<Self::Item> {
        self.into_iter().collect()
    }
}

pub fn parse_eval(ctx: &mut Context, body: Vec<TokenTree>) -> ExprBlock {
    ctx.push_dummy_scope(ScopeKind::Eval);
    let exprs = ctx.parse_expr_block_deny_trailing(Span::call_site(), &body);
    ctx.scopes.pop();

    exprs
}

pub fn parse_template(ctx: &mut Context, body: Vec<TokenTree>) -> ExprBlock {
    ctx.push_dummy_scope(ScopeKind::Template);
    let exprs = ctx
        .parse_raw_block_to_exprs(Span::call_site(), &body)
        .unwrap_or_default();
    ctx.scopes.pop();

    ExprBlock {
        span: Span::call_site(),
        stmts: exprs,
        trailing_semi: true,
    }
}

pub fn parse_replicate(
    ctx: &mut Context,
    attrib: Vec<TokenTree>,
    body: Vec<TokenTree>,
) -> ExprBlock {
    ctx.push_dummy_scope(ScopeKind::Eval);
    let (mut exprs, rest, trailing_block) =
        ctx.parse_body(Span::call_site(), &attrib, true);

    if !rest.is_empty() && ctx.errors.is_empty() {
        let tb = trailing_block.expect("rest without trailing block");
        ctx.error(
            attrib[attrib.len() - rest.len() - 1].span(),
            format!("template tag `{}` is never closed", tb.to_str()),
        );
        ctx.pop_scope();
        return ExprBlock::default();
    }

    let Some(trailing_block) = trailing_block else {
        ctx.error(
            exprs
                .stmts
                .last()
                .map(|e| e.span())
                .unwrap_or(Span::call_site()),
            "replicate ignores the attribute body",
        );
        ctx.pop_scope();
        return ExprBlock::default();
    };

    let Ok(contents) = ctx.parse_raw_block_to_exprs(Span::call_site(), &body)
    else {
        ctx.pop_scope();
        return ExprBlock::default();
    };

    ctx.close_expr_after_trailing_body(
        &mut exprs.stmts,
        trailing_block,
        contents,
    );

    ctx.pop_scope();
    ctx.pop_scope();

    exprs
}

pub fn parse_metamatch(ctx: &mut Context, body: Vec<TokenTree>) -> ExprBlock {
    ctx.push_dummy_scope(ScopeKind::Metamatch);

    let stmts = ctx
        .parse_raw_block_to_exprs(Span::call_site(), &body)
        .unwrap_or_default();
    ctx.scopes.pop();

    ExprBlock {
        span: Span::call_site(),
        stmts,
        trailing_semi: true,
    }
}
