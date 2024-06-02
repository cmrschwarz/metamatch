use itertools::Itertools;
use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

struct ExpandAttribute {
    ident_name: String,
    variants: Vec<Vec<TokenTree>>,
}

pub(crate) struct SyntaxError {
    pub message: String,
    pub span: Span,
}

struct Template<'a> {
    tokens: &'a [TokenTree],
    substitution_points: Vec<usize>,
}

#[proc_macro]
pub fn metamatch(body: TokenStream) -> TokenStream {
    match metamatch_impl(body) {
        Ok(body) => body,
        Err(err) => err.into_compile_error(),
    }
}

impl SyntaxError {
    pub(crate) fn into_compile_error(self) -> TokenStream {
        // compile_error! { $message }
        TokenStream::from_iter(vec![
            TokenTree::Ident(Ident::new("compile_error", self.span)),
            TokenTree::Punct({
                let mut punct = Punct::new('!', Spacing::Alone);
                punct.set_span(self.span);
                punct
            }),
            TokenTree::Group({
                let mut group = Group::new(Delimiter::Brace, {
                    TokenStream::from_iter(vec![TokenTree::Literal({
                        let mut string = Literal::string(&self.message);
                        string.set_span(self.span);
                        string
                    })])
                });
                group.set_span(self.span);
                group
            }),
        ])
    }
}

fn tok_span_or_parent_close(tok: &Option<TokenTree>, parent: &Group) -> Span {
    tok.as_ref()
        .map(|t| t.span())
        .unwrap_or(parent.span_close())
}

fn parse_expand_variants(variants_group: &Group) -> Result<Vec<Vec<TokenTree>>, SyntaxError> {
    Ok(variants_group
        .stream()
        .into_iter()
        .chunk_by(|t| matches!(t, TokenTree::Punct(p) if p.as_char() == ','))
        .into_iter()
        .map(|(_, iter)| iter.collect::<Vec<_>>())
        .collect::<Vec<_>>())
}

fn parse_expand_attribute(
    tt1: &TokenTree,
    tt2: &TokenTree,
) -> Result<Option<ExpandAttribute>, SyntaxError> {
    let TokenTree::Punct(punct) = tt1 else {
        return Ok(None);
    };
    if punct.as_char() != '#' {
        return Ok(None);
    }
    let TokenTree::Group(attrib_group) = tt2 else {
        return Ok(None);
    };

    if attrib_group.delimiter() != Delimiter::Bracket {
        return Ok(None);
    }
    let mut attrib_body = attrib_group.stream().into_iter();
    let Some(TokenTree::Ident(ident)) = attrib_body.next() else {
        return Ok(None);
    };
    if ident.to_string() != "expand" {
        return Ok(None);
    }
    // from this point on we know that the user intended to talk to us
    // ( we found `#[expand]` ), so we start to emit syntax errors in case
    // something unexpected appears

    let expand_body_tok = attrib_body.next();
    let expand_body = match expand_body_tok {
        Some(TokenTree::Group(expand_body))
            if expand_body.delimiter() == Delimiter::Parenthesis =>
        {
            expand_body
        }
        _ => {
            return Err(SyntaxError {
                message: "expected `(` to begin expand body".to_string(),
                span: tok_span_or_parent_close(&expand_body_tok, attrib_group),
            });
        }
    };

    if let Some(stray_tok) = attrib_body.next() {
        return Err(SyntaxError {
            message: "expected expand attribute to end after body".to_string(),
            span: stray_tok.span(),
        });
    }

    let mut expand_body_stream = expand_body.stream().into_iter();

    let expand_ident_tok = expand_body_stream.next();

    let ident_name = match expand_ident_tok {
        Some(TokenTree::Ident(ident)) => ident.to_string(),
        _ => {
            return Err(SyntaxError {
                message: "expected expand attribute body to start with an identifier".to_string(),
                span: tok_span_or_parent_close(&expand_ident_tok, &expand_body),
            });
        }
    };

    let kw_in_tok = expand_body_stream.next();

    match kw_in_tok {
        Some(TokenTree::Ident(ident)) if ident.to_string() == "in" => (),
        _ => {
            return Err(SyntaxError {
                message: "expand `in` after expand identifier".to_string(),
                span: tok_span_or_parent_close(&kw_in_tok, &expand_body),
            });
        }
    };

    let variants_group_tok = expand_body_stream.next();

    let variants_group = match variants_group_tok {
        Some(TokenTree::Group(variants_group))
            if variants_group.delimiter() == Delimiter::Bracket =>
        {
            variants_group
        }
        _ => {
            return Err(SyntaxError {
                message: "expected `[` to begin expand variants".to_string(),
                span: tok_span_or_parent_close(&variants_group_tok, attrib_group),
            });
        }
    };

    Ok(Some(ExpandAttribute {
        ident_name,
        variants: parse_expand_variants(&variants_group)?,
    }))
}

fn parse_match_arm_pattern<'a>(
    expand: &ExpandAttribute,
    tokens: &'a [TokenTree],
) -> Result<Template<'a>, SyntaxError> {
    todo!()
}
fn parse_match_arm_body<'a>(
    expand: &ExpandAttribute,
    tokens: &'a [TokenTree],
) -> Result<Template<'a>, SyntaxError> {
    todo!()
}

fn expand_variant(
    expansion: &mut Vec<TokenTree>,
    variant: &[TokenTree],
    pattern_template: &Template,
    body_template: &Template,
) {
    let len_before = expansion.len();
    expansion.extend(pattern_template.tokens.iter().cloned());
    expansion.extend(body_template.tokens.iter().cloned());
}

fn metamatch_impl(body: TokenStream) -> Result<TokenStream, SyntaxError> {
    let mut tokens = Vec::from_iter(body);
    let mut i = 0;
    while i < tokens.len() {
        let tt = &mut tokens[i];

        if let TokenTree::Group(group) = tt {
            let content = metamatch_impl(group.stream())?;
            let original_span = group.span();
            *group = Group::new(group.delimiter(), content);
            group.set_span(original_span);
            i += 1;
            continue;
        }

        if i + 1 == tokens.len() {
            break;
        }

        let Some(expand_attribute) = parse_expand_attribute(&tokens[i], &tokens[i + 1])? else {
            i += 1;
            continue;
        };
        let expand_directive_start = i;
        i += 2;

        let pattern_template = parse_match_arm_pattern(&expand_attribute, &tokens[i..])?;
        i += pattern_template.tokens.len();

        let body_template = parse_match_arm_body(&expand_attribute, &tokens[i..])?;
        i += body_template.tokens.len();

        let expand_directive_end = i;

        let mut expansion = Vec::new();

        for variant in &expand_attribute.variants {
            expand_variant(&mut expansion, variant, &pattern_template, &body_template);
        }

        i = expand_directive_start + expansion.len();
        tokens.splice(expand_directive_start..expand_directive_end, expansion);
    }

    Ok(TokenStream::from_iter(tokens))
}
