use std::collections::HashMap;

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

type ExpansionIdentIndex = usize;

#[derive(Debug, Default)]
struct ExpansionVariant {
    ident_replacements: Vec<Vec<TokenTree>>,
}

#[derive(Debug)]
struct ExpandAttribute {
    ident_names: HashMap<String, usize>,
    ident_tuple: bool,
    variants: Vec<ExpansionVariant>,
}

struct SyntaxError {
    pub message: String,
    pub span: Span,
}

#[derive(Debug)]
struct Substitution {
    pos: usize,
    kind: SubstitutionKind,
}

#[derive(Debug)]
enum SubstitutionKind {
    Inline(ExpansionIdentIndex),
    Nested(Vec<Substitution>),
}

#[derive(Debug)]
struct Template {
    offset: usize,
    substitutions: Vec<Substitution>,
    add_trailing_comma: bool,
    end: usize,
}

#[proc_macro]
pub fn metamatch(body: TokenStream) -> TokenStream {
    // TODO: make sure we have exactly one match body, throw an error
    // otherwise. no need to support any kind of `match match` BS for now

    let mut tokens = Vec::from_iter(body);

    let mut tokens_iter = tokens.iter_mut();

    match tokens_iter.next() {
        Some(TokenTree::Ident(ident)) if ident.to_string() == "match" => {}
        other => {
            return compile_error(
                "expected `match`",
                other.map(|t| t.span()).unwrap_or_else(Span::call_site),
            );
        }
    };

    loop {
        match tokens_iter.next() {
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Brace => {
                match process_match_body(group.stream()) {
                    Err(err) => {
                        return compile_error(&err.message, err.span);
                    }
                    Ok(body) => {
                        let mut body_replacement = Group::new(group.delimiter(), body);
                        body_replacement.set_span(group.span());
                        *group = body_replacement;
                    }
                };
                if let Some(stray_tok) = tokens_iter.next() {
                    return compile_error(
                        "unexpected token after suspected match body",
                        stray_tok.span(),
                    );
                }
                return TokenStream::from_iter(tokens);
            }
            Some(_) => continue,
            None => {
                return compile_error("no match body found", Span::call_site());
            }
        };
    }
}

fn compile_error(message: &str, span: Span) -> TokenStream {
    TokenStream::from_iter(vec![
        TokenTree::Ident(Ident::new("compile_error", span)),
        TokenTree::Punct({
            let mut punct = Punct::new('!', Spacing::Alone);
            punct.set_span(span);
            punct
        }),
        TokenTree::Group({
            let mut group = Group::new(Delimiter::Brace, {
                TokenStream::from_iter(vec![TokenTree::Literal({
                    let mut string =
                        Literal::string(&format!("`metamatch!` macro expansion: {}", message));
                    string.set_span(span);
                    string
                })])
            });
            group.set_span(span);
            group
        }),
    ])
}

fn tok_span_or_parent_close(tok: &Option<TokenTree>, parent: &Group) -> Span {
    tok.as_ref()
        .map(|t| t.span())
        .unwrap_or(parent.span_close())
}

fn parse_comma_separated_token_tree_lists(
    group: &Group,
) -> Result<Vec<Vec<TokenTree>>, SyntaxError> {
    let mut variants = Vec::new();
    let mut curr_variant = Vec::new();
    for tt in group.stream().into_iter() {
        if let TokenTree::Punct(p) = &tt {
            if p.as_char() == ',' {
                if curr_variant.is_empty() {
                    return Err(SyntaxError {
                        message: "expansion variant must have at least one token".to_string(),
                        span: p.span(),
                    });
                }
                variants.push(curr_variant);
                curr_variant = Vec::new();
                continue;
            }
        }
        curr_variant.push(tt);
    }
    if !curr_variant.is_empty() {
        variants.push(curr_variant);
    }
    Ok(variants)
}

fn parse_expand_variants(
    expand_attrib: &mut ExpandAttribute,
    variants_group: &Group,
) -> Result<(), SyntaxError> {
    let ident_count = expand_attrib.ident_names.len();

    if !expand_attrib.ident_tuple {
        debug_assert_eq!(ident_count, 1);
        let replacements = parse_comma_separated_token_tree_lists(variants_group)?;
        for r in replacements {
            expand_attrib.variants.push(ExpansionVariant {
                ident_replacements: vec![r],
            })
        }
        return Ok(());
    }
    let mut group_iter = variants_group.stream().into_iter();
    loop {
        let group = match group_iter.next() {
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Parenthesis => group,
            Some(other) => {
                return Err(SyntaxError {
                    message: "expected `(` or `]`".to_string(),
                    span: other.span(),
                });
            }
            None => break,
        };
        let ident_replacements = parse_comma_separated_token_tree_lists(&group)?;
        let replacements_len = ident_replacements.len();
        if replacements_len != ident_count {
            return Err(SyntaxError {
                message: format!("expected {ident_count} replacements, got {replacements_len}",),
                span: group.span(),
            });
        }
        expand_attrib
            .variants
            .push(ExpansionVariant { ident_replacements });
        match group_iter.next() {
            Some(TokenTree::Punct(p)) if p.as_char() == ',' => {
                continue;
            }
            Some(other) => {
                return Err(SyntaxError {
                    message: "expected `,` or `]`".to_string(),
                    span: other.span(),
                });
            }
            None => break,
        }
    }
    Ok(())
}

fn parse_expand_ident_names(
    tok: &Option<TokenTree>,
    expand_group: &Group,
) -> Result<ExpandAttribute, SyntaxError> {
    let ident_group = match tok {
        Some(TokenTree::Ident(ident)) => {
            return Ok(ExpandAttribute {
                ident_names: HashMap::from_iter([(ident.to_string(), 0)]),
                ident_tuple: false,
                variants: Vec::new(),
            })
        }
        Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Parenthesis => group,
        _ => {
            return Err(SyntaxError {
                message: "expected `(` or identifier".to_string(),
                span: tok_span_or_parent_close(tok, expand_group),
            });
        }
    };

    let mut name_count = 0;
    let mut names = HashMap::new();

    let mut group_iter = ident_group.stream().into_iter();

    loop {
        let Some(first) = group_iter.next() else {
            break;
        };
        let TokenTree::Ident(ident) = first else {
            return Err(SyntaxError {
                message: "expected identifier".to_string(),
                span: first.span(),
            });
        };
        if names.insert(ident.to_string(), name_count).is_some() {
            return Err(SyntaxError {
                message: "same identifier used again".to_string(),
                span: ident.span(),
            });
        }
        name_count += 1;
        match group_iter.next() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == ',' => {
                continue;
            }
            Some(other) => {
                return Err(SyntaxError {
                    message: "expected `,`".to_string(),
                    span: other.span(),
                });
            }
            None => break,
        }
    }
    if name_count == 0 {
        return Err(SyntaxError {
            message: "expand identifier tuple can't be empty".to_string(),
            span: ident_group.span(),
        });
    }
    Ok(ExpandAttribute {
        ident_names: names,
        ident_tuple: true,
        variants: Vec::new(),
    })
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

    let mut expand_attrib = parse_expand_ident_names(&expand_ident_tok, &expand_body)?;

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

    parse_expand_variants(&mut expand_attrib, &variants_group)?;

    Ok(Some(expand_attrib))
}

fn find_substitutions(expand: &ExpandAttribute, group: &Group) -> Vec<Substitution> {
    let mut substitutions = Vec::new();
    for (i, tt) in group.stream().into_iter().enumerate() {
        try_add_substitutions(&tt, expand, &mut substitutions, i);
    }
    substitutions
}

fn parse_match_arm_pattern(
    expand: &ExpandAttribute,
    tokens: &[TokenTree],
    offset: usize,
) -> Result<Template, SyntaxError> {
    let mut substitutions = Vec::new();
    let mut i = offset;
    while i < tokens.len() {
        let tok_idx = i;
        i += 1;
        let tt = &tokens[tok_idx];
        if try_add_substitutions(tt, expand, &mut substitutions, tok_idx) {
            continue;
        }
        let TokenTree::Punct(p) = tt else {
            continue;
        };
        if p.as_char() != '=' {
            continue;
        }
        if p.spacing() != Spacing::Joint {
            continue;
        }
        let Some(tt) = &tokens.get(i) else {
            break;
        };
        let TokenTree::Punct(p) = tt else {
            continue;
        };
        i += 1;
        if p.as_char() != '>' {
            continue;
        }
        // we include the arm here because we will always have to
        // paste that anyways
        break;
    }
    Ok(Template {
        offset,
        add_trailing_comma: false,
        substitutions,
        end: i,
    })
}

fn try_add_substitutions(
    tt: &TokenTree,
    expand: &ExpandAttribute,
    substitutions: &mut Vec<Substitution>,
    tok_idx: usize,
) -> bool {
    if let TokenTree::Ident(ident) = tt {
        if let Some(&ident_idx) = expand.ident_names.get(&ident.to_string()) {
            substitutions.push(Substitution {
                pos: tok_idx,
                kind: SubstitutionKind::Inline(ident_idx),
            });
            return true;
        }
    }
    if let TokenTree::Group(g) = tt {
        let nested = find_substitutions(expand, g);
        if !nested.is_empty() {
            substitutions.push(Substitution {
                pos: tok_idx,
                kind: SubstitutionKind::Nested(nested),
            });
        }
        return true;
    }
    false
}
fn parse_match_arm_body(
    expand: &ExpandAttribute,
    tokens: &[TokenTree],
    offset: usize,
) -> Result<Template, SyntaxError> {
    let mut substitutions = Vec::new();
    let mut i = offset;
    let mut add_trailing_comma = true;
    while i < tokens.len() {
        let tok_idx = i;
        i += 1;

        let tt = &tokens[tok_idx];
        if try_add_substitutions(tt, expand, &mut substitutions, tok_idx) {
            continue;
        }
        let TokenTree::Punct(p) = tt else {
            continue;
        };
        if p.as_char() != ',' {
            continue;
        }
        add_trailing_comma = false;
        break;
    }
    Ok(Template {
        offset,
        add_trailing_comma,
        substitutions,
        end: i,
    })
}

fn expand_template(
    source: &[TokenTree],
    expansion: &mut Vec<TokenTree>,
    template: &Template,
    variant: &ExpansionVariant,
) {
    expand_substitution(
        &source[..template.end],
        expansion,
        &template.substitutions,
        variant,
        template.offset,
    );
    if template.add_trailing_comma {
        expansion.push(TokenTree::Punct(Punct::new(',', Spacing::Alone)));
    }
}

fn expand_substitution(
    source: &[TokenTree],
    expansion: &mut Vec<TokenTree>,
    substitutions: &[Substitution],
    variant: &ExpansionVariant,
    initial_offset: usize,
) {
    let mut template_pos = initial_offset;

    for subst in substitutions {
        match &subst.kind {
            SubstitutionKind::Inline(ident_idx) => {
                expansion.extend(source[template_pos..subst.pos].iter().cloned());
                expansion.extend(variant.ident_replacements[*ident_idx].iter().cloned());
            }
            SubstitutionKind::Nested(nested_substs) => {
                let TokenTree::Group(source_group) = &source[subst.pos] else {
                    unreachable!()
                };
                let group_body = source_group.stream().into_iter().collect::<Vec<_>>();
                let mut group_target = Vec::new();
                expand_substitution(&group_body, &mut group_target, nested_substs, variant, 0);
                expansion.extend(source[template_pos..subst.pos].iter().cloned());
                let mut group = Group::new(
                    source_group.delimiter(),
                    TokenStream::from_iter(group_target),
                );
                group.set_span(source_group.span());
                expansion.push(TokenTree::Group(group));
            }
        }
        template_pos = subst.pos + 1;
    }
    expansion.extend(source[template_pos..].iter().cloned());
}

fn process_match_body(body: TokenStream) -> Result<TokenStream, SyntaxError> {
    let input_tokens = Vec::from_iter(body);
    let mut output_tokens = Vec::new();
    let mut i = 0;
    let mut last_directive_end = 0;
    while i < input_tokens.len() {
        if i + 1 == input_tokens.len() {
            break;
        }

        let Some(expand_attribute) =
            parse_expand_attribute(&input_tokens[i], &input_tokens[i + 1])?
        else {
            i += 1;
            continue;
        };

        let expand_directive_start = i;
        let pattern_template =
            parse_match_arm_pattern(&expand_attribute, &input_tokens, expand_directive_start + 2)?;
        let body_template =
            parse_match_arm_body(&expand_attribute, &input_tokens, pattern_template.end)?;

        output_tokens.extend(
            input_tokens[last_directive_end..expand_directive_start]
                .iter()
                .cloned(),
        );
        last_directive_end = body_template.end;
        i = last_directive_end;

        for variant in &expand_attribute.variants {
            expand_template(
                &input_tokens,
                &mut output_tokens,
                &pattern_template,
                variant,
            );
            expand_template(&input_tokens, &mut output_tokens, &body_template, variant);
        }
    }
    output_tokens.extend(input_tokens[last_directive_end..].iter().cloned());
    Ok(TokenStream::from_iter(output_tokens))
}
