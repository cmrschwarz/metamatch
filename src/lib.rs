use std::collections::HashMap;

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

type ExpansionIdentIndex = usize;
type TokenPos = usize;

#[derive(Default)]
struct ExpansionVariant {
    ident_replacements: Vec<Vec<TokenTree>>,
}

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
struct SubstitutionPoint {
    pos: TokenPos,
    ident_idx: ExpansionIdentIndex,
}

#[derive(Debug)]
struct SubstitutionLevel {
    offset: TokenPos,
    points: Vec<SubstitutionPoint>,
    sublevels: Vec<SubstitutionLevel>,
}

struct Template {
    substitutions: SubstitutionLevel,
    add_trailing_comma: bool,
    end: usize,
}

#[proc_macro]
pub fn metamatch(body: TokenStream) -> TokenStream {
    match metamatch_impl(body, true) {
        Ok(body) => body,
        Err(err) => err.into_compile_error(),
    }
}

impl SyntaxError {
    fn into_compile_error(self) -> TokenStream {
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

fn parse_comma_separated_token_tree_lists(
    variants_group: &Group,
) -> Result<Vec<Vec<TokenTree>>, SyntaxError> {
    let mut variants = Vec::new();
    let mut curr_variant = Vec::new();
    for tt in variants_group.stream().into_iter() {
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
    variants_group: &Group,
    ident_tuple: bool,
) -> Result<Vec<ExpansionVariant>, SyntaxError> {
    let mut res = Vec::new();
    if !ident_tuple {
        let variants = parse_comma_separated_token_tree_lists(variants_group)?;
        for v in variants {
            res.push(ExpansionVariant {
                ident_replacements: vec![v],
            })
        }
        return Ok(res);
    }
    todo!();
    Ok(res)
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
                span: tok_span_or_parent_close(tok, &expand_group),
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
                message: format!("same identifier used again"),
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

    expand_attrib.variants = parse_expand_variants(&variants_group, expand_attrib.ident_tuple)?;

    Ok(Some(expand_attrib))
}

fn find_substitution_points(
    expand: &ExpandAttribute,
    offset: usize,
    group: &Group,
) -> Option<SubstitutionLevel> {
    let mut level = SubstitutionLevel {
        offset,
        points: vec![],
        sublevels: vec![],
    };
    for (i, tt) in group.stream().into_iter().enumerate() {
        if let TokenTree::Ident(ident) = tt {
            if let Some(ident_idx) = expand.ident_names.get(&ident.to_string()) {
                level.points.push(SubstitutionPoint {
                    pos: offset + i,
                    ident_idx: *ident_idx,
                });
            }
            continue;
        }
        if let TokenTree::Group(g) = tt {
            if let Some(substs) = find_substitution_points(expand, offset + i, &g) {
                level.sublevels.push(substs);
            }
            continue;
        }
    }
    if level.points.is_empty() && level.sublevels.is_empty() {
        return None;
    }
    Some(level)
}

fn parse_match_arm_pattern(
    expand: &ExpandAttribute,
    tokens: &[TokenTree],
    offset: usize,
) -> Result<Template, SyntaxError> {
    let mut substitutions = SubstitutionLevel {
        offset,
        points: vec![],
        sublevels: vec![],
    };
    let mut i = offset;
    while i < tokens.len() {
        let tok_idx = i;
        i += 1;

        let tt = &tokens[tok_idx];

        if let TokenTree::Ident(ident) = tt {
            if let Some(&ident_idx) = expand.ident_names.get(&ident.to_string()) {
                substitutions.points.push(SubstitutionPoint {
                    pos: tok_idx,
                    ident_idx,
                });
            }
            continue;
        }
        if let TokenTree::Group(g) = tt {
            if let Some(substs) = find_substitution_points(expand, i, g) {
                substitutions.sublevels.push(substs);
            }
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
        add_trailing_comma: false,
        substitutions,
        end: i,
    })
}
fn parse_match_arm_body(
    expand: &ExpandAttribute,
    tokens: &[TokenTree],
    offset: usize,
) -> Result<Template, SyntaxError> {
    let mut substitutions = SubstitutionLevel {
        offset,
        points: vec![],
        sublevels: vec![],
    };
    let mut i = offset;
    let mut add_trailing_comma = true;
    while i < tokens.len() {
        let tok_idx = i;
        i += 1;

        let tt = &tokens[tok_idx];

        if let TokenTree::Ident(ident) = tt {
            if let Some(&ident_idx) = expand.ident_names.get(&ident.to_string()) {
                substitutions.points.push(SubstitutionPoint {
                    pos: tok_idx,
                    ident_idx,
                });
            }
            continue;
        }
        if let TokenTree::Group(g) = tt {
            if let Some(substs) = find_substitution_points(expand, i, g) {
                substitutions.sublevels.push(substs);
            }
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
    );
    if template.add_trailing_comma {
        expansion.push(TokenTree::Punct(Punct::new(',', Spacing::Alone)));
    }
}

fn expand_substitution(
    source: &[TokenTree],
    expansion: &mut Vec<TokenTree>,
    subst: &SubstitutionLevel,
    variant: &ExpansionVariant,
) {
    let mut template_pos = subst.offset;

    let mut sublevel_idx = 0;
    let mut point_idx = 0;
    loop {
        if sublevel_idx == subst.sublevels.len() && point_idx == subst.points.len() {
            break;
        }
        let next_sublevel_pos = subst
            .sublevels
            .get(sublevel_idx)
            .map(|sl| sl.offset)
            .unwrap_or(usize::MAX);
        let next_subst_point = subst.points.get(point_idx).unwrap_or(&SubstitutionPoint {
            pos: usize::MAX,
            ident_idx: 0,
        });

        if next_subst_point.pos < next_sublevel_pos {
            expansion.extend(source[template_pos..next_subst_point.pos].iter().cloned());
            template_pos = next_subst_point.pos + 1;
            expansion.extend(
                variant.ident_replacements[next_subst_point.ident_idx]
                    .iter()
                    .cloned(),
            );
            point_idx += 1;
            continue;
        }
        let TokenTree::Group(souuce_group) = &source[next_sublevel_pos] else {
            panic!("expected group found {:?}", source[next_sublevel_pos]);
        };
        let group_body = souuce_group.stream().into_iter().collect::<Vec<_>>();
        let mut group_target = Vec::new();
        expand_substitution(
            &group_body,
            &mut group_target,
            &subst.sublevels[sublevel_idx],
            variant,
        );
        expansion.extend(source[template_pos..next_sublevel_pos].iter().cloned());
        let mut group = Group::new(
            souuce_group.delimiter(),
            TokenStream::from_iter(group_target),
        );
        group.set_span(souuce_group.span());
        expansion.push(TokenTree::Group(group));
        sublevel_idx += 1;
        template_pos = next_sublevel_pos + 1;
    }
    expansion.extend(source[template_pos..].iter().cloned());
}

fn metamatch_impl(body: TokenStream, recurse: bool) -> Result<TokenStream, SyntaxError> {
    let mut input_tokens = Vec::from_iter(body);
    let mut i = 0;
    while i < input_tokens.len() {
        let tt = &mut input_tokens[i];

        if let TokenTree::Group(group) = tt {
            if !recurse {
                i += 1;
                continue;
            }
            let content = metamatch_impl(group.stream(), false)?;
            let original_span = group.span();
            *group = Group::new(group.delimiter(), content);
            group.set_span(original_span);
            i += 1;
            continue;
        }

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

        let expand_directive_end = body_template.end;

        let mut expansion = Vec::new();

        for variant in &expand_attribute.variants {
            expand_template(&input_tokens, &mut expansion, &pattern_template, variant);
            expand_template(&input_tokens, &mut expansion, &body_template, variant);
        }

        i = expand_directive_start + expansion.len();
        input_tokens.splice(expand_directive_start..expand_directive_end, expansion);
    }
    Ok(TokenStream::from_iter(input_tokens))
}
