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

#[derive(Debug)]
struct SubstitutionLevel {
    offset: usize,
    points: Vec<usize>,
    sublevels: Vec<SubstitutionLevel>,
}

struct Template {
    trailing_comma: bool,
    substitutions: SubstitutionLevel,
    end: usize,
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
            if ident.to_string() == expand.ident_name {
                level.points.push(offset + i);
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
            if ident.to_string() == expand.ident_name {
                substitutions.points.push(tok_idx);
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
        trailing_comma: false,
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
    let mut trailing_comma = false;
    while i < tokens.len() {
        let tok_idx = i;
        i += 1;

        let tt = &tokens[tok_idx];

        if let TokenTree::Ident(ident) = tt {
            if ident.to_string() == expand.ident_name {
                substitutions.points.push(tok_idx);
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
        trailing_comma = true;
        break;
    }
    Ok(Template {
        trailing_comma,
        substitutions,
        end: i,
    })
}

fn expand_substitution(
    source: &[TokenTree],
    expansion: &mut Vec<TokenTree>,
    subst: &SubstitutionLevel,
    variant: &[TokenTree],
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
        let next_subst_point = subst.points.get(point_idx).copied().unwrap_or(usize::MAX);

        if next_subst_point < next_sublevel_pos {
            expansion.extend(source[template_pos..next_subst_point].iter().cloned());
            template_pos = next_subst_point + 1;
            expansion.extend(variant.iter().cloned());
            point_idx += 1;
            continue;
        }
        let TokenTree::Group(g) = &source[next_sublevel_pos] else {
            panic!("expected group found {:?}", source[next_sublevel_pos]);
        };
        let group_source = g.stream().into_iter().collect::<Vec<_>>();
        let mut group_target = Vec::new();
        expand_substitution(
            &group_source,
            &mut group_target,
            &subst.sublevels[sublevel_idx],
            variant,
        );
        expansion.extend(source[template_pos..next_sublevel_pos].iter().cloned());
        expansion.push(TokenTree::Group(Group::new(
            g.delimiter(),
            TokenStream::from_iter(group_target),
        )));
        sublevel_idx += 1;
        template_pos = next_sublevel_pos + 1;
    }
    expansion.extend(source[template_pos..].iter().cloned());
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

        let pattern_template = parse_match_arm_pattern(&expand_attribute, &tokens, i)?;
        i = pattern_template.end;

        let body_template = parse_match_arm_body(&expand_attribute, &tokens, i)?;
        i = pattern_template.end;

        let expand_directive_end = i;

        let mut expansion = Vec::new();

        for variant in &expand_attribute.variants {
            expand_substitution(
                &tokens[..pattern_template.end],
                &mut expansion,
                &pattern_template.substitutions,
                variant,
            );
            expand_substitution(
                &tokens[..body_template.end],
                &mut expansion,
                &body_template.substitutions,
                variant,
            );
            if !body_template.trailing_comma {
                expansion.push(TokenTree::Punct(Punct::new(',', Spacing::Alone)));
            }
        }

        i = expand_directive_start + expansion.len();
        tokens.splice(expand_directive_start..expand_directive_end, expansion);
    }

    Ok(TokenStream::from_iter(tokens))
}
