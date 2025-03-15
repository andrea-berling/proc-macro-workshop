use std::iter;

use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Literal, TokenTree};
use quote::{format_ident, quote};
use syn::{parse::Parse, parse_macro_input, Ident, LitInt, Token};

#[derive(Debug, Clone)]
struct Sequence {
    ident: Ident,
    _in_token: Token![in],
    lower_bound: LitInt,
    _range_token: Option<Token![..]>,
    _range_inclusive_token: Option<Token![..=]>,
    upper_bound: LitInt,
    contents: Group,
}

#[derive(Debug, Clone)]
struct RepeatableBlock {
    contents: Group,
    _star_token: Token![*],
    rest: Option<proc_macro2::TokenStream>,
}

impl Parse for Sequence {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;
        let _in_token: Token![in] = input.parse()?;
        let lower_bound: LitInt = input.parse()?;
        let mut _range_token = None;
        let mut _range_inclusive_token = None;
        if let Ok(token) = input.parse::<Token![..=]>() {
            _range_inclusive_token = Some(token)
        } else {
            let token = input.parse::<Token![..]>()?;
            _range_token = Some(token);
        }
        let upper_bound: LitInt = input.parse()?;
        let contents: Group = input.parse()?;
        Ok(Sequence {
            ident,
            _in_token,
            lower_bound,
            _range_token,
            _range_inclusive_token,
            upper_bound,
            contents,
        })
    }
}

impl Parse for RepeatableBlock {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let contents: Group = input.parse()?;
        if contents.delimiter() != proc_macro2::Delimiter::Parenthesis {
            return Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                "Expected a parenthesis",
            ));
        }
        let _star_token: Token![*] = input.parse()?;
        let mut rest: Option<proc_macro2::TokenStream> = None;
        if !input.is_empty() {
            rest = Some(input.parse()?);
        }
        Ok(RepeatableBlock {
            contents,
            _star_token,
            rest,
        })
    }
}

fn expand_placeholder(tt: TokenTree, placeholder: Ident, value: i64) -> TokenTree {
    match tt {
        TokenTree::Group(ref group) => {
            let mut new_tt: Vec<TokenTree> = vec![];
            let mut group_iterator = group.stream().into_iter().peekable();
            while let Some(token) = group_iterator.next() {
                if let TokenTree::Ident(ref ident) = token {
                    if group_iterator
                        .next_if(|tt| {
                            if let TokenTree::Punct(maybe_tilde_token) = tt {
                                if maybe_tilde_token.as_char() == '~' {
                                    return true;
                                }
                            }
                            false
                        })
                        .is_some()
                        && group_iterator
                            .next_if(|tt| {
                                if let TokenTree::Ident(ident) = tt {
                                    if *ident == placeholder {
                                        return true;
                                    }
                                }
                                false
                            })
                            .is_some()
                    {
                        let mut new_ident =
                            TokenTree::Ident(format_ident!("{}{value}", ident.to_string()));
                        new_ident.set_span(ident.span());
                        new_tt.push(new_ident);
                        continue;
                    }
                }
                new_tt.push(expand_placeholder(
                    token.clone(),
                    placeholder.clone(),
                    value,
                ))
            }
            let mut new_tt = TokenTree::Group(Group::new(
                group.delimiter(),
                proc_macro2::TokenStream::from_iter(new_tt),
            ));
            new_tt.set_span(tt.span());
            new_tt
        }
        TokenTree::Ident(ref ident) if *ident == placeholder => {
            let mut new_tt = TokenTree::Literal(Literal::i64_unsuffixed(value));
            new_tt.set_span(tt.span());
            new_tt
        }
        _ => tt,
    }
}

fn parse_group(
    input: Group,
    macro_ident: Ident,
    lower_bound: i64,
    upper_bound: i64,
) -> proc_macro2::TokenStream {
    let mut macro_iterator = input.stream().into_iter().peekable();
    let mut output = quote! {};
    while let Some(token_tree) = macro_iterator.next() {
        //eprintln!("Current token tree: {:#?}", token_tree);
        match token_tree {
            TokenTree::Group(g) => {
                output.extend(iter::once(TokenTree::Group(Group::new(
                    g.delimiter(),
                    parse_group(g, macro_ident.clone(), lower_bound, upper_bound),
                ))));
            }
            TokenTree::Punct(ref p) if p.as_char() == '#' => {
                if let Some(TokenTree::Group(g)) = macro_iterator.peek() {
                    if g.delimiter() == Delimiter::Parenthesis {
                        let tokens = proc_macro2::TokenStream::from_iter(macro_iterator.by_ref());
                        if let Ok(repeatable_block) = syn::parse2::<RepeatableBlock>(tokens) {
                            let mut new_tt = vec![];
                            for i in lower_bound..upper_bound {
                                let new_block = expand_placeholder(
                                    TokenTree::Group(repeatable_block.contents.clone()),
                                    macro_ident.clone(),
                                    i,
                                );
                                if let TokenTree::Group(new_block) = new_block {
                                    new_tt.extend(new_block.stream());
                                } else {
                                    new_tt.extend(iter::once(new_block));
                                }
                            }
                            output.extend(new_tt.into_iter());
                            if repeatable_block.rest.is_some() {
                                output.extend(repeatable_block.rest.unwrap().into_iter());
                            }
                            break;
                        }
                    }
                }
                output.extend(iter::once(token_tree.clone()));
            }
            _ => output.extend(iter::once(token_tree.clone())),
        }
        //eprintln!("Output: {:#?}", output);
    }
    output
}

fn look_for_repeatable_block(input: Group) -> bool {
    let mut found = false;
    let mut first_pass_iterator = input.stream().into_iter();
    while let Some(tt) = first_pass_iterator.next() {
        if let TokenTree::Punct(ref p) = tt {
            if p.as_char() == '#' {
                if let Some(TokenTree::Group(g)) = first_pass_iterator.next() {
                    if g.delimiter() == Delimiter::Parenthesis {
                        if let Some(TokenTree::Punct(ref p)) = first_pass_iterator.next() {
                            if p.as_char() == '*' {
                                found = true;
                                break;
                            }
                        }
                    }
                }
            }
        }
        if let TokenTree::Group(g) = tt {
            found = found || look_for_repeatable_block(g);
        }
    }
    found
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let macro_input = parse_macro_input!(input as Sequence);
    let macro_ident = macro_input.ident.clone();
    let (lower_bound, mut upper_bound): (i64, i64) = (
        macro_input.lower_bound.clone().base10_parse().unwrap(),
        macro_input.upper_bound.clone().base10_parse().unwrap(),
    );
    if macro_input._range_inclusive_token.is_some() {
        upper_bound += 1;
    }

    if look_for_repeatable_block(macro_input.contents.clone()) {
        parse_group(macro_input.contents, macro_ident, lower_bound, upper_bound).into()
    } else {
        let mut output = quote! {};
        for i in lower_bound..upper_bound {
            let new_block = expand_placeholder(
                TokenTree::Group(macro_input.contents.clone()),
                macro_ident.clone(),
                i,
            );
            if let TokenTree::Group(new_block) = new_block {
                output.extend(new_block.stream());
            } else {
                output.extend(iter::once(new_block));
            }
        }
        output.into()
    }
}
