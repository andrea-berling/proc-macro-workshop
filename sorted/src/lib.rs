use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::ToTokens;
use syn::{parse_quote, visit_mut::VisitMut};

fn sort(item: syn::Item) -> Result<syn::Item, syn::Error> {
    let syn::Item::Enum(enum_item) = item else {
        return Err(syn::Error::new(
            Span::call_site(),
            "expected enum or match expression",
        ));
    };

    for (i, variant) in enum_item.variants.iter().enumerate() {
        if i == 0 {
            continue;
        }
        if variant.ident < enum_item.variants[i - 1].ident {
            let successor = enum_item
                .variants
                .iter()
                .find(|v| v.ident > variant.ident)
                .unwrap();
            return Err(syn::Error::new(
                variant.ident.span(),
                format!("{} should sort before {}", variant.ident, successor.ident),
            ));
        }
    }

    Ok(syn::Item::Enum(enum_item))
}

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let item = syn::parse_macro_input!(input as syn::Item);
    let mut output = item.to_token_stream();
    if let Err(err) = sort(item) {
        output.extend(err.to_compile_error());
    }
    output.into()
}

fn get_full_ident(path: &syn::Path) -> String {
    let mut full_ident = String::new();
    for segment in &path.segments {
        if !full_ident.is_empty() {
            full_ident.push_str("::".into());
        }
        full_ident.push_str(&segment.ident.to_string());
    }
    full_ident
}

struct MatchBranchesSorter;

impl VisitMut for MatchBranchesSorter {
    fn visit_expr_match_mut(&mut self, match_expr: &mut syn::ExprMatch) {
        if match_expr
            .attrs
            .iter()
            .any(|attr| attr.path().is_ident("sorted"))
        {
            match_expr
                .attrs
                .retain(|attr| !attr.path().is_ident("sorted"));
            let mut patterns_so_far = Vec::new();
            let mut tokens_to_append: Vec<syn::Arm> = Vec::new();
            for (i, arm) in match_expr.arms.iter().enumerate() {
                patterns_so_far.push(arm.pat.clone());
                if i == 0
                    || matches!(arm.pat, syn::Pat::Wild(_))
                    || matches!(match_expr.arms[i - 1].pat, syn::Pat::Wild(_))
                {
                    continue;
                }
                match (&match_expr.arms[i - 1].pat, &arm.pat) {
                    (syn::Pat::TupleStruct(prev), syn::Pat::TupleStruct(current)) => {
                        if get_full_ident(&prev.path) > get_full_ident(&current.path) {
                            let successor = patterns_so_far
                                .iter()
                                .map(|arm| {
                                    if let syn::Pat::TupleStruct(pat) = arm {
                                        pat
                                    } else {
                                        unreachable!()
                                    }
                                })
                                .find(|pat| {
                                    get_full_ident(&pat.path) > get_full_ident(&current.path)
                                })
                                .unwrap();
                            let error = syn::Error::new_spanned(
                                &current.path,
                                format!(
                                    "{} should sort before {}",
                                    get_full_ident(&current.path),
                                    get_full_ident(&successor.path)
                                ),
                            )
                            .to_compile_error();
                            tokens_to_append.push(parse_quote! {
                                _ => #error,
                            });
                            break;
                        }
                    }
                    (syn::Pat::Ident(prev), syn::Pat::Ident(current)) => {
                        if &prev.ident > &current.ident {
                            let successor = patterns_so_far
                                .iter()
                                .map(|arm| {
                                    if let syn::Pat::Ident(pat) = arm {
                                        pat
                                    } else {
                                        unreachable!()
                                    }
                                })
                                .find(|pat| pat.ident > current.ident)
                                .unwrap();
                            let error = syn::Error::new_spanned(
                                &current.ident,
                                format!(
                                    "{} should sort before {}",
                                    &current.ident, &successor.ident
                                ),
                            )
                            .to_compile_error();
                            tokens_to_append.push(parse_quote! {
                                _ => #error,
                            });
                            break;
                        }
                    }
                    _ => {
                        let error = syn::Error::new_spanned(
                            &match_expr.arms[i - 1].pat,
                            format!("unsupported by #[sorted]",),
                        )
                        .to_compile_error();
                        tokens_to_append.push(parse_quote! {
                            _ => #error,
                        });
                        break;
                    }
                }
            }
            if let Some(pos) = match_expr
                .arms
                .iter()
                .position(|arm| matches!(arm.pat, syn::Pat::Wild(_)))
            {
                if pos != match_expr.arms.len() - 1 {
                    let error = syn::Error::new_spanned(
                        &match_expr.arms[pos].pat,
                        format!("wildcard patterns should appear last",),
                    )
                    .to_compile_error();
                    tokens_to_append.push(parse_quote! {
                        _ => #error,
                    });
                }
            }
            if !tokens_to_append.is_empty() {
                match_expr.arms.extend(tokens_to_append);
            }
        }
        syn::visit_mut::visit_expr_match_mut(self, match_expr);
    }
}

#[proc_macro_attribute]
pub fn check(_: TokenStream, input: TokenStream) -> TokenStream {
    let mut item = syn::parse_macro_input!(input as syn::ItemFn);
    let mut branches_sorter = MatchBranchesSorter;
    branches_sorter.visit_item_fn_mut(&mut item);
    item.to_token_stream().into()
}
