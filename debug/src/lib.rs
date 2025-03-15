use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote, DeriveInput, Expr, ExprLit, Lit, Meta, MetaNameValue, Token,
    WherePredicate,
};

#[derive(Debug)]
struct UserProvidedBound {
    _bound_token: syn::Ident,
    _eq: Token![=],
    predicate: syn::WherePredicate,
}

impl Parse for UserProvidedBound {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let _bound_token: syn::Ident = input.parse()?;
        if _bound_token.to_string() != "bound" {
            return Err(syn::Error::new(_bound_token.span(), "expected `bound`"));
        }
        let _eq = input.parse()?;
        let bound_literal = input.parse::<proc_macro2::Literal>()?;
        let bound_string = bound_literal.to_string();
        let predicate =
            if let Ok(predicate) = syn::parse_str(&bound_string[1..&bound_string.len() - 1]) {
                predicate
            } else {
                return Err(syn::Error::new_spanned(
                    bound_literal,
                    "malformed type bound",
                ));
            };
        Ok(UserProvidedBound {
            _bound_token,
            _eq,
            predicate,
        })
    }
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let mut derive_input = parse_macro_input!(input as DeriveInput);
    let item_ident = derive_input.ident;
    let item_ident_str = item_ident.to_string();
    let data = derive_input.data;
    let mut user_provided_bound: Option<WherePredicate> = None;
    if let Some(attr) = derive_input.attrs.first() {
        if let syn::Meta::List(syn::MetaList { path, tokens, .. }) = &attr.meta {
            if let Some(ident) = path.get_ident() {
                if ident.to_string() == "debug" {
                    match syn::parse2::<UserProvidedBound>(tokens.clone()) {
                        Ok(_user_provided_bound) => {
                            user_provided_bound = Some(_user_provided_bound.predicate)
                        }
                        Err(err) => return err.to_compile_error().into(),
                    }
                }
            }
        }
    }
    let fields = match data {
        syn::Data::Struct(s) => s.fields,
        _ => {
            return quote! {
                compile_error!("CustomDebug only supports structs")
            }
            .into();
        }
    };
    let fields_tokens = fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        let field_ident_str = field_ident.to_string();
        if let Some(attr) = field
            .attrs
            .iter()
            .find(|attr| attr.path().is_ident("debug"))
        {
            if let Meta::NameValue(MetaNameValue {
                value:
                    Expr::Lit(ExprLit {
                        lit: Lit::Str(lit_str),
                        ..
                    }),
                ..
            }) = &attr.meta
            {
                let format_str = lit_str.value();
                quote! {
                    .field(#field_ident_str, &::std::format_args!(#format_str, &self.#field_ident))
                }
            } else {
                quote! {
                    .field(#field_ident_str, &self.#field_ident)
                }
            }
        } else {
            quote! {
                .field(#field_ident_str, &self.#field_ident)
            }
        }
    });

    let mut phantomized_type_parameter_idents: HashSet<String> = HashSet::new();
    let ty_params_types_idents: HashSet<String> = derive_input
        .generics
        .type_params()
        .map(|param| param.ident.to_string())
        .collect();
    let mut paths_with_bounds = vec![];
    for ty in fields.iter().map(|f| &f.ty) {
        if let syn::Type::Path(syn::TypePath {
            path: syn::Path { segments, .. },
            ..
        }) = ty
        {
            if segments.len() == 1 {
                if let syn::PathSegment {
                    ident,
                    arguments:
                        syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                            args,
                            ..
                        }),
                } = segments.first().unwrap()
                {
                    if ident.to_string() == "PhantomData" {
                        if args.len() == 1 {
                            if let syn::GenericArgument::Type(syn::Type::Path(syn::TypePath {
                                path,
                                ..
                            })) = args.first().unwrap()
                            {
                                if path.segments.len() == 1 {
                                    if let syn::PathSegment {
                                        ident,
                                        arguments: syn::PathArguments::None,
                                    } = path.segments.first().unwrap()
                                    {
                                        if ty_params_types_idents.contains(&ident.to_string()) {
                                            phantomized_type_parameter_idents
                                                .insert(ident.to_string());
                                            continue;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if args.len() == 1 {
                        if let syn::GenericArgument::Type(syn::Type::Path(syn::TypePath {
                            path,
                            ..
                        })) = args.first().unwrap()
                        {
                            if path.segments.len() > 1 {
                                if let syn::PathSegment {
                                    ident,
                                    arguments: syn::PathArguments::None,
                                } = path.segments.first().unwrap()
                                {
                                    if ty_params_types_idents.contains(&ident.to_string()) {
                                        phantomized_type_parameter_idents.insert(ident.to_string());
                                        paths_with_bounds.push(path.clone());
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    if user_provided_bound.is_none() {
        for param in derive_input.generics.type_params_mut() {
            if !phantomized_type_parameter_idents.contains(&param.ident.to_string()) {
                param.bounds.push(parse_quote!(::std::fmt::Debug));
            }
        }
    }
    let (impl_generics, ty_generics, where_clause) = derive_input.generics.split_for_impl();

    let mut where_clause = where_clause.cloned();
    if user_provided_bound.is_none() {
        for path in paths_with_bounds {
            if let Some(ref mut where_predicates) = where_clause {
                where_predicates
                    .predicates
                    .push(parse_quote!(#path: ::std::fmt::Debug));
            } else {
                where_clause = Some(parse_quote!(where #path: ::std::fmt::Debug));
            }
        }
    } else {
        where_clause = Some(parse_quote!(where #user_provided_bound));
    }
    let output = quote! {
        impl #impl_generics ::std::fmt::Debug for #item_ident #ty_generics #where_clause {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                fmt.debug_struct(#item_ident_str)
                    #(#fields_tokens)*
                    .finish()
            }
        }
    };
    output.into()
}
