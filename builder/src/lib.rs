use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse, AngleBracketedGenericArguments, Data, DataStruct, DeriveInput, Error, Expr, ExprLit,
    Fields, GenericArgument, Ident, Lit, MetaNameValue, Path, PathArguments, PathSegment, Type,
    TypePath,
};

enum Repeated {
    NotRepeated,
    Repeated(Ident, Type),
}

struct FieldData {
    ident: Ident,
    ty: Type,
    optional: bool,
    repeated: Repeated,
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let Ok(input) = parse::<DeriveInput>(input) else {
        return quote! {
            compile_error!("Couldn't parse the item after derive(Builder) as a DeriveInput")
        }
        .into();
    };
    let type_name = input.ident;
    let builder_type_name = Ident::new(&format!("{type_name}Builder"), Span::call_site());
    let Data::Struct(DataStruct {
        fields: Fields::Named(named_fields),
        ..
    }) = input.data
    else {
        return quote! {
            compile_error!("Couldn't find named fields in the given struct")
        }
        .into();
    };
    let fields = parse_field_data(named_fields);
    for field in fields.iter() {
        if field.is_err() {
            return field
                .as_ref()
                .err()
                .unwrap()
                .clone()
                .to_compile_error()
                .into();
        }
    }
    let fields: Vec<_> = fields.into_iter().map(Result::unwrap).collect();
    let fields_types = fields
        .iter()
        .map(|field| {
            let ident = field.ident.clone();
            let ty = field.ty.clone();
            quote! {
                #ident: ::std::option::Option<#ty>
            }
        })
        .collect::<Vec<_>>();
    let setters =
        fields
            .iter()
            .map(|field| {
                let ident = field.ident.clone();
                let ty = field.ty.clone();
                let main_tokens = quote! {
                    fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = Some(#ident);
                        self
                    }
                };
                let mut items = match &field.repeated {
                    Repeated::Repeated(repeated_ident, _)
                        if repeated_ident.to_string() != ident.to_string() =>
                    {
                        main_tokens
                    }
                    Repeated::NotRepeated => main_tokens.into(),
                    _ => quote! {},
                };
                if let Repeated::Repeated(repeated_ident, repeated_ty) = &field.repeated {
                    items.extend([quote! {
                    fn #repeated_ident(&mut self, #repeated_ident: #repeated_ty) -> &mut Self {
                        if self.#ident.is_none() {
                            self.#ident = Some(vec![])
                        }
                        self.#ident.as_mut().unwrap().push(#repeated_ident);
                        self
                    }
                }].into_iter())
                }
                items
            })
            .collect::<Vec<_>>();

    let build_method_set_fields: Vec<_> = fields
        .iter()
        .map(|field| {
            let ident = field.ident.clone();
            let error_msg = format!("Required field '{}' is not set", ident);
            if let Repeated::Repeated(_, _) = field.repeated {
                return quote! {
                    let #ident = if let Some(ref #ident) = self.#ident {
                        #ident
                    } else { &vec![] };
                };
            }
            if field.optional {
                quote! {
                    let #ident = self.#ident.clone();
                }
            } else {
                quote! {
                    let Some(ref #ident) = self.#ident else { return Err(#error_msg.into()) };
                }
            }
        })
        .collect();
    let fields_idents: Vec<Ident> = fields.into_iter().map(|field| field.ident).collect();
    let build_method = quote! {
        pub fn build(&mut self) -> ::std::result::Result<#type_name,::std::boxed::Box<dyn std::error::Error>> {
            #(#build_method_set_fields)*
            ::std::result::Result::Ok(#type_name {
                #(#fields_idents: #fields_idents.clone()),*
            })
        }
    };
    let tokens = quote! {
        pub struct #builder_type_name {
            #(#fields_types),*
        }
        impl #type_name {
            pub fn builder() -> #builder_type_name {
                #builder_type_name {
                    #(#fields_idents: ::std::option::Option::None),*
                }
            }
        }
        impl #builder_type_name {
            #(#setters)*
            #build_method
        }
    }
    .into();
    //eprintln!("TOKENS: {}", tokens);
    tokens
}

fn parse_field_data(named_fields: syn::FieldsNamed) -> Vec<Result<FieldData, Error>> {
    named_fields
        .named
        .into_iter()
        .map(|field| {
            let field_ident = field.ident.unwrap().clone();
            let mut ty = field.ty.clone();
            let mut optional = false;
            let mut repeated = Repeated::NotRepeated;
            if let Type::Path(TypePath {
                qself: None,
                path: Path { segments, .. },
            }) = &field.ty
            {
                if let PathSegment {
                    ident,
                    arguments:
                        PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
                    ..
                } = segments.first().unwrap()
                {
                    if ident.to_string() == "Option" {
                        if let GenericArgument::Type(inner_ty) = args.first().unwrap() {
                            optional = true;
                            ty = inner_ty.clone();
                        }
                    }
                }
            }
            for attr in field.attrs {
                if !attr.path().is_ident("builder") {
                    continue;
                }
                let tokens =
                    parse::<MetaNameValue>(attr.meta.require_list().unwrap().tokens.clone().into())
                        .unwrap();
                if tokens.path.is_ident("each") {
                    if let Expr::Lit(ExprLit {
                        lit: Lit::Str(lit), ..
                    }) = tokens.value
                    {
                        let repeated_ident = Ident::new(&lit.value(), Span::call_site());
                        if let Type::Path(TypePath { path, .. }) = &ty {
                            let first_segment = path.segments.first().unwrap();
                            if first_segment.ident != "Vec" {
                                continue;
                            }
                            if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                                args,
                                ..
                            }) = &first_segment.arguments
                            {
                                if let GenericArgument::Type(inner_ty) = args.first().unwrap() {
                                    repeated = Repeated::Repeated(
                                        repeated_ident.clone(),
                                        inner_ty.clone(),
                                    );
                                }
                            }
                        }
                    }
                } else {
                    return Err(Error::new_spanned(
                        attr.meta,
                        "expected `builder(each = \"...\")`",
                    ));
                }
            }
            Ok(FieldData {
                ident: field_ident,
                ty,
                optional,
                repeated,
            })
        })
        .collect()
}
