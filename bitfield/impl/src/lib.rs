use proc_macro::TokenStream;
use proc_macro2 as pm2;
use quote::{format_ident, quote, quote_spanned};
use syn::{parse_macro_input, Item};

#[proc_macro]
pub fn define_mod8_enums_and_n_types(_tt: TokenStream) -> TokenStream {
    // https://users.rust-lang.org/t/passing-computed-constants-to-procmacro/36466/2
    let mut output = pm2::TokenStream::new();
    output.extend(quote! {
        pub trait TotalSizeIsMultipleOfEightBits {}
        pub trait HelperTrait {
            type Type;
        }
        pub struct N<const N: usize>;
    });
    for i in 0..8usize {
        let prefix = match i {
            0 => "Zero",
            1 => "One",
            2 => "Two",
            3 => "Three",
            4 => "Four",
            5 => "Five",
            6 => "Six",
            7 => "Seven",
            _ => unreachable!(),
        };
        let ident = format_ident!("{prefix}Mod8");
        output.extend(quote! {
            pub enum #ident {}
        });

        let i = syn::Index::from(i);
        output.extend(quote! {
            impl HelperTrait for N<#i> {
                type Type = crate::checks::#ident;
            }
        })
    }

    output.into()
}

fn make_getter_token_stream(
    field_ident: pm2::Ident,
    ty_ident: pm2::Ident,
    offset_bits_token_stream: pm2::TokenStream,
    expected_size_bits: Option<syn::LitInt>,
) -> pm2::TokenStream {
    let size_bits_token_stream = quote! { <#ty_ident as Specifier>::BITS };
    let getter_name = format_ident!("get_{}", field_ident);
    let return_type = quote! { <#ty_ident as Specifier>::ValueType };
    let mut size_check = quote! {};
    if let Some(expected_size_bits) = expected_size_bits {
        size_check = quote_spanned! {
            expected_size_bits.span() =>
            let _: [(); #expected_size_bits] = [(); #size_bits_token_stream];
        }
    }
    quote! {
        pub fn #getter_name(&self) -> #return_type {
            #size_check
            let mut offset_bits = #offset_bits_token_stream;
            let mut offset_within_the_byte = offset_bits % 8;
            let mut current_byte = offset_bits / 8;
            let mut size_bits = #size_bits_token_stream as usize;

            <#ty_ident as Specifier>::from_usize(if size_bits + offset_within_the_byte <= 8 {
                let shiftr = (8 - offset_within_the_byte - size_bits) as u8;
                let mask = ((1 << size_bits) - 1) as u8;
                (self.data[current_byte] >> shiftr & mask) as usize
            } else {
                let mut result: u128 = 0;
                while offset_within_the_byte + size_bits > 8 {
                    let size_within_the_byte = 8 - offset_within_the_byte;
                    let mask = ((1u16 << size_within_the_byte) - 1) as u8;
                    result = (result.wrapping_shl(size_within_the_byte as u32)) | (self.data[current_byte] & mask) as u128;
                    size_bits -= size_within_the_byte;
                    offset_bits += size_within_the_byte;
                    offset_within_the_byte = offset_bits % 8;
                    current_byte = (offset_bits + 1) / 8;
                }
                let mask = ((1u16 << size_bits) - 1) as u8;
                result = (result.wrapping_shl(size_bits as u32)) | ((self.data[current_byte] >> (8 - size_bits)) & mask) as u128;
                result as usize
            })
        }
    }
}

fn make_setter_token_stream(
    field_ident: pm2::Ident,
    ty_ident: pm2::Ident,
    offset_bits_token_stream: pm2::TokenStream,
) -> pm2::TokenStream {
    let size_bits_token_stream = quote! { <#ty_ident as Specifier>::BITS };
    let setter_name = format_ident!("set_{}", field_ident);
    let uint_type = quote! { <BITS<{#size_bits_token_stream}> as SmallestUnsignedType>::Type };
    let arg_type = quote! { <#ty_ident as Specifier>::ValueType };
    quote! {
        pub fn #setter_name(&mut self, val: #arg_type) {
            let val = val as #uint_type;
            let uint_size = <#uint_type>::BITS as usize;
            let mut offset_bits = #offset_bits_token_stream;
            let mut offset_within_the_byte = offset_bits % 8;
            let mut current_data_byte = offset_bits / 8;
            let mut size_bits = #size_bits_token_stream as usize;
            if size_bits + offset_within_the_byte <= 8 {
                let shiftl = (8 - offset_within_the_byte - size_bits) as u8;
                let mask = ((1 << size_bits) - 1) as u8;
                let clear_mask = !(mask << shiftl);
                self.data[current_data_byte] &= clear_mask;
                self.data[current_data_byte] |=  (val as u8 & mask) << shiftl;
            } else {
                let mut bits_to_extract = 8 - offset_within_the_byte;
                let mut offset_within_the_val = uint_size - size_bits;
                while size_bits > 0 {
                    let shift = uint_size - offset_within_the_val - bits_to_extract;
                    let mask = (1 << bits_to_extract) - 1;
                    let value = (val >> shift) & mask;
                    let shiftl = 8 - offset_within_the_byte - bits_to_extract;
                    self.data[current_data_byte] &= !(mask as u8);
                    self.data[current_data_byte] |= ((value & mask) << shiftl) as u8;
                    size_bits -= bits_to_extract;
                    offset_within_the_val += bits_to_extract;
                    offset_bits += bits_to_extract;
                    current_data_byte = (offset_bits + 1) / 8;
                    offset_within_the_byte = offset_bits % 8;
                    bits_to_extract = (8 - offset_within_the_byte).min(size_bits) as usize;
                }
            }
        }
    }
}

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let input_item = parse_macro_input!(input as Item);
    let total_bits = total_bits_as_token_stream(&input_item);
    let ident = get_ident(&input_item);
    let broken_down_fields = break_fields_down_token_streams(&input_item);
    let mut getters = Vec::new();
    let mut setters = Vec::new();
    for (field_ident, type_ident, offset_bits, expected_size_bits) in broken_down_fields {
        getters.push(make_getter_token_stream(
            field_ident.clone(),
            type_ident.clone(),
            offset_bits.clone(),
            expected_size_bits.clone(),
        ));
        setters.push(make_setter_token_stream(
            field_ident.clone(),
            type_ident.clone(),
            offset_bits.clone(),
        ));
    }
    let output = quote! {
        #[repr(C)]
        pub struct #ident {
            data: [u8; (#total_bits)/8],
        }

        impl #ident
        where <Self as Specifier>::SizeModEight : bitfield::checks::TotalSizeIsMultipleOfEightBits {
            pub fn new() -> Self {
                Self {
                    data: [0; {(#total_bits)/8}],
                }
            }
            #(#getters)*
            #(#setters)*
        }

        impl Specifier for #ident {
            const BITS: usize = #total_bits;
            type SizeModEight = <bitfield::checks::N<{(#total_bits) % 8}> as bitfield::checks::HelperTrait>::Type;
            type ValueType = u128;
            fn from_usize(value: usize) -> Self::ValueType {
                value as Self::ValueType
            }
        }
    };
    output.into()
}

#[proc_macro]
pub fn make_field_size_helpers(_: TokenStream) -> TokenStream {
    let mut output = quote! {
        pub struct BITS<const BITS: usize> {}
        pub trait SmallestUnsignedType {
            type Type;
        }
    };
    for i in 1..=64u8 {
        let field_size_bits = 8u8.max(1 << (8 - (1u8.max(i - 1)).leading_zeros()));
        let i = syn::Index::from(i as usize);
        let uint_type = format_ident!("u{}", field_size_bits);
        output.extend(quote! {
            impl SmallestUnsignedType for BITS<#i> {
                type Type = #uint_type;
            }
        });
    }
    output.into()
}

fn break_fields_down_token_streams(
    item: &Item,
) -> Vec<(
    pm2::Ident,
    pm2::Ident,
    pm2::TokenStream,
    Option<syn::LitInt>,
)> {
    let mut broken_down_fields = Vec::new();
    let mut offset = quote! { 0 };
    let Item::Struct(s) = item else {
        panic!("Only structs and enums are supported")
    };
    for field in &s.fields {
        if let syn::Type::Path(ty) = &field.ty {
            if let Some(ty_ident) = ty.path.get_ident() {
                let expected_field_size_bits = extract_expected_field_size_bits(&field.attrs);
                broken_down_fields.push((
                    field.ident.clone().unwrap(),
                    ty_ident.clone(),
                    offset.clone(),
                    expected_field_size_bits,
                ));
                offset.extend(quote! { + <#ty_ident as Specifier>::BITS });
            }
        }
    }
    broken_down_fields
}

fn extract_expected_field_size_bits(attrs: &[syn::Attribute]) -> Option<syn::LitInt> {
    if let Some(bits_attr) = attrs
        .iter()
        .find(|attr| attr.path().is_ident("bits") && matches!(attr.meta, syn::Meta::NameValue(_)))
    {
        let syn::Meta::NameValue(meta) = &bits_attr.meta else {
            panic!("Expected a name-value attribute")
        };
        if let syn::Expr::Lit(lit) = &meta.value {
            if let syn::Lit::Int(int) = &lit.lit {
                return Some(int.clone());
            }
        }
    }
    None
}

fn total_bits_as_token_stream(item: &Item) -> pm2::TokenStream {
    let Item::Struct(s) = item else {
        panic!("Only structs are supported")
    };
    let mut total = quote! { 0 };
    for field in &s.fields {
        if let syn::Type::Path(ty) = &field.ty {
            if let Some(ident) = ty.path.get_ident() {
                total.extend(quote! { + <#ident as Specifier>::BITS });
            }
        }
    }
    total
}

fn get_ident(item: &Item) -> pm2::Ident {
    match item {
        Item::Struct(s) => s.ident.clone(),
        Item::Enum(e) => e.ident.clone(),
        _ => panic!("Only structs and enums are supported"),
    }
}

#[proc_macro]
pub fn define_specifiers(_input: TokenStream) -> TokenStream {
    let mut output = pm2::TokenStream::new();
    for i in 1..=64usize {
        let specifier = format_ident!("B{}", i);
        output.extend(quote! {
            pub enum #specifier {}
            impl Specifier for #specifier {
                const BITS: usize = #i;
                type SizeModEight = <crate::checks::N<{#i % 8}> as crate::checks::HelperTrait>::Type;
                type ValueType = <BITS<{#i}> as SmallestUnsignedType>::Type;
                fn from_usize(value: usize) -> Self::ValueType {
                    value as Self::ValueType
                }
            }
        });
    }
    output.into()
}

#[proc_macro_derive(BitfieldSpecifier)]
pub fn derive_bitfield_specifier(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    let ident = input.ident;
    let variants = match input.data {
        syn::Data::Enum(e) => e.variants,
        _ => panic!("Only enums are supported"),
    };
    if !variants.len().is_power_of_two() {
        return quote! {
            compile_error!("BitfieldSpecifier expected a number of variants which is a power of 2");
        }
        .into();
    }
    let size_bits = variants.len().ilog2() as usize;
    let max_value = 1 << size_bits;
    let variants_as_usize = variants.iter().enumerate().map(|(i, variant)| {
        let variant_ident = &variant.ident;
        let var_ident = format_ident!("var{}", i);
        if let Some((_, expr)) = &variant.discriminant {
            if let syn::Expr::Lit(lit) = expr {
                if let syn::Lit::Int(discriminant) = &lit.lit {
                    return quote_spanned! {
                        variant_ident.span() =>
                        let _ : &dyn crate::checks::DiscriminantInRange = &(<crate::checks::EvaluateDiscriminantInRange<{#discriminant < #max_value}> as crate::checks::ConvertToCheck>::Result::new());
                        let #var_ident = #discriminant
                    };
                }
            }
            quote_spanned! {
                variant_ident.span() =>
                let #var_ident = Self::#variant_ident as usize;
                let _ : &dyn crate::checks::DiscriminantInRange = &(<crate::checks::EvaluateDiscriminantInRange<{(Self::#variant_ident as i32) < #max_value}> as crate::checks::ConvertToCheck>::Result::new());
            }
        } else {
            quote_spanned! {
                variant_ident.span() =>
                let #var_ident = Self::#variant_ident as usize;
                let _ : &dyn crate::checks::DiscriminantInRange = &(<crate::checks::EvaluateDiscriminantInRange<{(Self::#variant_ident as i32) < #max_value}> as crate::checks::ConvertToCheck>::Result::new());
            }
        }
    });
    let match_arms = variants.iter().enumerate().map(|(i, variant)| {
        let var_ident = format_ident!("var{}", i);
        let variant_ident = &variant.ident;
        quote! {
            x if x == #var_ident => Self::#variant_ident
        }
    });
    quote! {
        impl Specifier for #ident {
            const BITS: usize = #size_bits;
            type SizeModEight = <crate::checks::N<{#size_bits % 8}> as crate::checks::HelperTrait>::Type;
            type ValueType = Self;
            fn from_usize(value: usize) -> Self::ValueType {
                #(#variants_as_usize;)*
                match value {
                    #(#match_arms,)*
                    _ => panic!("Unrecognized value")
                }
            }
        }
    }.into()
}
