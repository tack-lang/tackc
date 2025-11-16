use quote::{format_ident, quote};
use syn::{
    Data, DataStruct, DeriveInput, FnArg, GenericParam, Generics, Ident, LitInt, parse_macro_input,
    parse_quote,
};

#[proc_macro_attribute]
/// # Panics
/// This macro panics if being applied to a non-function. Methods do not work. To fuzz a method, create a function to fuzz outside of the impl block.
pub fn fuzz(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let loops = if attr.is_empty() {
        parse_quote! { 100 }
    } else {
        parse_macro_input!(attr as LitInt)
    };
    let input = parse_macro_input!(item as syn::ItemFn);
    let ident = &input.sig.ident;
    let arg_types = input
        .sig
        .inputs
        .iter()
        .map(|arg| match arg {
            FnArg::Receiver(_) => panic!(
                "#[fuzz] doesn't work for methods! Create a function outside of the impl block."
            ),
            FnArg::Typed(ty) => &ty.ty,
        })
        .cloned();
    let arg_types2 = arg_types.clone();

    let fuzz_ident = format_ident!("__{ident}_fuzz");

    quote! {
        #input

        #[test]
        fn #fuzz_ident()
        where #(#arg_types: ::tackc_macros::Random),* {
            for _ in 0..#loops {
                #ident(#(<#arg_types2 as ::tackc_macros::Random>::random()),*);
            }
        }
    }
    .into()
}

#[proc_macro_derive(Random)]
pub fn derive_random(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    match input.data {
        Data::Struct(struct_in) => {
            derive_random_struct_impl(struct_in, input.ident, input.generics)
        }
        _ => unimplemented!(
            "please implement this macro for the type of item you need for future use"
        ),
    }
    .into()
}

#[allow(clippy::needless_pass_by_value)]
fn derive_random_struct_impl(
    input: DataStruct,
    ident: Ident,
    generics: Generics,
) -> proc_macro2::TokenStream {
    let generic_types = generics.params.into_iter().map(|s| match s {
        GenericParam::Type(mut ty) => {
            ty.bounds.push(parse_quote! { ::tackc_macros::Random });
            GenericParam::Type(ty)
        }
        _ => s,
    });
    let where_clause = generics.where_clause;
    let types = input.fields.iter().map(|x| &x.ty);
    let random_assert = format_ident!("__random_assert_{ident}");
    let fields = input.fields.iter().map(|s| &s.ident);

    quote! {
        #[allow(non_snake_case)]
        fn #random_assert() where #(#types: ::tackc_macros::Random),* {}

        impl<#(#generic_types)*> ::tackc_macros::Random for #ident
        #where_clause
        {
            fn random() -> Self {
                #ident {
                    #(#fields: ::tackc_macros::Random::random()),*
                }
            }
        }
    }
}
