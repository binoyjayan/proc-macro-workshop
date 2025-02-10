use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

/// Function to unwrap the Option type or return None
fn unwrap_option_type(ty: &syn::Type) -> Option<syn::Type> {
    if let syn::Type::Path(syn::TypePath { path, .. }) = ty {
        if path.segments.len() == 1 && path.segments[0].ident == "Option" {
            if let syn::PathArguments::AngleBracketed(args) = &path.segments[0].arguments {
                if let syn::GenericArgument::Type(ty) = args.args.first().unwrap() {
                    return Some(ty.clone());
                }
            }
        }
    }
    None
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let ident = &ast.ident;
    // Use span from the input to get the span of the builder
    // so the error messages related to the original struct are more helpful.
    let bident = syn::Ident::new(&format!("{}Builder", ident), ident.span());

    // Get the fields of the struct
    let input_fields = if let syn::Data::Struct(data) = &ast.data {
        if let syn::Fields::Named(fields) = &data.fields {
            fields
        } else {
            unimplemented!()
        }
    } else {
        unimplemented!()
    };

    // Wrap the fields in an Option type for the builder
    let builder_fields = input_fields.named.iter().map(|f| {
        let ident = &f.ident;
        let orig_type = &f.ty;
        if unwrap_option_type(orig_type).is_some() {
            quote::quote! {
                #ident: #orig_type
            }
        } else {
            quote::quote! {
                #ident: std::option::Option<#orig_type>
            }
        }
    });

    // Generate the setter methods for the builder
    let builder_methods = input_fields.named.iter().map(|f| {
        let ident = &f.ident;
        let orig_type = &f.ty;
        // Methods for the Option(al) types should accept the type itself
        if let Some(inner_type) = unwrap_option_type(orig_type) {
            quote::quote! {
                pub fn #ident(&mut self, #ident: #inner_type) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        } else {
            quote::quote! {
                pub fn #ident(&mut self, #ident: #orig_type) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        }
    });

    // Generate initializers for the build function
    let build_init = input_fields.named.iter().map(|f| {
        let ident = &f.ident;
        let orig_type = &f.ty;
        // Option(al) types should not fail the build
        if unwrap_option_type(orig_type).is_some() {
            return quote::quote! {
                #ident: self.#ident.clone()
            };
        }
        quote::quote! {
            #ident: self.#ident.clone().ok_or(concat!(stringify!(#ident), " is required"))?
        }
    });

    // Generate initializer for the builder that sets all fields to None
    // This is the default state of the builder
    let builder_init = input_fields.named.iter().map(|f| {
        let ident = &f.ident;
        quote::quote! {
            #ident: None
        }
    });

    let stream = quote::quote! {
        pub struct #bident {
            #(#builder_fields,)*
        }

        impl #bident {
            #(#builder_methods)*

            pub fn build(&self) -> Result<#ident, Box<dyn std::error::Error>> {
                Ok(#ident {
                    #(#build_init,)*
                })
            }
        }

        impl #ident {
            pub fn builder() -> #bident {
                #bident {
                    #(#builder_init,)*
                }
            }
        }
    };

    stream.into()
}
