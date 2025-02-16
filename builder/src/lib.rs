use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};
use syn::spanned::Spanned;

/// Function to unwrap the Option type or return None
fn unwrap_type(wrapper_type: &str, ty: &syn::Type) -> Option<syn::Type> {
    if let syn::Type::Path(syn::TypePath { path, .. }) = ty {
        if path.segments.len() == 1 && path.segments[0].ident == wrapper_type {
            if let syn::PathArguments::AngleBracketed(args) = &path.segments[0].arguments {
                if let syn::GenericArgument::Type(ty) = args.args.first().unwrap() {
                    return Some(ty.clone());
                }
            }
        }
    }
    None
}

#[proc_macro_derive(Builder, attributes(builder))]
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
        if unwrap_type("Option", orig_type).is_some() {
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
    // Add attributes builders to the fields if they have the 'arg' attribute
    // and if the field is extendable (here it is a Vec)
    let mut builder_methods = Vec::new();

    for f in &input_fields.named {
        let ident = &f.ident;
        let orig_type = &f.ty;
        let mut has_attr_method = false;
        for attr in &f.attrs {
            let path = &attr.path();
            // Check for `builder(each = "...")` attribute
            if path.is_ident("builder") {
                let meta = attr.meta.clone();
                let meta_span = meta.span();
                if let syn::Meta::List(meta) = meta {
                    let tokens = meta.tokens.clone();
                    let mut tokens_iter = tokens.into_iter();
                    if let Some(proc_macro2::TokenTree::Ident(each)) = tokens_iter.next() {
                        let _punct = tokens_iter.next().unwrap();
                        let value = tokens_iter.next();
                        if each == "each" {
                            if let Some(proc_macro2::TokenTree::Literal(lit)) = value {
                                if let syn::Lit::Str(lit) = syn::Lit::new(lit) {
                                    let value = lit.value();
                                    let lit_span = lit.span();
                                    let arg = syn::Ident::new(&value, lit_span);
                                    let inner_type = unwrap_type("Vec", orig_type).unwrap();
                                    let val = quote::quote! {
                                        pub fn #arg(&mut self, #arg: #inner_type) -> &mut Self {
                                            if let std::option::Option::Some(ref mut vec) = self.#ident {
                                                vec.push(#arg);
                                            } else {
                                                self.#ident = std::option::Option::Some(vec![#arg]);
                                            }
                                            self
                                        }
                                    };
                                    // Prioritize attr_method, skip default builder method
                                    builder_methods.push(val);
                                    has_attr_method = true;
                                    break;
                                }
                            }
                        } else {
                            return syn::Error::new(meta_span, r#"`builder(each = "...")`"#)
                                .to_compile_error()
                                .into();
                        }
                    }
                }
            }
        }

        // If no attr_method was added, generate the usual builder method
        if !has_attr_method {
            if let Some(inner_type) = unwrap_type("Option", orig_type) {
                builder_methods.push(quote::quote! {
                    pub fn #ident(&mut self, #ident: #inner_type) -> &mut Self {
                        self.#ident = std::option::Option::Some(#ident);
                        self
                    }
                });
            } else {
                builder_methods.push(quote::quote! {
                    pub fn #ident(&mut self, #ident: #orig_type) -> &mut Self {
                        self.#ident = std::option::Option::Some(#ident);
                        self
                    }
                });
            }
        }
    }

    // Generate initializers for the build function
    let build_init = input_fields.named.iter().map(|f| {
        let ident = &f.ident;
        let orig_type = &f.ty;

        if unwrap_type("Option", orig_type).is_some() {
            quote::quote! {
                #ident: self.#ident.clone()
            }
        } else if unwrap_type("Vec", orig_type).is_some() {
            // If the field is a Vec, it is initialized to an empty Vec if it is None
            quote::quote! {
                #ident: self.#ident.clone().unwrap_or_else(Vec::new)
            }
        } else {
            quote::quote! {
                #ident: self.#ident.clone().ok_or(concat!(stringify!(#ident), " is required"))?
            }
        }
    });

    // Generate initializer for the builder that sets all fields to None
    // This is the default state of the builder
    let builder_init = input_fields.named.iter().map(|f| {
        let ident = &f.ident;
        quote::quote! {
            #ident: std::option::Option::None
        }
    });

    let stream = quote::quote! {
        pub struct #bident {
            #(#builder_fields,)*
        }

        impl #bident {
            #(#builder_methods)*

            pub fn build(&self) -> std::result::Result<#ident, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(#ident {
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
