use proc_macro2::{Ident, Span};
use quote::{quote, ToTokens};
use std::ops::Range;
use syn::{
    parse::{Parse, ParseStream},
    Expr, LitStr, Token,
};

#[proc_macro]
pub fn bitmatch(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parse = syn::parse::<Match>(stream).expect("failed to parse inner match");
    quote!(#parse).into()
}

enum Type {
    U8,
    U16,
    U32,
    U64,
    U128,
}

struct Var {
    range: Range<usize>,
    ident: char,
}

struct Arm {
    kind: Type,
    condition_mask: u128,
    condition_val: u128,
    vars: Vec<Var>,
    body: Expr,
}

struct Match {
    arms: Vec<Arm>,
    input_expr: Expr,
}

impl Parse for Match {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut arms = vec![];
        input.parse::<Token![match]>()?;
        let input_expr = input.parse::<Expr>()?;
        let content;
        syn::braced!(content in input);
        while !content.is_empty() {
            let arm = input.parse::<Arm>()?;
            arms.push(arm);
        }
        Ok(Self { arms, input_expr })
    }
}

impl ToTokens for Match {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let arms = &self.arms;
        let input_expr = &self.input_expr;
        tokens.extend(quote! {
            match #input_expr{
                #(#arms),*
            }
        });
    }
}

impl Parse for Arm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let string = input
            .parse::<LitStr>()
            .or_else(|_| Err(input.error("failed to parse arm as string")))?;
        let string = string.value();
        let string_len = string.len();
        let kind = if string_len <= 8 {
            Type::U8
        } else if string_len <= 16 {
            Type::U16
        } else if string_len <= 32 {
            Type::U32
        } else if string_len <= 64 {
            Type::U64
        } else if string_len <= 128 {
            Type::U128
        } else {
            return Err(input.error("string larger than any integer type can fit"));
        };
        let mut iter = string.chars().peekable();
        let mut vars = vec![];
        let mut mask = 0;
        let mut val = 0;
        let mut index = 0;
        while let Some(c) = iter.next() {
            let bit = 1 << index;
            index += 1;
            match c {
                '0' => mask |= bit,
                '1' => {
                    mask |= bit;
                    val |= bit
                }
                '?' => {}
                '_' => index -= 1,
                'a'..='z' => {
                    let begin = index;
                    let ident = c;
                    let mut size = 1;
                    if vars.iter().find(|var: &&Var| var.ident == ident).is_some() {
                        return Err(input.error("duplicate variables"));
                    }
                    while let Some(peek) = iter.peek() {
                        if *peek == ident {
                            size += 1;
                        } else {
                            break;
                        }
                    }
                    vars.push(Var {
                        ident,
                        range: begin..begin + size,
                    })
                }
                _ => return Err(input.error("invalid character in bitmatch string pattern")),
            }
        }
        input.parse::<Token![=>]>()?;
        let body = input.parse::<Expr>()?;
        let _ = input.parse::<Token![,]>();
        Ok(Self {
            body,
            condition_mask: mask,
            condition_val: val,
            vars,
            kind,
        })
    }
}

impl ToTokens for Arm {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let Arm {
            kind,
            condition_mask,
            condition_val,
            vars,
            body,
            ..
        } = self;
        let condition_mask = *condition_mask;
        let condition_val = *condition_val;
        let (condition_val_quote, condition_mask_quote) = match kind {
            Type::U8 => {
                let val = condition_val as u8;
                let mask = condition_mask as u8;
                (quote! {#val}, quote!(#mask))
            }
            Type::U16 => {
                let val = condition_val as u16;
                let mask = condition_mask as u16;
                (quote! {#val}, quote!(#mask))
            }
            Type::U32 => {
                let val = condition_val as u32;
                let mask = condition_mask as u32;
                (quote! {#val}, quote!(#mask))
            }
            Type::U64 => {
                let val = condition_val as u64;
                let mask = condition_mask as u64;
                (quote! {#val}, quote!(#mask))
            }
            Type::U128 => (quote! {#condition_val}, quote! {#condition_mask}),
        };
        let var_initalization: Vec<proc_macro2::TokenStream> = vars
            .iter()
            .map(|var| {
                let shft = var.range.start;
                let mask = (1 << var.range.len()) - 1;
                let ident = Ident::new(&String::from(var.ident), Span::call_site());
                quote! {
                    let #ident = (_bitmatch >> #shft) & #mask
                }
            })
            .collect();
        tokens.extend(quote! {
            _bitmatch if _bitmatch & #condition_mask_quote == #condition_val_quote => {
                #(#var_initalization);*
                #body
            }
        });
    }
}
