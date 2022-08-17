use proc_macro2::{Ident, Span};
use quote::{quote, ToTokens};
use std::ops::Range;
use syn::{parse::Parse, Expr, ExprMatch};

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

impl Type {
    fn type_name(&self) -> proc_macro2::TokenStream {
        match self {
            Type::U8 => quote!(u8),
            Type::U16 => quote!(u16),
            Type::U32 => quote!(u32),
            Type::U64 => quote!(u64),
            Type::U128 => quote!(u128),
        }
    }
}

struct Var {
    range: Range<usize>,
    ident: char,
}

enum Arm {
    Pattern {
        kind: Type,
        condition_mask: u128,
        condition_val: u128,
        vars: Vec<Var>,
        body: Expr,
    },
    Wild {
        body: Expr,
    },
}

struct Match {
    arms: Vec<Arm>,
    input_expr: Expr,
}

impl Parse for Match {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut arms = vec![];
        let mexpr = input.parse::<ExprMatch>()?;
        for arm in mexpr.arms {
            arms.push(Arm::from_match_arm(arm));
        }
        Ok(Self {
            arms,
            input_expr: *mexpr.expr,
        })
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

impl Arm {
    fn from_match_arm(arm: syn::Arm) -> Self {
        let string = match &arm.pat {
            syn::Pat::Lit(patlit) => match &*patlit.expr {
                Expr::Lit(expr) => match &expr.lit {
                    syn::Lit::Str(lit_str) => lit_str.value(),
                    _ => panic!("only accept string literals"),
                },
                _ => panic!("Unable to take in any expressions apart from a string literal"),
            },
            syn::Pat::Wild(_) => return Self::Wild { body: *arm.body },
            _ => panic!("requires match arm to be a string literal or wild (_)"),
        };
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
            panic!("string larger than any integer type can fit");
        };
        let mut iter = string.chars().rev().peekable();
        let mut vars = vec![];
        let mut mask = 0;
        let mut val = 0;
        let mut index = 0;
        while let Some(c) = iter.next() {
            let bit = 1 << index;
            let mut inc_index = true;
            match c {
                '0' => mask |= bit,
                '1' => {
                    mask |= bit;
                    val |= bit
                }
                '?' => {}
                '_' => inc_index = false,
                'a'..='z' => {
                    let begin = index;
                    let ident = c;
                    let mut size = 1;
                    if vars.iter().find(|var: &&Var| var.ident == ident).is_some() {
                        panic!("duplicate variables");
                    }
                    while let Some(peek) = iter.peek() {
                        if *peek == ident {
                            size += 1;
                            iter.next();
                        } else if *peek == '_' {
                            iter.next();
                            continue;
                        } else {
                            break;
                        }
                    }
                    index += size;
                    inc_index = false;
                    vars.push(Var {
                        ident,
                        range: begin..begin + size,
                    })
                }
                _ => panic!("invalid character in bitmatch string pattern"),
            }
            if inc_index {
                index += 1;
            }
        }
        let body = *arm.body;
        Self::Pattern {
            body,
            condition_mask: mask,
            condition_val: val,
            vars,
            kind,
        }
    }
}

impl ToTokens for Arm {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.extend(match self {
            Arm::Pattern {
                kind,
                condition_mask,
                condition_val,
                vars,
                body,
            } => {
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
                let kind_quote = kind.type_name();
                let var_initalization: Vec<proc_macro2::TokenStream> = vars
                    .iter()
                    .map(|var| {
                        let shft = var.range.start;
                        let mask = (1u128 << var.range.len()) - 1;
                        let ident = Ident::new(&String::from(var.ident), Span::call_site());
                        quote! {
                            let #ident = ((_bitmatch as #kind_quote) >> #shft) & (#mask as #kind_quote);
                        }
                    })
                    .collect();
                quote! {
                    _bitmatch if (_bitmatch as #kind_quote) & #condition_mask_quote == #condition_val_quote => {
                        #(#var_initalization)*
                        #body
                    }
                }
            }
            Arm::Wild { body } => {
                quote! {
                    _ => #body
                }
            }
        });
    }
}
