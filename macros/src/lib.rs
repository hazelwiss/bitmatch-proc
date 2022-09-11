use proc_macro2::{Ident, Span};
use quote::{quote, ToTokens};
use std::ops::Range;
use syn::{parse::Parse, Expr, ExprMatch};

#[proc_macro]
pub fn bitmatch(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parse = syn::parse::<Match>(stream).expect("failed to parse inner match");
    quote!(#parse).into()
}

//enum Type {
//    U8,
//    U16,
//    U32,
//    U64,
//    U128,
//}
//
//impl Type {
//    fn type_name(&self) -> proc_macro2::TokenStream {
//        match self {
//            Type::U8 => quote!(u8),
//            Type::U16 => quote!(u16),
//            Type::U32 => quote!(u32),
//            Type::U64 => quote!(u64),
//            Type::U128 => quote!(u128),
//        }
//    }
//}

#[derive(PartialEq, Eq)]
struct Var {
    range: Range<usize>,
    ident: char,
}

enum ArmTy {
    Pattern { mask: u128, cmp: u128 },
    Wild,
    Or { masks: Vec<u128>, cmps: Vec<u128> },
}

impl ArmTy {
    pub fn from_pat(pat: syn::Pat, vars: &mut Vec<Var>) -> Self {
        match pat {
            syn::Pat::Lit(patlit) => match &*patlit.expr {
                Expr::Lit(expr) => match &expr.lit {
                    syn::Lit::Str(lit_str) => {
                        let string = lit_str.value();
                        let mut iter = string.chars().rev().peekable();
                        let mut new_vars = vec![];
                        let mut mask = 0;
                        let mut cmp = 0;
                        let mut index = 0;
                        while let Some(c) = iter.next() {
                            let bit = 1 << index;
                            let mut inc_index = true;
                            match c {
                                '0' => mask |= bit,
                                '1' => {
                                    mask |= bit;
                                    cmp |= bit
                                }
                                '?' => {}
                                '_' => inc_index = false,
                                'a'..='z' => {
                                    let begin = index;
                                    let ident = c;
                                    let mut size = 1;
                                    if new_vars
                                        .iter()
                                        .find(|var: &&Var| var.ident == ident)
                                        .is_some()
                                    {
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
                                    new_vars.push(Var {
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
                        if vars.is_empty() {
                            *vars = new_vars;
                        } else {
                            assert!(vars.len() == new_vars.len());
                            for new in new_vars {
                                assert!(vars.contains(&new));
                            }
                        }
                        Self::Pattern { mask, cmp }
                    }
                    _ => panic!("only accept string literals"),
                },
                _ => panic!("Unable to take in any expressions apart from a string literal"),
            },
            syn::Pat::Wild(_) => return Self::Wild,
            syn::Pat::Or(pat_or) => {
                let mut masks = vec![];
                let mut cmps = vec![];
                for pat in pat_or.cases {
                    let cur = Self::from_pat(pat, vars);
                    match cur {
                        ArmTy::Pattern { mask, cmp } => {
                            masks.push(mask);
                            cmps.push(cmp);
                        }
                        ArmTy::Wild => {
                            masks.clear();
                            masks.push(!0);
                            cmps.clear();
                            cmps.push(!0);
                            break;
                        }
                        ArmTy::Or { .. } => panic!("nested or pattern"),
                    }
                }
                Self::Or { masks, cmps }
            }
            _ => panic!("requires match arm to be a string literal or wild (_)"),
        }
    }
}

struct Arm {
    ty: ArmTy,
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
        let mut vars = vec![];
        let pattern = ArmTy::from_pat(arm.pat, &mut vars);
        let body = *arm.body;
        Self {
            body,
            vars,
            ty: pattern,
        }
    }
}

impl ToTokens for ArmTy {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.extend(match self {
            ArmTy::Wild => {
                quote! {
                    _
                }
            }
            ArmTy::Pattern { mask, cmp } => {
                let mask = syn::LitInt::new(&mask.to_string(), Span::call_site());
                let cmp = syn::LitInt::new(&cmp.to_string(), Span::call_site());
                let (cmp, mask) = (quote! {#cmp}, quote!(#mask));
                quote! {
                    _bitmatch if _bitmatch & #mask == #cmp
                }
            }
            ArmTy::Or { masks, cmps } => {
                let masks = masks
                    .iter()
                    .map(|mask| syn::LitInt::new(&mask.to_string(), Span::call_site()));
                let cmps = cmps
                    .iter()
                    .map(|cmp| syn::LitInt::new(&cmp.to_string(), Span::call_site()));
                quote! {
                    _bitmatch if #(_bitmatch & #masks == #cmps)&&*
                }
            }
        });
    }
}

impl ToTokens for Arm {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let body = &self.body;
        let ty = &self.ty;
        let vars = &self.vars;
        let var_initalization: Vec<proc_macro2::TokenStream> = vars
            .iter()
            .map(|var| {
                let shft = var.range.start;
                let mask = (1u128 << var.range.len()) - 1;
                let ident = Ident::new(&String::from(var.ident), Span::call_site());
                let mask_quote = syn::LitInt::new(&mask.to_string(), Span::call_site());
                quote! {
                    let #ident = (_bitmatch >> #shft) & #mask_quote;
                }
            })
            .collect();
        tokens.extend(quote!(
            #ty => {
                #(#var_initalization)*
                #body
            }
        ));
    }
}
