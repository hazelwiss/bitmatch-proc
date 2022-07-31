use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{parse::Parse, Arm, Attribute, Expr, ExprMatch, Ident, LitStr, Pat, PatWild, Token};

macro_rules! ignore_chars {
    () => {
        '_' | ' ' | '-'
    };
}

#[proc_macro_attribute]
pub fn bitmatch(attr: TokenStream, stream: TokenStream) -> TokenStream {
    assert!(
        attr.is_empty(),
        "bitmatch attribute may only be written as bare attribute name"
    );
    let out = syn::parse::<ProcMacroParse>(stream)
        .expect("unable to parse expression for bitmatch attribute");
    quote! {
        #out
    }
    .into()
}

enum ProcMacroParse {
    BitMatch(BitMatch),
    BitLet(BitLet),
}

#[proc_macro]
pub fn bitpack_proc(stream: TokenStream) -> TokenStream {
    let str = syn::parse::<LitStr>(stream).expect("bitpack only takes in a string");
    let bstr = parse_bstr(str, false);
    let val = bstr.val;
    let val = syn::LitInt::new(&val.to_string(), Span::call_site());
    let vars: Vec<proc_macro2::TokenStream> = bstr
        .vars
        .into_iter()
        .map(|var| {
            let ident = Ident::new(var.ident.to_string().as_str(), Span::call_site());
            let mask = var.mask;
            quote! {
                {
                    use ::bitmatch::{FromBitPack, IntoBitVar};
                    fn compute<T: FromBitPack + IntoBitVar>(val: T) -> T {
                        T::from_bitpack(val.into_bitvar(#mask))
                    }
                    compute(#ident)
                }
            }
        })
        .collect();
    quote! {
        {
            #val | #(#vars)|*
        }
    }
    .into()
}

struct BitMatch {
    expr: ExprMatch,
}

enum BitMatchPattern {
    Bit {
        attrs: Vec<Attribute>,
        bp: BitStr,
    },
    Or {
        attrs: Vec<Attribute>,
        cases: Vec<(Vec<Attribute>, BitStr)>,
    },
    Ident {
        attrs: Vec<Attribute>,
        mutability: Option<Token![mut]>,
        ident: Ident,
    },
    Wild {
        attrs: Vec<Attribute>,
    },
}

struct BitMatchArm {
    attrs: Vec<Attribute>,
    pattern: BitMatchPattern,
    body: Box<Expr>,
}

struct BitLet {
    lit_str: LitStr,
    expr: Expr,
    attributes: Vec<Attribute>,
    pat: Pat,
}

struct BitStr {
    vars: Vec<Var>,
    mask: u128,
    val: u128,
}

#[derive(Clone)]
struct Var {
    ident: char,
    mask: u128,
}

fn parse_bstr(str: LitStr, allow_ignore_char: bool) -> BitStr {
    let string = str.value();
    assert!(!string.is_empty(), "string for bitmatch may not be empty");
    let mut mask = 0;
    let mut val = 0;
    let mut pat_vars: Vec<Var> = vec![];
    let mut pos = 0;
    let mut iter = string.chars().rev().peekable();
    while let Some(c) = iter.next() {
        let bit = 1 << pos;
        match c {
            '0' => {
                mask |= bit;
                pos += 1;
            }
            '1' => {
                mask |= bit;
                val |= bit;
                pos += 1;
            }
            'z' | 'Z' => {
                if allow_ignore_char {
                    pos += 1
                } else {
                    panic!("use of 'z' or 'Z' is illegla here")
                }
            }
            'a'..='y' | 'A'..='Y' => {
                if let Some(var) = pat_vars.iter_mut().find(|e| e.ident == c) {
                    var.mask |= bit;
                } else {
                    pat_vars.push(Var {
                        ident: c,
                        mask: bit,
                    });
                }
                pos += 1;
            }
            ignore_chars!() => {}
            _ => panic!("Invalid character '{c}' in string literal"),
        }
    }
    BitStr {
        vars: pat_vars,
        mask,
        val,
    }
}

impl BitMatchArm {
    fn from_arm(arm: Arm) -> Self {
        assert!(!arm.guard.is_some(), "match arm cannot have guard");
        let pattern = match arm.pat {
            syn::Pat::Wild(wild) => BitMatchPattern::Wild { attrs: wild.attrs },
            syn::Pat::Ident(ident) => {
                assert!(!ident.by_ref.is_some(), "identifier cannot be a reference");
                assert!(
                    !ident.subpat.is_some(),
                    "illegal subpatern detected in identifier"
                );
                BitMatchPattern::Ident {
                    attrs: ident.attrs,
                    ident: ident.ident,
                    mutability: ident.mutability,
                }
            }
            syn::Pat::Or(or) => {
                let cases: Vec<(Vec<Attribute>, BitStr)> = or
                    .cases
                    .into_iter()
                    .map(|case| match case {
                        syn::Pat::Lit(pat) => match *pat.expr {
                            Expr::Lit(mut lit) => match lit.lit {
                                syn::Lit::Str(str) => (
                                    {
                                        let mut vec = pat.attrs;
                                        vec.append(&mut lit.attrs);
                                        vec
                                    },
                                    parse_bstr(str, true),
                                ),
                                _ => panic!("literal has to be a string"),
                            },
                            _ => panic!("pattern expression may only be a string literal"),
                        },
                        _ => panic!(
                            "bitmatch only accepts string literals or identifiers as patterns"
                        ),
                    })
                    .collect();
                assert!(
                    !cases.is_empty(),
                    "multiple arm pattern cases cannot be empty."
                );
                let base_case = &cases[0];
                for i in 1..cases.len() {
                    let vars = &base_case.1.vars;
                    let cur = &cases[i].1.vars;
                    vars.iter().for_each(|var| {
                        assert!(
                            cur.iter()
                                .find(|search| var.mask != search.mask && var.ident == search.ident)
                                .is_some(),
                            "mismatching variable '{}' within multiple patterns in match arm",
                            var.ident
                        )
                    });
                }
                BitMatchPattern::Or {
                    attrs: or.attrs,
                    cases,
                }
            }
            syn::Pat::Lit(pat) => match *pat.expr {
                Expr::Lit(mut lit) => match lit.lit {
                    syn::Lit::Str(str) => BitMatchPattern::Bit {
                        attrs: {
                            let mut vec = pat.attrs;
                            vec.append(&mut lit.attrs);
                            vec
                        },
                        bp: parse_bstr(str, true),
                    },
                    _ => panic!("literal has to be a string"),
                },
                _ => panic!("pattern expression may only be a string literal"),
            },
            _ => panic!("bitmatch only accepts string literals or identifiers as patterns"),
        };
        Self {
            attrs: arm.attrs,
            pattern,
            body: arm.body,
        }
    }
}

impl Parse for ProcMacroParse {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(if let Ok(expr) = input.parse::<BitMatch>() {
            Self::BitMatch(expr)
        } else if let Ok(expr) = input.parse::<BitLet>() {
            Self::BitLet(expr)
        } else {
            return Err(input.error("bitmatch macro only accepts let or match expressions."));
        })
    }
}

impl Parse for BitLet {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let attributes = if let Ok(atr) = input.call(Attribute::parse_outer) {
            atr
        } else {
            vec![]
        };
        input.parse::<Token![let]>()?;
        let lit_str = input.parse::<LitStr>()?;
        let pat = if input.parse::<Token![:]>().is_ok() {
            input.parse::<Pat>()?
        } else {
            Pat::Wild(PatWild {
                attrs: vec![],
                underscore_token: Default::default(),
            })
        };
        input.parse::<Token![=]>()?;
        let expr = input.parse::<Expr>()?;
        input.parse::<Token![;]>()?;
        Ok(BitLet {
            lit_str,
            expr,
            attributes,
            pat,
        })
    }
}

impl Parse for BitMatch {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            expr: input.parse::<ExprMatch>()?,
        })
    }
}

impl ToTokens for ProcMacroParse {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.extend(match self {
            ProcMacroParse::BitMatch(m) => quote! { #m },
            ProcMacroParse::BitLet(l) => quote! { #l },
        });
    }
}

impl ToTokens for BitLet {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let string = self.lit_str.value();
        let expr = &self.expr;
        let attrs = &self.attributes;
        let pat = &self.pat;
        let mut iter = string.chars().rev().peekable();
        let mut pos = 0;
        let mut vars: Vec<Var> = vec![];
        while let Some(c) = iter.next() {
            let bit = 1 << pos;
            match c {
                'a'..='y' | 'A'..='Y' => {
                    if let Some(var) = vars.iter_mut().find(|e| e.ident == c) {
                        var.mask |= bit;
                    } else {
                        vars.push(Var {
                            ident: c,
                            mask: bit,
                        });
                    }
                    pos += 1;
                }
                'z' | 'Z' => {
                    pos += 1;
                }
                ignore_chars!() => {}
                _ => panic!(""),
            }
        }
        let (lhs, rhs) =
            vars.into_iter()
                .rev()
                .fold((vec![], vec![]), |(mut lhs, mut rhs), var| {
                    lhs.push(var.ident);
                    rhs.push(var);
                    (lhs, rhs)
                });
        let lhs: Vec<proc_macro2::TokenStream> = lhs
            .into_iter()
            .map(|ident| {
                let ident = Ident::new(ident.to_string().as_str(), Span::call_site());
                quote! {#ident}
            })
            .collect();
        let rhs: Vec<proc_macro2::TokenStream> = rhs
            .into_iter()
            .map(|var| {
                let mask = var.mask;
                quote! {
                    {
                        use ::bitmatch::{IntoBitVar, FromBitVar};
                        fn restrict<T: FromBitVar + IntoBitVar>(v: T) -> T{
                            FromBitVar::from_bitvar(IntoBitVar::into_bitvar(v, #mask))
                        }
                        restrict(#expr)
                    }
                }
            })
            .collect();
        tokens.extend(quote! {
            #(#attrs)*
            let (#(#lhs),*): #pat = (#(#rhs),*);
        })
    }
}

impl ToTokens for BitMatch {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let expr = self.expr.clone();
        let arms: Vec<BitMatchArm> = expr
            .arms
            .into_iter()
            .map(|arm| BitMatchArm::from_arm(arm))
            .collect();
        let match_attributes = &expr.attrs;
        let match_expr = &expr.expr;
        tokens.extend(quote! {
            {
                #[inline(always)]
                fn _full_bitmatch_cmp(cmp: impl ::bitmatch::BitMaskable, mask: u128, val: u128) -> bool{
                    use  ::bitmatch::BitMaskable;
                    cmp.bitmask(mask, val)
                }
                #(#match_attributes)*
                match #match_expr{
                    #(#arms)*
                }
            }
        });
    }
}

impl ToTokens for BitMatchArm {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let bit_pattern_body = |body, attrs: &Vec<Attribute>, bp: &BitStr| {
            let BitStr { vars, mask, val } = &bp;
            let vars_init_vec: Vec<proc_macro2::TokenStream> = vars
                .iter()
                .map(|var| {
                    let var_name = Ident::new(var.ident.to_string().as_str(), Span::call_site());
                    let mask = var.mask;
                    quote! {
                        let #var_name = {
                            use ::bitmatch::{IntoBitVar, FromBitVar};
                            fn into_bitvar<T: IntoBitVar>(bitvar: T) -> ::bitmatch::BitVar<T>{
                                bitvar.into_bitvar(#mask)
                            }
                            ::bitmatch::FromBitVar::from_bitvar(into_bitvar(_full_bitmatch))
                        };
                    }
                })
                .collect();
            quote! {
                #(#attrs)*
                _full_bitmatch if _full_bitmatch_cmp(_full_bitmatch, #mask, #val) => {
                    #(#vars_init_vec)*
                    {
                        // static assertions.
                    }
                    { #body }
                }
            }
        };
        let body = &self.body;
        let body = match &self.pattern {
            BitMatchPattern::Bit { attrs, bp } => bit_pattern_body(body, attrs, bp),
            BitMatchPattern::Or { attrs, cases } => {
                let cases: Vec<proc_macro2::TokenStream> = cases
                    .iter()
                    .map(|(attrs, case)| {
                        let body = bit_pattern_body(body, attrs, case);
                        quote! { #body }
                    })
                    .collect();
                quote!(
                    #(#attrs)*
                    #(#cases)*
                )
            }
            BitMatchPattern::Ident {
                ident,
                mutability,
                attrs,
            } => quote! {
                #(#attrs)*
                #mutability #ident => { #body }
            },
            BitMatchPattern::Wild { attrs } => {
                quote! {
                    #(#attrs)*
                    _ => { #body }
                }
            }
        };
        let attrs = &self.attrs;
        tokens.extend(quote! {
            #(#attrs)*
            #body
        });
    }
}
