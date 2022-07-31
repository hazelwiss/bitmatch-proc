/*! 
 * 
*/

#![no_std]
#![feature(stmt_expr_attributes)]
#![feature(proc_macro_hygiene)]
#![feature(inline_const)]
#![feature(const_trait_impl)]
#![feature(const_ops)]
#![feature(const_slice_index)]

#![warn(missing_docs)]

#[cfg(test)]
#[macro_use]
extern crate std;
extern crate self as bitmatch;

///
pub use macros::bitpack_proc as bitpack;

///
pub use macros::bitmatch;

///
pub trait BitMaskable {
    ///
    fn bitmask(self, mask: u128, val: u128) -> bool;
}

///
pub trait IntoBitVar: Sized {
    ///
    fn into_bitvar(self, mask: u128) -> BitVar<Self>;
}

///
pub trait FromBitVar: Sized {
    ///
    fn from_bitvar(var: BitVar<Self>) -> Self;
}

///
pub trait FromBitPack: Sized {
    ///
    fn from_bitpack(var: BitVar<Self>) -> Self;
}

///
pub struct BitVar<T> {
    data: T,
    mask: u128,
}

macro_rules! impl_from_bit_maskable {
    ($($t:ty)*) => {
        $(impl const BitMaskable for $t {
            #[inline(always)]
            fn bitmask(self, mask: u128, val: u128) -> bool {
                (self as u128) & mask == val
            }
        })*
    };
}

macro_rules! impl_into_bit_var {
    ($($t:ty)*) => {
        $(impl const IntoBitVar for $t {
            #[inline(always)]
            fn into_bitvar(
                self,
                mask: u128,
            ) -> BitVar<$t>{
                BitVar{
                    data: self,
                    mask,
                }
            }
        })*
    };
}

macro_rules! impl_from_bit_var {
    ($($t:ty)*) => {
        $(impl const FromBitVar for $t {
            #[inline(always)]
            fn from_bitvar(var: BitVar<$t>) -> $t{
                let mut data = var.data;
                let mut mask = var.mask as $t;
                let mut ret = 0;
                let mut offset = 0;
                while mask != 0 {
                    let trailing_zeros = mask.trailing_zeros();
                    data >>= trailing_zeros;
                    mask >>= trailing_zeros;
                    let trailing_ones = mask.trailing_ones();
                    ret |= (data & ((1 << trailing_ones) - 1) as $t) << offset;
                    offset += trailing_ones;
                    data >>= trailing_ones;
                    mask >>= trailing_ones;
                }
                ret
            }
        })*
    };
}

macro_rules! impl_from_bit_var_raw {
    ($($t:ty)*) => {
        $(impl const FromBitPack for $t {
            #[inline(always)]
            fn from_bitpack(var: BitVar<$t>) -> Self{
                let mut data = var.data;
                let mut mask = var.mask as $t;
                let mut ret = 0;
                let mut offset = 0;
                while mask != 0 {
                    let trailing_zeros = mask.trailing_zeros();
                    mask >>= trailing_zeros;
                    offset += trailing_zeros;
                    let trailing_ones = mask.trailing_ones();
                    ret |= (data & ((1 << trailing_ones) - 1) as $t) << offset;
                    offset += trailing_ones;
                    data >>= trailing_ones;
                    mask >>= trailing_ones;
                }
                ret
            }
        })*
    };
}

macro_rules! impl_all {
    ($($t:ty)*) => {
        impl_from_bit_maskable!($($t)*);
        impl_into_bit_var!($($t)*);
        impl_from_bit_var!($($t)*);
        impl_from_bit_var_raw!($($t)*);
    };
}

impl_all!(u8 u16 u32 u64 u128);

#[cfg(test)]
mod tests {
    use macros::{bitmatch, bitpack_proc};

    #[test]
    fn test_match() {
        // 0000 zzzz zzzz <- None,
        // 0001 xxxx zzzz <- x
        // 0010 0x0x 0zxx <- x
        // 0011 xxxx xxz0 <- x
        // 0100 xxyy xyxy <- x + y
        // 0101 xay0 xay1 <- x + y + a
        let tests_input: std::vec::Vec<(u32, Option<u32>)> = vec![
            (0b0000_1100_0010, Some(0)),
            (0b0000_0101_0000, Some(0)),
            (0b0000_0000_0000, Some(0)),
            (0b0001_1111_0000, Some(0b1111)),
            (0b0001_1101_0101, Some(0b1101)),
            (0b0010_0101_0011, Some(0b1111)),
            (0b0010_0101_0111, Some(0b1111)),
            (0b0010_0101_1011, None),
            (0b0010_0100_1100, None),
            (0b0011_0000_1100, Some(0b11)),
            (0b0011_0000_1101, None),
            (0b0100_1100_1010, Some(0b1111)),
            (0b0100_0011_0101, Some(0b1111)),
            (0b0100_0000_1111, Some(0b11 + 0b11)),
            (0b0101_1110_1111, Some(0b11 + 0b11 + 0b11)),
            (0b0101_1110_1110, None),
        ];
        let mut test_case = 0;
        for (i, r) in tests_input {
            let val = #[bitmatch]
            match i {
                "0000_zzzz_zzzz" => Some(0),
                "0001_xxxx_zzzz" => Some(x),
                "0010_0x0x_0zxx" => Some(x),
                "0011_xxxx_xxz0" => Some(x),
                "0100_xxyy_xyxy" => Some(x + y),
                "0101_xay0_xay1" => Some(x + y + a),
                _ => None,
            };
            assert!(
                    r == val,
                    "expected '{r:?}', got '{val:?}'\noriginal input: '{i:012b}'\nat test_case {test_case}"
                );
            test_case += 1;
        }
    }

    #[test]
    fn test_let_no_split() {
        for input_value in 0..u16::MAX {
            {
                #[bitmatch]
                let "aaaa_bbbb_cccc_dddd" = input_value;
                let a_test = input_value >> 12;
                let b_test = (input_value >> 8) & 0xF;
                let c_test = (input_value >> 4) & 0xF;
                let d_test = input_value & 0xF;
                assert!(
                    a_test == a && b_test == b && c_test == c && d_test == d,
                    "input: {input_value:016b}\na: {a} == {a_test}\nb: {b} == {b_test}\nc: {c} == {c_test}\nd: {d} == {d_test}"
                )
            }
        }
    }

    #[test]
    fn test_let_split() {
        for input_value in 0..u16::MAX {
            {
                #[bitmatch]
                let "aaaa_baaa_cdaa_dcda" = input_value;
                let a_test = ((input_value >> 12) << 6)
                    | (((input_value >> 8) & 0b111) << 3)
                    | (((input_value >> 4) & 0b11) << 1)
                    | (input_value & 0b1);
                let b_test = ((input_value >> 8) & 0b1000) >> 3;
                let c_test = (((input_value >> 4) & 0b1000) >> 2) | ((input_value & 0b0100) >> 2);
                let d_test = ((input_value & 0b10) >> 1)
                    | ((input_value & 0b1000) >> 2)
                    | ((input_value & 0b0100_0000) >> 4);
                assert!(
                    a_test == a && b_test == b && c_test == c && d_test == d,
                    "input: {input_value:016b}\na: {a} == {a_test}\nb: {b} == {b_test}\nc: {c} == {c_test}\nd: {d} == {d_test}"
                )
            }
        }
    }

    #[test]
    fn test_let_ignore() {
        for input_value in 0..u16::MAX {
            {
                #[bitmatch]
                let "aaaa_zaaa_zzaa_zzza" = input_value;
                let a_test = ((input_value >> 12) << 6)
                    | (((input_value >> 8) & 0b111) << 3)
                    | (((input_value >> 4) & 0b11) << 1)
                    | (input_value & 0b1);
                assert!(a_test == a, "input: {input_value:016b}\na: {a} == {a_test}")
            }
        }
    }

    #[test]
    fn test_bitpack_from_bitmatch() {
        for input_value in 0..u16::MAX {
            #[bitmatch]
            let "aaaa_bbbb_cccc_dddd" = input_value;
            #[bitmatch]
            let "eeee_ffff_gggg_hhhh" = bitpack_proc!("aaaa_bbbb_cccc_dddd");
            assert!(
                a == e && b == f && c == g && d == h, "input_value: {input_value:016b}\na: {a} == {e}\nb: {b} == {f}\nc: {c} == {g}\nd: {d} == {h}")
        }
    }

    #[test]
    fn test_bitpack_mixed_with_bitlet() {
        for input_value in 0..u16::MAX {
            #[bitmatch]
            let "aaaa_bbbb_cccc_dddd" = input_value;
            let val = bitpack_proc!("dcba_dcba_dcba_dcba");
            #[bitmatch]
            let "eeee_ffff_gggg_hhhh" = val;
            #[bitmatch]
            let "zzzi_zzzi_zzzi_zzzi" = val;
            #[bitmatch]
            let "zzjz_zzjz_zzjz_zzjz" = val;
            #[bitmatch]
            let "zkzz_zkzz_zkzz_zkzz" = val;
            #[bitmatch]
            let "lzzz_lzzz_lzzz_lzzz" = val;
            let combine = |shft: u16| {
                ((i >> shft) & 1)
                    | (((j >> shft) & 1) << 1)
                    | (((k >> shft) & 1) << 2)
                    | (((l >> shft) & 1) << 3)
            };
            let cmp_e = combine(3);
            let cmp_f = combine(2);
            let cmp_g = combine(1);
            let cmp_h = combine(0);
            assert!(
                e == cmp_e && f == cmp_f&& g == cmp_g && h == cmp_h, 
                "input_value: {input_value:016b}\ne {e} == {cmp_e}\nf {f} == {cmp_f}\ng: {g} == {cmp_g}\nh: {h} == {cmp_h}")
        }
    }

    #[test]
    fn simple_unpack() {
        for input_value in 0..u16::MAX{
            #[bitmatch]
            let "aaaa_aaaa_bbbb_bbbb" = input_value;
            #[bitmatch]
            let "cccc_cccc_dddd_dddd" = bitpack_proc!("bbbb_bbbb_aaaa_aaaa");
            #[bitmatch]
            let "eeee_eeee_ffff_ffff" = bitpack_proc!("dddd_dddd_cccc_cccc");
            assert!(a == e && f == b, "input: {input_value} a: {a}\n b: {b}\ne: {e}\nf: {f}");
        }
    }

    #[test]
    fn test_bitpack_cmp_bitpack() {
        for input_value in 0..u16::MAX{
            #[bitmatch]
            let "hhhh_hhhh_llll_llll" = input_value;
            let reversed = bitpack_proc!("llll_llll_hhhh_hhhh");
            assert!((reversed >> 8) | (reversed << 8) == input_value, "input: {input_value:016b}\nreversed: {reversed:016b}");
        }
    }
}
