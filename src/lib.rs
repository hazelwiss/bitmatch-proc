#![no_std]

pub use macros::bitmatch;

#[cfg(test)]
#[macro_use]
extern crate std;
extern crate self as bitmatch;

#[cfg(test)]
mod tests {
    use macros::bitmatch;

    #[test]
    fn test_match() {
        // 0000 ???? ???? <- None,
        // 0001 xxxx ???? <- x
        // 0010 0?0? 0?xx <- x
        // 0011 xxxx xx?0 <- x
        let tests_input: std::vec::Vec<(u32, Option<u16>)> = vec![
            (0b0000_1100_0010, Some(0)),
            (0b0000_0101_0000, Some(0)),
            (0b0000_0000_0000, Some(0)),
            (0b0001_1111_0000, Some(0b1111)),
            (0b0001_1101_0101, Some(0b1101)),
            (0b0010_0101_0011, Some(0b11)),
            (0b0010_0101_0111, Some(0b11)),
            (0b0010_0101_1011, None),
            (0b0010_0100_1100, None),
            (0b0010_0101_0100, Some(0b00)),
            (0b0010_0101_0110, Some(0b10)),
            (0b0011_0000_1100, Some(0b11)),
            (0b0011_0000_1101, None),
            (0b0011_1110_1100, Some(0b11_1011)),
        ];
        let mut test_case = 0;
        for (i, r) in tests_input {
            let val = bitmatch! {
                match i {
                    "0000_????_????" => Some(0),
                    "0001_xxxx_????" => Some(x),
                    "0010_0?0?_0?xx" => Some(x),
                    "0011_xxxx_xx?0" => Some(x),
                    _ => None,
                }
            };
            assert!(
                    r == val,
                    "expected '{r:?}', got '{val:?}'\noriginal input: '{i:012b}'\nat test_case {test_case}"
                );
            test_case += 1;
        }
    }
}
