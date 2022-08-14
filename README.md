# bitmatch-proc

An alternative Rust crate for bitmatching. The crate supports bitmatching for let and match expressions, as well as a bitpack! macro for packing bits into a single value. 
The provided macros works for const functions as well as ensures efficiency for bitmatching at runtime with integers.

## Examples

Bitmatch may be used on a match where each literal portion has to match the expression of the match, and binds or ignores the remiander of the bits. 
```rust
fn bitmatch(val: u16) -> u32{
  #[bitmatch]
  match val {
    // 0111 portion has to correlate to val, while x and y are bound values.
    "0111_xxxx_yyxx" => (x + y) as u32,
    // z character is ignored when matching bits.
    "0000_zzzz_zzzz" => 0 as u32, 
    // Given there is no literal portion, this arm will always be true.
    "aaaa_bbbb_cccc" => ((a + b) << c) as u32
  }
}

fn main(){
  assert_eq!(bitmatch(0b0111_1111_1100), 0b0011_1111);
}
```

Bitmatch may also be used with a let in order to unpack a variable into single-character variables. 
```rust
fn bitlet(val: u32) -> u32{
  #[bitmatch]
  let "xxxx_yyyy_xxxx_yyyy" = val;
  x | y
}

fn main(){
  assert_eq!(bitmatch(0xF00F), 0xFF);
}
``` 

Bitpack allows packing the values of single-character variables into a single value.
```rust
fn bitpack(val: u32) -> u32{
  #[bitmatch]  
  let "aaaa_bbbb_cccc_dddd" = val;
  bitpack!("dddd_aaaa_ccbb_bbcc")
}

fn main(){
  assert_eq!(bitpack(0xABCD), 0xDAEC);
}
```

For further examples of different use cases and syntax, look at the tests inside src/lib.rs.