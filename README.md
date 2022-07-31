# bitmatch-proc

An alternative Rust crate for bitmatching. The crate supports bitmatching for let and match expressions, as well as a bitpack! macro for packing bits into a single value in any given order. 
The provided macros works for const functions and ensures efficiency for bitmatching at runtime.

## Example with a match expression.
```rust
fn bitmatch(val: u16) -> u32{
  #[bitmatch]
  match val {
    "0111_xxxx_yyxx" => (x + y) as u32,
    /* z character is ignored when matching bits. */
    "0000_zzzz_zzzz" => 0 as u32, 
    "aaaa_bbbb_cccc" => ((a + b) << c) as u32
  }
}
```

## Example with a let expression.
```rust
fn bitlet(val: u32) -> u32{
  #[bitmatch]
  let "xxxx_yyyy_xxxx_yyyy" = val;
  x | y
}
``` 

## Example for bitpacking values.
```rust
fn bitpack(val: u32) -> u32{
  #[bitmatch]  
  let "aaaa_bbbb_cccc_dddd" = val;
  bitpack_proc!("dddd_aaaa_ccbb_bbcc")
}
```

For further examples of different use cases and syntax, go into src/lib.rs and look at the tests.