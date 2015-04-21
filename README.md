# wrapping_macros [![Build Status](https://img.shields.io/travis/lfairy/wrapping_macros.svg)](http://travis-ci.org/lfairy/wrapping_macros) [![Cargo](https://img.shields.io/crates/v/wrapping_macros.svg)](https://crates.io/crates/wrapping_macros)

A macro for wrapping arithmetic.

Any code within a `wrapping! { .. }` block will be transformed as follows:

* `a + b` becomes `a.wrapping_add(b)`. Similarly for `-`, `*`, `/`, `%`, `<<`, `>>`.
* `a += b` becomes `a = a.wrapping_add(b)`. Similarly for `-=`, `*=`, `/=`, `%=`, `<<=`, `>>=`.
* `-a` becomes `a.wrapping_neg()`.

See [this Internals thread][1] for the motivation behind this crate.

[1]: http://internals.rust-lang.org/t/ergonomics-of-wrapping-operations/1756?u=lfairy

**Note:** This crate uses internal compiler APIs, and so requires the Nightly version of Rust.


## Cargo

Add this to your `Cargo.toml`:

```toml
wrapping_macros = "*"
```


## Example

```rust
#![feature(plugin)]
#![plugin(wrapping_macros)]

fn main() {
    let mut sum = 0u8;
    for x in 0u8..50 {
        wrapping! {
            sum += x;
        }
    }
}
```


## Caveats

*   This crate uses unstable APIs, and will only work on Rust Nightly.

*   You cannot nest another macro invocation within a `wrapping!` block. For example, this will not work:

    ```rust
    let x = 41i32;
    wrapping! {
        println!("The answer is {}", x + 1);  //~ ERROR
    }
    ```

    Instead, move the macro call out of the block:

    ```rust
    let x = 41i32;
    println!("The answer is {}", wrapping! { x + 1 });
    ```
