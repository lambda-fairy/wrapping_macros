#![feature(plugin)]
#![plugin(wrapping_macros)]

#[test]
fn it_works() {
    let x = wrapping! { 1 + 1 };
    panic!("{}", x);
}
