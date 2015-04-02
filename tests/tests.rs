#![feature(plugin)]
#![plugin(wrapping_macros)]

#[test]
fn smoke() {
    let x = wrapping! { 1i32 + 1 };
    let y;
    let z;
    wrapping! {
        y = 2i32 + 3;
        z = y * 2;
    }
    assert_eq!(x, 2);
    assert_eq!(y, 5);
    assert_eq!(z, 10);
}
