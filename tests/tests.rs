#![feature(core)]
#![feature(plugin)]
#![plugin(wrapping_macros)]

mod simple {
    #[test]
    fn add() {
        let a = 255u8;
        let b = wrapping! { a + 2 };
        assert_eq!(b, 1);
    }

    #[test]
    fn sub() {
        let a = 1u8;
        let b = wrapping! { a - 2 };
        assert_eq!(b, 255);
    }

    #[test]
    fn mul() {
        let a = 128u8;
        let b = wrapping! { a * 2 };
        assert_eq!(b, 0);
    }

    #[test]
    fn neg() {
        let a = -128i8;
        let b = wrapping! { -a };
        assert_eq!(b, -128);
        let c = 1u8;
        let d = wrapping! { -c };
        assert_eq!(d, 255);
    }

    #[test]
    fn nested() {
        let a = 100u8;
        let b = wrapping! { -(a + a * (a + a + a)) };
        assert_eq!(b, 108);
    }
}

mod compound {
    #[test]
    fn basic() {
        let mut a = 250u8;
        let mut b = 4u8;
        let mut c = 100u8;
        wrapping! {
            a += 10;
            b -= 10;
            c *= 10;
        }
        assert_eq!(a, 4);
        assert_eq!(b, 250);
        assert_eq!(c, 232);
    }

    #[test]
    fn accumulate() {
        let mut byte = 0u8;
        let mut long = 0u64;
        wrapping! {
            for i in 1..1000 {
                byte += i as u8; long += i as u64;
                byte *= i as u8; long *= i as u64;
            }
        }
        assert_eq!(byte, (long & 0xff) as u8);
    }
}
