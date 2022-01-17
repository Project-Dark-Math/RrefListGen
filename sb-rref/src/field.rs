use num_rational::Rational64;

pub trait Field: Copy + PartialEq {
    fn add(self, rhs: Self) -> Self;
    fn mul(self, rhs: Self) -> Self;
    fn add_ident() -> Self;
    fn mul_ident() -> Self;
    fn add_inv(self) -> Self;
    fn mul_inv(self) -> Self;
}

impl Field for Rational64 {
    #[inline]
    fn add(self, rhs: Self) -> Self {
        self + rhs
    }
    #[inline]
    fn mul(self, rhs: Self) -> Self {
        self * rhs
    }
    #[inline]
    fn add_ident() -> Self {
        Rational64::new(0, 1)
    }
    #[inline]
    fn mul_ident() -> Self {
        Rational64::new(1, 1)
    }
    #[inline]
    fn add_inv(self) -> Self {
        -self
    }
    #[inline]
    fn mul_inv(self) -> Self {
        Rational64::new(1, 1) / self
    }
}

pub mod f5 {
    use std::fmt::{self, Display};

    use super::*;

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum F5 {
        Zero,
        One,
        Two,
        Three,
        Four,
    }

    impl From<i32> for F5 {
        fn from(n: i32) -> Self {
            match n {
                0 => Self::Zero,
                1 => Self::One,
                2 => Self::Two,
                3 => Self::Three,
                4 => Self::Four,
                _ => panic!("Out of range of {n}"),
            }
        }
    }

    impl From<F5> for i32 {
        fn from(n: F5) -> Self {
            match n {
                F5::Zero => 0,
                F5::One => 1,
                F5::Two => 2,
                F5::Three => 3,
                F5::Four => 4,
            }
        }
    }

    impl Display for F5 {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", self)
        }
    }

    impl Field for F5 {
        #[inline]
        fn add(self, rhs: Self) -> Self {
            (i32::from(self) + i32::from(rhs)).rem_euclid(5).into()
        }
        #[inline]
        fn mul(self, rhs: Self) -> Self {
            (i32::from(self) * i32::from(rhs)).rem_euclid(5).into()
        }
        #[inline]
        fn add_ident() -> Self {
            Self::Zero
        }
        #[inline]
        fn mul_ident() -> Self {
            Self::One
        }
        #[inline]
        fn add_inv(self) -> Self {
            (-i32::from(self)).rem_euclid(5).into()
        }
        #[inline]
        fn mul_inv(self) -> Self {
            match self {
                F5::Zero => panic!("cannot divide with zero"),
                F5::One => F5::One,
                F5::Two => F5::Three,
                F5::Three => F5::Two,
                F5::Four => F5::Four,
            }
        }
    }
}
