#[cfg(not(feature = "big-arith"))]
pub type Integer = i64;
#[cfg(feature = "big-arith")]
pub type Integer = num_bigint::BigInt;
pub type Rational = num_rational::Ratio<Integer>;

mod composite;

pub mod quantities;
pub mod unit;
pub mod value;
