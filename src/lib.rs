#[cfg(not(feature = "big-arith"))]
pub type Integer = i64;
#[cfg(feature = "big-arith")]
pub type Integer = num_bigint::BigInt;
pub type Rational = num_rational::Ratio<Integer>;

pub mod si {
	pub use crate::quantities::isq::*;
	pub use crate::unit::si::*;
	pub type SimpleUnit = crate::unit::SimpleUnit<SI>;
	pub type ScalableUnit<T> = crate::unit::ScalableUnit<SI, T>;
	pub type AffineUnit<T, O> = crate::unit::AffineUnit<SI, T, O>;
	pub type Value<T, U> = crate::value::Value<SI, T, U>;
}

mod composite;

pub mod quantities;
pub mod unit;
pub mod value;
