#[cfg(not(feature = "big-rational"))]
type Rational = num_rational::Rational64;
#[cfg(feature = "big-rational")]
type Rational = num_rational::BigRational;

mod composite;

pub mod quantities;
pub mod unit;
pub mod value;
