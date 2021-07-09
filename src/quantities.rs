use std::hash::Hash;
use std::ops::{Div, Mul};

use derivative::Derivative;
use num_traits::Inv;

use crate::composite::Composite;
use crate::Rational;

pub trait SystemOfQuantities {
	type BaseQuantity;
}

pub mod isq {
	use super::SystemOfQuantities;
	use crate::quantities::Quantity;
	use crate::{Integer, Rational};
	use num_traits::Inv;
	use std::ops::{Div, Mul};

	pub struct ISQ;

	impl SystemOfQuantities for ISQ {
		type BaseQuantity = BaseQuantity;
	}

	#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
	pub enum BaseQuantity {
		/// length, L
		Length,
		/// mass, M
		Mass,
		/// time, T
		Time,
		/// electric current, I
		Current,
		/// thermodynamic temperature, Θ
		Temperature,
		/// amount of substance, N
		Substance,
		/// luminous intensity, J
		LumIntensity,
	}

	impl BaseQuantity {
		pub fn pow(self, pow: Rational) -> Quantity<ISQ> {
			Quantity::new_base_pow(self, pow)
		}

		pub fn ipow(self, pow: Integer) -> Quantity<ISQ> {
			self.pow(Rational::from_integer(pow))
		}
	}

	impl Mul for BaseQuantity {
		type Output = Quantity<ISQ>;

		fn mul(self, rhs: Self) -> Self::Output {
			Quantity::new_base(self) * Quantity::new_base(rhs)
		}
	}

	impl Div for BaseQuantity {
		type Output = Quantity<ISQ>;

		fn div(self, rhs: Self) -> Self::Output {
			Quantity::new_base(self) / Quantity::new_base(rhs)
		}
	}

	impl Inv for BaseQuantity {
		type Output = Quantity<ISQ>;

		fn inv(self) -> Self::Output {
			self.ipow(Integer::from(-1i8))
		}
	}
}

#[derive(Derivative)]
#[derivative(
	Debug(bound = "Q::BaseQuantity: std::fmt::Debug"),
	Clone(bound = "Q::BaseQuantity: Clone"),
	PartialEq(bound = "Q::BaseQuantity: Eq + Hash"),
	Eq(bound = "Q::BaseQuantity: Eq + Hash"),
	Default(bound = "")
)]
#[repr(transparent)]
pub struct Quantity<Q: SystemOfQuantities>(pub(crate) Composite<Q::BaseQuantity>);

impl<Q: SystemOfQuantities> Mul for Quantity<Q>
where
	Q::BaseQuantity: Hash + Eq,
{
	type Output = Self;

	fn mul(self, rhs: Self) -> Self::Output {
		Self(self.0 * rhs.0)
	}
}

impl<Q: SystemOfQuantities> Div for Quantity<Q>
where
	Q::BaseQuantity: Hash + Eq,
{
	type Output = Self;

	fn div(self, rhs: Self) -> Self::Output {
		Self(self.0 / rhs.0)
	}
}

impl<Q: SystemOfQuantities> Inv for Quantity<Q> {
	type Output = Self;

	fn inv(self) -> Self::Output {
		Self(self.0.inv())
	}
}

impl<Q: SystemOfQuantities> Quantity<Q> {
	pub fn new_dimensionless() -> Self {
		Self::default()
	}

	pub fn new_base_pow(base: Q::BaseQuantity, power: Rational) -> Self
	where
		Q::BaseQuantity: Hash + Eq,
	{
		Self(Composite::new_base_pow(base, power))
	}

	pub fn new_base(base: Q::BaseQuantity) -> Self
	where
		Q::BaseQuantity: Hash + Eq,
	{
		Self(Composite::new_base(base))
	}
}
