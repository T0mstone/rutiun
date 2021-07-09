use std::hash::Hash;
use std::ops::{Div, Mul};

use derivative::Derivative;
use num_traits::{Inv, Pow};

use crate::composite::Composite;
use crate::{Integer, Rational};

pub trait SystemOfQuantities {
	type BaseQuantity;
}

pub mod isq {
	use std::ops::{Div, Mul};

	use num_traits::{Inv, Pow};

	use super::{Quantity, SystemOfQuantities};
	use crate::{Integer, Rational};

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

	impl Pow<Rational> for BaseQuantity {
		type Output = Quantity<ISQ>;

		#[inline]
		fn pow(self, rhs: Rational) -> Self::Output {
			Quantity::new_base_pow(self, rhs)
		}
	}

	impl Pow<Integer> for BaseQuantity {
		type Output = Quantity<ISQ>;

		#[inline]
		fn pow(self, rhs: Integer) -> Self::Output {
			self.pow(Rational::from_integer(rhs))
		}
	}

	impl Pow<i32> for BaseQuantity {
		type Output = Quantity<ISQ>;

		#[inline]
		fn pow(self, rhs: i32) -> Self::Output {
			self.pow(Integer::from(rhs))
		}
	}

	impl Mul for BaseQuantity {
		type Output = Quantity<ISQ>;

		#[inline]
		fn mul(self, rhs: Self) -> Self::Output {
			Quantity::new_base(self) * Quantity::new_base(rhs)
		}
	}

	impl Mul<Quantity<ISQ>> for BaseQuantity {
		type Output = Quantity<ISQ>;

		#[inline]
		fn mul(self, rhs: Quantity<ISQ>) -> Self::Output {
			Quantity::new_base(self) * rhs
		}
	}

	impl Mul<BaseQuantity> for Quantity<ISQ> {
		type Output = Quantity<ISQ>;

		#[inline]
		fn mul(self, rhs: BaseQuantity) -> Self::Output {
			self * Quantity::new_base(rhs)
		}
	}

	impl Div for BaseQuantity {
		type Output = Quantity<ISQ>;

		#[inline]
		fn div(self, rhs: Self) -> Self::Output {
			Quantity::new_base(self) / Quantity::new_base(rhs)
		}
	}

	impl Div<Quantity<ISQ>> for BaseQuantity {
		type Output = Quantity<ISQ>;

		#[inline]
		fn div(self, rhs: Quantity<ISQ>) -> Self::Output {
			Quantity::new_base(self) / rhs
		}
	}

	impl Div<BaseQuantity> for Quantity<ISQ> {
		type Output = Quantity<ISQ>;

		#[inline]
		fn div(self, rhs: BaseQuantity) -> Self::Output {
			self / Quantity::new_base(rhs)
		}
	}

	impl Inv for BaseQuantity {
		type Output = Quantity<ISQ>;

		#[inline]
		fn inv(self) -> Self::Output {
			self.pow(Integer::from(-1i8))
		}
	}

	impl Quantity<ISQ> {
		#[inline]
		pub fn velocity() -> Self {
			BaseQuantity::Length * BaseQuantity::Time.pow(-1)
		}

		#[inline]
		pub fn acceleration() -> Self {
			BaseQuantity::Length * BaseQuantity::Time.pow(-2)
		}

		#[inline]
		pub fn momentum() -> Self {
			BaseQuantity::Mass * Self::velocity()
		}

		#[inline]
		pub fn force() -> Self {
			BaseQuantity::Mass * Self::acceleration()
		}

		#[inline]
		pub fn energy() -> Self {
			BaseQuantity::Mass * BaseQuantity::Length.pow(2) * BaseQuantity::Time.pow(-2)
		}

		#[inline]
		pub fn power() -> Self {
			BaseQuantity::Mass * BaseQuantity::Length.pow(2) * BaseQuantity::Time.pow(-3)
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

impl<Q: SystemOfQuantities> Pow<Rational> for Quantity<Q> {
	type Output = Self;

	fn pow(self, rhs: Rational) -> Self::Output {
		Self(self.0.pow(rhs))
	}
}

impl<Q: SystemOfQuantities> Pow<Integer> for Quantity<Q> {
	type Output = Self;

	fn pow(self, rhs: Integer) -> Self::Output {
		Self(self.0.pow(Rational::from_integer(rhs)))
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
