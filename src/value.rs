use std::hash::Hash;
use std::ops::{Add, Div, Mul, Sub};

use derivative::Derivative;
use num_traits::{Inv, One, Zero};

use crate::unit::{ScalableUnit, UnitSystem};

#[derive(Derivative)]
#[derivative(
	Clone(bound = "U::BaseUnit: Clone, T: Clone"),
	Debug(bound = "U::BaseUnit: std::fmt::Debug, T: std::fmt::Debug"),
	Eq(bound = "U::BaseUnit: Eq + Hash, T: Eq"),
	Default(bound = "T: Default")
)]
pub struct Value<T, U: UnitSystem> {
	pub value: T,
	pub unit: ScalableUnit<U, T>,
}

impl<T, X, U: UnitSystem> PartialEq<Value<X, U>> for Value<T, U>
where
	U::BaseUnit: Eq + Hash,
	T: PartialEq<X>,
{
	fn eq(&self, other: &Value<X, U>) -> bool {
		self.value == other.value && self.unit == other.unit
	}
}

impl<U: UnitSystem, T: Mul<X>, X> Mul<Value<X, U>> for Value<T, U>
where
	U::BaseUnit: Hash + Eq,
{
	type Output = Value<T::Output, U>;

	fn mul(self, rhs: Value<X, U>) -> Self::Output {
		Value {
			value: self.value * rhs.value,
			unit: self.unit * rhs.unit,
		}
	}
}

impl<U: UnitSystem, T: Div<X>, X> Div<Value<X, U>> for Value<T, U>
where
	U::BaseUnit: Hash + Eq,
{
	type Output = Value<T::Output, U>;

	fn div(self, rhs: Value<X, U>) -> Self::Output {
		Value {
			value: self.value / rhs.value,
			unit: self.unit / rhs.unit,
		}
	}
}

impl<U: UnitSystem, T: Inv> Inv for Value<T, U>
where
	U::BaseUnit: Hash + Eq,
{
	type Output = Value<T::Output, U>;

	fn inv(self) -> Self::Output {
		Value {
			value: self.value.inv(),
			unit: self.unit.inv(),
		}
	}
}

impl<U: UnitSystem, T: Add<X>, X> Add<Value<X, U>> for Value<T, U>
where
	U::BaseUnit: Hash + Eq,
	T: PartialEq<X>,
	T: Into<T::Output>,
{
	type Output = Value<T::Output, U>;

	fn add(self, rhs: Value<X, U>) -> Self::Output {
		if self.unit != rhs.unit {
			panic!("tried to add values with different units");
		}

		Value {
			value: self.value + rhs.value,
			unit: ScalableUnit {
				scale: self.unit.scale.into(),
				unit: self.unit.unit,
			},
		}
	}
}

impl<U: UnitSystem, T: Sub<X>, X> Sub<Value<X, U>> for Value<T, U>
where
	U::BaseUnit: Hash + Eq,
	T: PartialEq<X>,
	T: Into<T::Output>,
{
	type Output = Value<T::Output, U>;

	fn sub(self, rhs: Value<X, U>) -> Self::Output {
		if self.unit != rhs.unit {
			panic!("tried to subtract values with different units");
		}

		Value {
			value: self.value - rhs.value,
			unit: ScalableUnit {
				scale: self.unit.scale.into(),
				unit: self.unit.unit,
			},
		}
	}
}

impl<U: UnitSystem, T> Zero for Value<T, U>
where
	U::BaseUnit: Hash + Eq,
	T: Zero + One + PartialEq,
{
	fn zero() -> Self {
		Self {
			value: T::zero(),
			unit: ScalableUnit::new_dimensionless(T::one()),
		}
	}

	// note: the below impls treat e.g. 0 meters as being zero, even though zero() only returns dimensionless 0

	fn set_zero(&mut self) {
		self.value.set_zero()
	}

	fn is_zero(&self) -> bool {
		self.value.is_zero()
	}
}
