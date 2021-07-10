use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::{Add, Div, Mul, Sub};

use derivative::Derivative;
use num_traits::{Inv, One, Pow, Zero};

use crate::unit::{ScalableUnit, UnitForValue, UnitSystem};
use crate::{Clopy, Integer, Rational};

#[derive(Derivative)]
#[derivative(
	Copy(bound = "T: Copy, U: Copy"),
	Clone(bound = "T: Clone, U: Clone"),
	Debug(bound = "T: std::fmt::Debug, U: std::fmt::Debug"),
	Eq(bound = "T: Eq, U: Eq"),
	Default(bound = "T: Default, U: Default")
)]
pub struct Value<S: UnitSystem, T, U: UnitForValue<S, T>> {
	pub value: T,
	pub unit: U,
	_marker: PhantomData<S>,
}

impl<S: UnitSystem, T, U: UnitForValue<S, T>> Value<S, T, U> {
	pub fn new(value: T, unit: U) -> Self {
		Self {
			value,
			unit,
			_marker: PhantomData,
		}
	}

	pub fn into_unit<U2: UnitForValue<S, T>>(self, target: U2) -> Value<S, T, U2> {
		let value = target.convert_from_simple(self.unit.convert_to_simple(self.value));
		Value {
			value,
			unit: target,
			_marker: PhantomData,
		}
	}

	pub fn map_value<F: FnOnce(T) -> T2, T2>(self, f: F) -> Value<S, T2, U>
	where
		U: UnitForValue<S, T2>,
	{
		Value {
			value: f(self.value),
			unit: self.unit,
			_marker: PhantomData,
		}
	}
}

impl<S: UnitSystem, T1, T2, U1: UnitForValue<S, T1>, U2: UnitForValue<S, T2>>
	PartialEq<Value<S, T2, U2>> for Value<S, T1, U1>
where
	T1: PartialEq<T2>,
	U1: PartialEq<U2>,
{
	fn eq(&self, other: &Value<S, T2, U2>) -> bool {
		self.value == other.value && self.unit == other.unit
	}
}

impl<S: UnitSystem, T1, T2, U1: UnitForValue<S, T1>, U2: UnitForValue<S, T2>> Mul<Value<S, T2, U2>>
	for Value<S, T1, U1>
where
	T1: Mul<T2>,
	U1: Mul<U2>,
	<U1 as Mul<U2>>::Output: UnitForValue<S, <T1 as Mul<T2>>::Output>,
{
	type Output = Value<S, T1::Output, U1::Output>;

	fn mul(self, rhs: Value<S, T2, U2>) -> Self::Output {
		Value {
			value: self.value * rhs.value,
			unit: self.unit * rhs.unit,
			_marker: PhantomData,
		}
	}
}

impl<S: UnitSystem, T, U: UnitForValue<S, T>> Pow<Rational> for Value<S, T, U>
where
	T: Pow<Rational>,
	U: Pow<Rational>,
	<U as Pow<Rational>>::Output: UnitForValue<S, <T as Pow<Rational>>::Output>,
{
	type Output = Value<S, T::Output, U::Output>;

	fn pow(self, rhs: Rational) -> Self::Output {
		Value {
			value: self.value.pow(rhs.clopy()),
			unit: self.unit.pow(rhs),
			_marker: PhantomData,
		}
	}
}

impl<S: UnitSystem, T, U: UnitForValue<S, T>> Pow<Integer> for Value<S, T, U>
where
	T: Pow<Integer>,
	U: Pow<Integer>,
	<U as Pow<Integer>>::Output: UnitForValue<S, <T as Pow<Integer>>::Output>,
{
	type Output = Value<S, T::Output, U::Output>;

	fn pow(self, rhs: Integer) -> Self::Output {
		Value {
			value: self.value.pow(rhs.clopy()),
			unit: self.unit.pow(rhs),
			_marker: PhantomData,
		}
	}
}

#[cfg(feature = "big-arith")]
impl<S: UnitSystem, T, U: UnitForValue<S, T>> Pow<i64> for Value<S, T, U>
where
	T: Pow<i64>,
	U: Pow<i64>,
	<U as Pow<i64>>::Output: UnitForValue<S, <T as Pow<i64>>::Output>,
{
	type Output = Value<S, T::Output, U::Output>;

	fn pow(self, rhs: i64) -> Self::Output {
		Value {
			value: self.value.pow(rhs),
			unit: self.unit.pow(rhs),
			_marker: PhantomData,
		}
	}
}

impl<S: UnitSystem, T, U: UnitForValue<S, T>, U2> Pow<f64> for Value<S, T, U>
where
	T: Pow<f64>,
	U: Pow<f64, Output = Option<U2>>,
	U2: UnitForValue<S, <T as Pow<f64>>::Output>,
{
	type Output = Option<Value<S, T::Output, U2>>;

	fn pow(self, rhs: f64) -> Self::Output {
		Some(Value {
			value: self.value.pow(rhs),
			unit: self.unit.pow(rhs)?,
			_marker: PhantomData,
		})
	}
}

impl<S: UnitSystem, T1, T2, U1: UnitForValue<S, T1>, U2: UnitForValue<S, T2>> Div<Value<S, T2, U2>>
	for Value<S, T1, U1>
where
	T1: Div<T2>,
	U1: Div<U2>,
	<U1 as Div<U2>>::Output: UnitForValue<S, <T1 as Div<T2>>::Output>,
{
	type Output = Value<S, T1::Output, U1::Output>;

	fn div(self, rhs: Value<S, T2, U2>) -> Self::Output {
		Value {
			value: self.value / rhs.value,
			unit: self.unit / rhs.unit,
			_marker: PhantomData,
		}
	}
}

impl<S: UnitSystem, T, U: UnitForValue<S, T>> Inv for Value<S, T, U>
where
	T: Inv,
	U: Inv,
	<U as Inv>::Output: UnitForValue<S, <T as Inv>::Output>,
{
	type Output = Value<S, T::Output, U::Output>;

	fn inv(self) -> Self::Output {
		Value {
			value: self.value.inv(),
			unit: self.unit.inv(),
			_marker: PhantomData,
		}
	}
}

impl<S: UnitSystem, T1, T2, U1: UnitForValue<S, T1>, U2: UnitForValue<S, T2>> Add<Value<S, T2, U2>>
	for Value<S, T1, U1>
where
	T1: Add<T2> + Into<T1::Output>,
	U1: PartialEq<U2> + UnitForValue<S, T1::Output>,
{
	type Output = Value<S, T1::Output, U1>;

	fn add(self, rhs: Value<S, T2, U2>) -> Self::Output {
		if self.unit != rhs.unit {
			panic!("tried to add values with different units");
		}

		Value {
			value: self.value + rhs.value,
			unit: self.unit,
			_marker: PhantomData,
		}
	}
}

impl<S: UnitSystem, T1, T2, U1: UnitForValue<S, T1>, U2: UnitForValue<S, T2>> Sub<Value<S, T2, U2>>
	for Value<S, T1, U1>
where
	T1: Sub<T2> + Into<T1::Output>,
	U1: PartialEq<U2> + UnitForValue<S, T1::Output>,
{
	type Output = Value<S, T1::Output, U1>;

	fn sub(self, rhs: Value<S, T2, U2>) -> Self::Output {
		if self.unit != rhs.unit {
			panic!("tried to subtract values with different units");
		}

		Value {
			value: self.value - rhs.value,
			unit: self.unit,
			_marker: PhantomData,
		}
	}
}

impl<S: UnitSystem, T> Zero for Value<S, T, ScalableUnit<S, T>>
where
	S::BaseQuantity: Hash + Eq,
	S::BaseUnit: Hash + Eq,
	T: Zero + One + Div<Output = T> + Clone + PartialEq,
{
	fn zero() -> Self {
		Self {
			value: T::zero(),
			unit: ScalableUnit::new_dimensionless(T::one()),
			_marker: PhantomData,
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
