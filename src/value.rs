use std::cmp::Ordering;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::{Add, Div, Mul, Neg, Sub};

use derivative::Derivative;
use num_traits::{FromPrimitive, Inv, One, Pow, ToPrimitive, Zero};

use crate::si::SI;
use crate::unit::si::{Prefix, PrefixCompatible};
use crate::unit::{AffineUnit, ScalableUnit, SimpleUnit, UnitForValue, UnitSystem};
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
		assert!(
			target.is_of(self.unit.quantity()),
			"invalid unit conversion"
		);
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

impl<S: UnitSystem, T1, T2, U1: UnitForValue<S, T1>, U2: UnitForValue<S, T2>>
	PartialOrd<Value<S, T2, U2>> for Value<S, T1, U1>
where
	T1: PartialOrd<T2> + Clone,
	U1: PartialEq<U2>,
	T2: Clone,
	S::BaseQuantity: Hash + Eq,
	S::BaseUnit: Hash + Eq,
{
	fn partial_cmp(&self, other: &Value<S, T2, U2>) -> Option<Ordering> {
		if self.unit == other.unit {
			self.value.partial_cmp(&other.value)
		} else {
			self.unit
				.convert_to_simple(self.value.clone())
				.partial_cmp(&other.unit.convert_to_simple(other.value.clone()))
		}
	}
}

impl<S: UnitSystem, T, U: UnitForValue<S, T>> Ord for Value<S, T, U>
where
	T: Ord + Clone,
	U: Eq,
	S::BaseQuantity: Hash + Eq,
	S::BaseUnit: Hash + Eq,
{
	fn cmp(&self, other: &Value<S, T, U>) -> Ordering {
		if self.unit == other.unit {
			self.value.cmp(&other.value)
		} else {
			self.unit
				.convert_to_simple(self.value.clone())
				.cmp(&other.unit.convert_to_simple(other.value.clone()))
		}
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
		// todo: auto conversion
		if self.unit != rhs.unit {
			panic!("tried to subtract values with different units",);
		}

		Value {
			value: self.value - rhs.value,
			unit: self.unit,
			_marker: PhantomData,
		}
	}
}

impl<S: UnitSystem, T, U: UnitForValue<S, T>> Neg for Value<S, T, U>
where
	T: Neg,
	U: UnitForValue<S, T::Output>,
{
	type Output = Value<S, T::Output, U>;

	fn neg(self) -> Self::Output {
		self.map_value(Neg::neg)
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

impl<S: UnitSystem, T, Z, O> Value<S, T, AffineUnit<S, Z, O>>
where
	AffineUnit<S, Z, O>: UnitForValue<S, T>,
{
	pub fn untranslate(self) -> Value<S, T, ScalableUnit<S, Z>>
	where
		T: Add<O, Output = T>,
		ScalableUnit<S, Z>: UnitForValue<S, T>,
	{
		let (value, unit) = self.unit.untranslate(self.value);
		Value {
			value,
			unit,
			_marker: PhantomData,
		}
	}
}

impl<T, S> Value<SI, T, ScalableUnit<SI, S>>
where
	T: Mul<S, Output = T> + Div<S, Output = T>,
	S: Clone,
{
	pub fn into_with_si_prefix(
		self,
		allow_sub_thousand: bool,
	) -> Value<SI, PrefixCompatible<T>, ScalableUnit<SI, Prefix>>
	where
		T: FromPrimitive + Div<Output = T> + Mul<Output = T>,
		S: FromPrimitive + ToPrimitive,
	{
		let (fac, scale) = Prefix::from_f32(self.unit.scale.to_f32().unwrap(), allow_sub_thousand);

		let unit = ScalableUnit {
			scale,
			unit: self.unit.unit,
		};

		Value {
			value: PrefixCompatible(self.value * T::from_f32(fac).unwrap()),
			unit,
			_marker: PhantomData,
		}
	}
}

impl<S: UnitSystem, T, Sc> Value<S, T, ScalableUnit<S, Sc>>
where
	T: Mul<Sc, Output = T> + Div<Sc, Output = T>,
	Sc: Clone,
	S::BaseQuantity: Hash + Eq,
	S::BaseUnit: Hash + Eq,
{
	pub fn apply_scale(self) -> Value<S, T, SimpleUnit<S>>
	where
		S::BaseUnit: Clone,
	{
		let unit = self.unit.unit.clone();
		self.into_unit(unit)
	}
}

impl<S: UnitSystem, T> Value<S, T, SimpleUnit<S>>
where
	S::BaseQuantity: Hash + Eq,
	S::BaseUnit: Hash + Eq,
{
	pub fn into_scalable<Sc>(self) -> Value<S, T, ScalableUnit<S, Sc>>
	where
		T: Mul<Sc, Output = T> + Div<Sc, Output = T>,
		Sc: Clone + One,
	{
		Value {
			value: self.value,
			unit: ScalableUnit::from(self.unit),
			_marker: PhantomData,
		}
	}
}

pub fn to_lowest_scale<S: UnitSystem, T, Sc>(
	v: &mut Vec<Value<S, T, ScalableUnit<S, Sc>>>,
) -> Option<ScalableUnit<S, Sc>>
where
	S::BaseQuantity: Hash + Eq,
	S::BaseUnit: Hash + Eq + Clone,
	T: Clone + Mul<Sc, Output = T> + Div<Sc, Output = T>,
	Sc: Clone + PartialOrd + Div<Output = Sc>,
{
	if v.is_empty() {
		return None;
	}
	// check same unit
	let mut unit = None;
	let mut lowest_scale: Option<&Sc> = None;
	for val in &*v {
		match unit {
			None => unit = Some(&val.unit.unit),
			Some(un) => {
				if val.unit.unit != *un {
					// only allowed with vecs of the same unit
					// todo: error enum
					return None;
				}
			}
		}
		lowest_scale = Some(match lowest_scale {
			None => &val.unit.scale,
			Some(sc) if val.unit.scale < *sc => &val.unit.scale,
			Some(sc) => sc,
		});
	}
	let lowest_scale = lowest_scale.unwrap().clone();
	let unit = unit.unwrap().clone();

	for v in v {
		let fac = v.unit.scale.clone() / lowest_scale.clone();
		v.unit.scale = lowest_scale.clone();
		v.value = v.value.clone() * fac;
	}
	Some(ScalableUnit {
		scale: lowest_scale,
		unit,
	})
}
