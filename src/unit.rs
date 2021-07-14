use std::borrow::Cow;
use std::fmt;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::{Add, Div, Mul, Sub};

use derivative::Derivative;
use num_traits::{FromPrimitive, Inv, One, Pow, Zero};

use crate::composite::Composite;
use crate::quantities::{Quantity, SystemOfQuantities};
use crate::{Clopy, Integer, Rational};

// note: IntelliJ Rust is not advanced enough for this file (8 false positive errors at the time of writing),
// so always resort to cargo check instead!

pub trait BaseUnit {
	fn long_name(&self) -> Cow<'static, str>;
	fn short_name(&self) -> Cow<'static, str>;
}

pub trait UnitSystem {
	type SystemOfQuantities: SystemOfQuantities<BaseQuantity = Self::BaseQuantity>;
	type BaseQuantity;
	type BaseUnit: BaseUnit;

	fn base_unit(q: Self::BaseQuantity) -> Self::BaseUnit;
	fn base_quantity(u: Self::BaseUnit) -> Self::BaseQuantity;
}

pub trait Unit<U: UnitSystem> {
	fn quantity(self) -> Quantity<U::SystemOfQuantities>;
	fn is_of<Q: Into<Quantity<U::SystemOfQuantities>>>(&self, q: Q) -> bool;
}

pub trait UnitForValue<U: UnitSystem, V>: Unit<U> {
	fn convert_to_simple(&self, val: V) -> V;
	fn convert_from_simple(&self, val: V) -> V;
}

pub trait UnitSymbols<U: Unit<Self>>: UnitSystem + Sized {
	fn format(u: &U) -> String;
}

pub trait UnitExt<U: UnitSystem>: Unit<U> {
	fn display(self) -> UnitSymbol<U, Self>
	where
		Self: Sized,
	{
		UnitSymbol(self, PhantomData)
	}
}

impl<S: UnitSystem, U: Unit<S>> UnitExt<S> for U {}

pub mod si {
	use std::borrow::Cow;

	use num_traits::{FromPrimitive, One, Pow, Signed, Zero};

	use super::{BaseUnit as BaseUnitT, SimpleUnit, UnitSystem};
	use crate::quantities::isq::{BaseQuantity, ISQ};
	use crate::quantities::Quantity;
	use crate::unit::{AffineUnit, ScalableUnit, UnitSymbols};
	use crate::{Integer, Rational};

	pub struct SI;

	impl UnitSystem for SI {
		type SystemOfQuantities = ISQ;
		type BaseQuantity = BaseQuantity;
		type BaseUnit = BaseUnit;

		fn base_unit(q: Self::BaseQuantity) -> Self::BaseUnit {
			BaseUnit(q)
		}

		fn base_quantity(u: Self::BaseUnit) -> Self::BaseQuantity {
			u.0
		}
	}

	#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
	#[repr(transparent)]
	pub struct BaseUnit(pub BaseQuantity);

	impl BaseUnit {
		pub fn meter() -> Self {
			Self(BaseQuantity::Length)
		}

		pub fn kilogram() -> Self {
			Self(BaseQuantity::Mass)
		}

		pub fn second() -> Self {
			Self(BaseQuantity::Time)
		}

		pub fn ampere() -> Self {
			Self(BaseQuantity::ElectricCurrent)
		}

		pub fn kelvin() -> Self {
			Self(BaseQuantity::Temperature)
		}

		pub fn mole() -> Self {
			Self(BaseQuantity::Substance)
		}

		pub fn candela() -> Self {
			Self(BaseQuantity::LumIntensity)
		}
	}

	impl BaseUnitT for BaseUnit {
		fn long_name(&self) -> Cow<'static, str> {
			Cow::Borrowed(match self.0 {
				BaseQuantity::Length => "meter",
				BaseQuantity::Mass => "kilogram",
				BaseQuantity::Time => "second",
				BaseQuantity::ElectricCurrent => "ampere",
				BaseQuantity::Temperature => "kelvin",
				BaseQuantity::Substance => "mole",
				BaseQuantity::LumIntensity => "candela",
			})
		}

		fn short_name(&self) -> Cow<'static, str> {
			Cow::Borrowed(match self.0 {
				BaseQuantity::Length => "m",
				BaseQuantity::Mass => "kg",
				BaseQuantity::Time => "s",
				BaseQuantity::ElectricCurrent => "A",
				BaseQuantity::Temperature => "K",
				BaseQuantity::Substance => "mol",
				BaseQuantity::LumIntensity => "cd",
			})
		}
	}

	// todo: impl Mul, Div, Inv for BaseUnit (mirroring BaseQuantity)

	impl Pow<Rational> for BaseUnit {
		type Output = SimpleUnit<SI>;

		fn pow(self, rhs: Rational) -> Self::Output {
			SimpleUnit::from_quantity(self.0.pow(rhs))
		}
	}

	impl Pow<Integer> for BaseUnit {
		type Output = SimpleUnit<SI>;

		fn pow(self, rhs: Integer) -> Self::Output {
			self.pow(Rational::from_integer(rhs))
		}
	}

	#[cfg(feature = "big-arith")]
	impl Pow<i64> for BaseUnit {
		type Output = SimpleUnit<SI>;

		fn pow(self, rhs: i64) -> Self::Output {
			self.pow(Integer::from(rhs))
		}
	}

	impl Pow<f64> for BaseUnit {
		type Output = Option<SimpleUnit<SI>>;

		fn pow(self, rhs: f64) -> Self::Output {
			Some(self.pow(Rational::from_f64(rhs)?))
		}
	}

	impl SimpleUnit<SI> {
		pub fn newton() -> Self {
			Self::from_quantity(Quantity::force())
		}

		pub fn joule() -> Self {
			Self::from_quantity(Quantity::energy())
		}

		pub fn watt() -> Self {
			Self::from_quantity(Quantity::power())
		}

		pub fn volt() -> Self {
			Self::from_quantity(Quantity::electric_potential())
		}
	}

	impl<S: From<u32>> ScalableUnit<SI, S> {
		pub fn minute() -> Self {
			Self {
				scale: S::from(60),
				unit: SimpleUnit::new_base(BaseUnit::second()),
			}
		}

		pub fn hour() -> Self {
			Self {
				scale: S::from(3600),
				unit: SimpleUnit::new_base(BaseUnit::second()),
			}
		}
	}

	// todo: SI prefixes

	impl<S: One, O: From<f32>> AffineUnit<SI, S, O> {
		pub fn degree_celsius() -> Self {
			Self {
				scale: S::one(),
				offset: O::from(273.15),
				unit: SimpleUnit::new_base(BaseUnit::kelvin()),
			}
		}
	}

	impl UnitSymbols<SimpleUnit<SI>> for SI {
		fn format(u: &SimpleUnit<SI>) -> String {
			let known_units = [
				(SimpleUnit::joule(), "J"),
				(SimpleUnit::watt(), "W"),
				(SimpleUnit::newton(), "N"),
				(SimpleUnit::volt(), "V"),
			];

			for (u2, s) in &known_units {
				if u == u2 {
					return s.to_string();
				}
			}

			for (u2, s) in known_units {
				let mut res: SimpleUnit<SI> = u.clone() / u2;
				res.0.powers.retain(|_, r| !r.is_zero());
				if res.0.powers.len() == 1 {
					let (unit, power) = res.0.powers.iter().next().unwrap();
					if power.abs().is_one() {
						let delim = if power.is_negative() { "/" } else { " " };
						return format!("{}{}{}", s, delim, unit.short_name());
					}
				}
			}

			// fallback: for the rest just use the usual display impl
			format!("{}", u)
		}
	}

	/*
	pub enum Prefix {
	Deca,
	Hecto,
	Kilo,
	Mega,
	Giga,
	Tera,
	Peta,
	Exa,
	Zetta,
	Yotta,

	Deci,
	Centi,
	Milli,
	Micro,
	Nano,
	Pico,
	Femto,
	Atto,
	Zepto,
	Yocto,
	}

	impl Prefix {
	pub fn tens_exponent(&self) -> i8 {
	use Prefix::*;

	match self {
	Deca => 1,
	Hecto => 2,
	Kilo => 3,
	Mega => 6,
	Giga => 9,
	Tera => 12,
	Peta => 15,
	Exa => 18,
	Zetta => 21,
	Yotta => 24,

	Deci => -1,
	Centi => -2,
	Milli => -3,
	Micro => -6,
	Nano => -9,
	Pico => -12,
	Femto => -15,
	Atto => -18,
	Zepto => -21,
	Yocto => -24,
	}
	}

	pub fn symbol(&self) -> &'static str {
	use Prefix::*;

	match self {
	Deca => "da",
	Hecto => "h",
	Kilo => "k",
	Mega => "M",
	Giga => "G",
	Tera => "T",
	Peta => "P",
	Exa => "E",
	Zetta => "Z",
	Yotta => "Y",

	Deci => "d",
	Centi => "c",
	Milli => "m",
	Micro => "μ",
	Nano => "n",
	Pico => "p",
	Femto => "f",
	Atto => "a",
	Zepto => "z",
	Yocto => "y",
	}
	}
	}
	*/
}

#[derive(Derivative)]
#[derivative(
	Debug(bound = "U: std::fmt::Debug"),
	Clone(bound = "U: Clone"),
	PartialEq(bound = "U: PartialEq"),
	Eq(bound = "U: Eq"),
	Default(bound = "U: Default")
)]
#[repr(transparent)]
pub struct UnitSymbol<S: UnitSystem, U: Unit<S>>(U, PhantomData<S>);

impl<S: UnitSymbols<U>, U: Unit<S>> fmt::Display for UnitSymbol<S, U> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		f.pad(&S::format(&self.0))
	}
}

#[derive(Derivative)]
#[derivative(
	Debug(bound = "U::BaseUnit: std::fmt::Debug"),
	Clone(bound = "U::BaseUnit: Clone"),
	PartialEq(bound = "U::BaseUnit: Eq + Hash"),
	Eq(bound = "U::BaseUnit: Eq + Hash"),
	Default(bound = "")
)]
#[repr(transparent)]
pub struct SimpleUnit<U: UnitSystem>(pub(crate) Composite<U::BaseUnit>);

impl<U: UnitSystem> fmt::Display for SimpleUnit<U>
where
	U::BaseUnit: Ord,
{
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let mut powers = self.0.powers.iter().collect::<Vec<_>>();
		powers.sort();
		let mut first = true;
		for (u, pow) in powers.into_iter() {
			if first {
				first = false;
			} else {
				f.pad(" ")?;
			}
			f.write_fmt(format_args!("{}^{}", u.short_name(), pow))?;
		}
		Ok(())
	}
}

impl<U: UnitSystem> Unit<U> for SimpleUnit<U>
where
	U::BaseQuantity: Hash + Eq,
	U::BaseUnit: Hash + Eq,
{
	fn quantity(self) -> Quantity<U::SystemOfQuantities> {
		Quantity(self.0.map_keys(U::base_quantity))
	}

	fn is_of<Q: Into<Quantity<U::SystemOfQuantities>>>(&self, q: Q) -> bool {
		*self == Self::from_quantity(q.into())
	}
}

impl<U: UnitSystem, V> UnitForValue<U, V> for SimpleUnit<U>
where
	U::BaseQuantity: Hash + Eq,
	U::BaseUnit: Hash + Eq,
{
	fn convert_to_simple(&self, val: V) -> V {
		val
	}

	fn convert_from_simple(&self, val: V) -> V {
		val
	}
}

impl<U: UnitSystem> Mul for SimpleUnit<U>
where
	U::BaseUnit: Hash + Eq,
{
	type Output = Self;

	fn mul(self, rhs: Self) -> Self::Output {
		Self(self.0 * rhs.0)
	}
}

impl<U: UnitSystem> Pow<Rational> for SimpleUnit<U> {
	type Output = Self;

	fn pow(self, rhs: Rational) -> Self::Output {
		Self(self.0.pow(rhs))
	}
}

impl<U: UnitSystem> Pow<Integer> for SimpleUnit<U> {
	type Output = Self;

	fn pow(self, rhs: Integer) -> Self::Output {
		self.pow(Rational::from_integer(rhs))
	}
}

#[cfg(feature = "big-arith")]
impl<U: UnitSystem> Pow<i64> for SimpleUnit<U> {
	type Output = Self;

	fn pow(self, rhs: i64) -> Self::Output {
		self.pow(Integer::from(rhs))
	}
}

impl<U: UnitSystem> Pow<f64> for SimpleUnit<U> {
	type Output = Option<Self>;

	fn pow(self, rhs: f64) -> Self::Output {
		Some(self.pow(Rational::from_f64(rhs)?))
	}
}

impl<U: UnitSystem> Div for SimpleUnit<U>
where
	U::BaseUnit: Hash + Eq,
{
	type Output = Self;

	fn div(self, rhs: Self) -> Self::Output {
		Self(self.0 / rhs.0)
	}
}

impl<U: UnitSystem> Inv for SimpleUnit<U> {
	type Output = Self;

	fn inv(self) -> Self::Output {
		Self(self.0.inv())
	}
}

impl<U: UnitSystem> SimpleUnit<U> {
	pub fn one() -> Self {
		Self::default()
	}
}

impl<U: UnitSystem> SimpleUnit<U>
where
	U::BaseUnit: Hash + Eq,
{
	pub fn from_quantity(quantity: Quantity<U::SystemOfQuantities>) -> Self {
		Self(quantity.0.map_keys(U::base_unit))
	}

	pub fn new_base_pow(base: U::BaseUnit, power: Rational) -> Self {
		Self(Composite::new_base_pow(base, power))
	}

	pub fn new_base(base: U::BaseUnit) -> Self {
		Self(Composite::new_base(base))
	}
}

#[derive(Derivative)]
#[derivative(
	Clone(bound = "U::BaseUnit: Clone, S: Clone"),
	Debug(bound = "U::BaseUnit: std::fmt::Debug, S: std::fmt::Debug"),
	Eq(bound = "U::BaseUnit: Eq + Hash, S: Eq"),
	Default(bound = "S: Default")
)]
pub struct ScalableUnit<U: UnitSystem, S> {
	pub scale: S,
	pub unit: SimpleUnit<U>,
}

impl<U: UnitSystem, S> ScalableUnit<U, S> {
	pub fn new_dimensionless(scale: S) -> Self {
		Self {
			scale,
			unit: SimpleUnit::one(),
		}
	}

	pub fn to_quantity(&self) -> Quantity<U::SystemOfQuantities>
	where
		U::BaseQuantity: Hash + Eq,
		U::BaseUnit: Clone + Hash + Eq,
	{
		self.unit.clone().quantity()
	}
}

impl<U: UnitSystem, S, T> PartialEq<ScalableUnit<U, T>> for ScalableUnit<U, S>
where
	U::BaseUnit: Eq + Hash,
	S: PartialEq<T>,
{
	fn eq(&self, other: &ScalableUnit<U, T>) -> bool {
		self.scale == other.scale && self.unit == other.unit
	}
}

impl<U: UnitSystem, S> PartialEq<SimpleUnit<U>> for ScalableUnit<U, S>
where
	U::BaseUnit: Eq + Hash,
	S: One + PartialEq,
{
	fn eq(&self, other: &SimpleUnit<U>) -> bool {
		self.scale.is_one() && self.unit == *other
	}
}

impl<U: UnitSystem, S> PartialEq<ScalableUnit<U, S>> for SimpleUnit<U>
where
	U::BaseUnit: Eq + Hash,
	S: One + PartialEq,
{
	fn eq(&self, other: &ScalableUnit<U, S>) -> bool {
		other.scale.is_one() && *self == other.unit
	}
}

impl<U: UnitSystem, S: One> From<SimpleUnit<U>> for ScalableUnit<U, S> {
	fn from(unit: SimpleUnit<U>) -> Self {
		Self {
			scale: S::one(),
			unit,
		}
	}
}

impl<U: UnitSystem, S: One> One for ScalableUnit<U, S>
where
	U::BaseUnit: Hash + Eq,
{
	fn one() -> Self {
		Self::from(SimpleUnit::one())
	}
}

impl<U: UnitSystem, S: Mul<T>, T> Mul<ScalableUnit<U, T>> for ScalableUnit<U, S>
where
	U::BaseUnit: Hash + Eq,
{
	type Output = ScalableUnit<U, S::Output>;

	fn mul(self, rhs: ScalableUnit<U, T>) -> Self::Output {
		ScalableUnit {
			scale: self.scale * rhs.scale,
			unit: self.unit * rhs.unit,
		}
	}
}

impl<U: UnitSystem, S> Mul<SimpleUnit<U>> for ScalableUnit<U, S>
where
	U::BaseUnit: Hash + Eq,
{
	type Output = ScalableUnit<U, S>;

	fn mul(self, rhs: SimpleUnit<U>) -> Self::Output {
		ScalableUnit {
			scale: self.scale,
			unit: self.unit * rhs,
		}
	}
}

impl<U: UnitSystem, S> Mul<ScalableUnit<U, S>> for SimpleUnit<U>
where
	U::BaseUnit: Hash + Eq,
{
	type Output = ScalableUnit<U, S>;

	fn mul(self, rhs: ScalableUnit<U, S>) -> Self::Output {
		ScalableUnit {
			scale: rhs.scale,
			unit: self * rhs.unit,
		}
	}
}

impl<U: UnitSystem, S> Pow<Rational> for ScalableUnit<U, S>
where
	S: Pow<Rational>,
{
	type Output = ScalableUnit<U, S::Output>;

	fn pow(self, rhs: Rational) -> Self::Output {
		ScalableUnit {
			scale: self.scale.pow(rhs.clopy()),
			unit: self.unit.pow(rhs),
		}
	}
}

impl<U: UnitSystem, S> Pow<Integer> for ScalableUnit<U, S>
where
	S: Pow<Integer>,
{
	type Output = ScalableUnit<U, S::Output>;

	fn pow(self, rhs: Integer) -> Self::Output {
		ScalableUnit {
			scale: self.scale.pow(rhs.clopy()),
			unit: self.unit.pow(rhs),
		}
	}
}

#[cfg(feature = "big-arith")]
impl<U: UnitSystem, S> Pow<i64> for ScalableUnit<U, S>
where
	S: Pow<i64>,
{
	type Output = ScalableUnit<U, S::Output>;

	fn pow(self, rhs: i64) -> Self::Output {
		ScalableUnit {
			scale: self.scale.pow(rhs),
			unit: self.unit.pow(rhs),
		}
	}
}

impl<U: UnitSystem, S> Pow<f64> for ScalableUnit<U, S>
where
	S: Pow<f64>,
{
	type Output = Option<ScalableUnit<U, S::Output>>;

	fn pow(self, rhs: f64) -> Self::Output {
		Some(ScalableUnit {
			scale: self.scale.pow(rhs),
			unit: self.unit.pow(Rational::from_f64(rhs)?),
		})
	}
}

impl<U: UnitSystem, S: Div<T>, T> Div<ScalableUnit<U, T>> for ScalableUnit<U, S>
where
	U::BaseUnit: Hash + Eq,
{
	type Output = ScalableUnit<U, S::Output>;

	fn div(self, rhs: ScalableUnit<U, T>) -> Self::Output {
		ScalableUnit {
			scale: self.scale / rhs.scale,
			unit: self.unit / rhs.unit,
		}
	}
}

impl<U: UnitSystem, S: Inv> Inv for ScalableUnit<U, S> {
	type Output = ScalableUnit<U, S::Output>;

	fn inv(self) -> Self::Output {
		ScalableUnit {
			scale: self.scale.inv(),
			unit: self.unit.inv(),
		}
	}
}

impl<U: UnitSystem, S> Unit<U> for ScalableUnit<U, S>
where
	U::BaseQuantity: Hash + Eq,
	U::BaseUnit: Hash + Eq,
{
	fn quantity(self) -> Quantity<U::SystemOfQuantities> {
		self.unit.quantity()
	}

	fn is_of<Q: Into<Quantity<U::SystemOfQuantities>>>(&self, q: Q) -> bool {
		self.unit.is_of(q)
	}
}

impl<U: UnitSystem, S, V> UnitForValue<U, V> for ScalableUnit<U, S>
where
	U::BaseQuantity: Hash + Eq,
	U::BaseUnit: Hash + Eq,
	V: Mul<S, Output = V> + Div<S, Output = V>,
	S: Clone,
{
	fn convert_to_simple(&self, val: V) -> V {
		val * self.scale.clone()
	}

	fn convert_from_simple(&self, val: V) -> V {
		val / self.scale.clone()
	}
}

#[derive(Derivative)]
#[derivative(
	Clone(bound = "U::BaseUnit: Clone, S: Clone, O: Clone"),
	Debug(bound = "U::BaseUnit: std::fmt::Debug, S: std::fmt::Debug, O: std::fmt::Debug"),
	Eq(bound = "U::BaseUnit: Eq + Hash, S: Eq, O: Eq"),
	Default(bound = "S: Default, O: Default")
)]
pub struct AffineUnit<U: UnitSystem, S, O> {
	pub scale: S,
	pub offset: O,
	pub unit: SimpleUnit<U>,
}

impl<U: UnitSystem, S, O> AffineUnit<U, S, O> {
	pub fn new_dimensionless(scale: S, offset: O) -> Self {
		Self {
			scale,
			offset,
			unit: SimpleUnit::one(),
		}
	}

	pub fn to_quantity(&self) -> Quantity<U::SystemOfQuantities>
	where
		U::BaseQuantity: Hash + Eq,
		U::BaseUnit: Clone + Hash + Eq,
	{
		self.unit.clone().quantity()
	}

	#[doc(alias = "into_scalable")]
	pub fn untranslate<V>(self, value: V) -> (V, ScalableUnit<U, S>)
	where
		Self: UnitForValue<U, V>,
		V: Add<O, Output = V>,
	{
		(value + self.offset, ScalableUnit {
			scale: self.scale,
			unit: self.unit,
		})
	}
}

impl<U: UnitSystem, S, T, O, P> PartialEq<AffineUnit<U, T, P>> for AffineUnit<U, S, O>
where
	U::BaseUnit: Eq + Hash,
	S: PartialEq<T>,
	O: PartialEq<P>,
{
	fn eq(&self, other: &AffineUnit<U, T, P>) -> bool {
		self.scale == other.scale && self.offset == other.offset && self.unit == other.unit
	}
}

impl<U: UnitSystem, S: One, O: Zero> From<SimpleUnit<U>> for AffineUnit<U, S, O> {
	fn from(unit: SimpleUnit<U>) -> Self {
		Self {
			scale: S::one(),
			offset: O::zero(),
			unit,
		}
	}
}

impl<U: UnitSystem, S, O> Unit<U> for AffineUnit<U, S, O>
where
	U::BaseQuantity: Hash + Eq,
	U::BaseUnit: Hash + Eq,
{
	fn quantity(self) -> Quantity<U::SystemOfQuantities> {
		self.unit.quantity()
	}

	fn is_of<Q: Into<Quantity<U::SystemOfQuantities>>>(&self, q: Q) -> bool {
		self.unit.is_of(q)
	}
}

impl<U: UnitSystem, S, O, V> UnitForValue<U, V> for AffineUnit<U, S, O>
where
	U::BaseQuantity: Hash + Eq,
	U::BaseUnit: Hash + Eq,
	S: Clone,
	O: Clone,
	V: Mul<S> + Sub<O>,
	<V as Mul<S>>::Output: Add<O, Output = V>,
	<V as Sub<O>>::Output: Div<S, Output = V>,
{
	fn convert_to_simple(&self, val: V) -> V {
		val * self.scale.clone() + self.offset.clone()
	}

	fn convert_from_simple(&self, val: V) -> V {
		(val - self.offset.clone()) / self.scale.clone()
	}
}
