use std::borrow::Cow;
use std::hash::Hash;
use std::ops::{Div, Mul};

use derivative::Derivative;
use num_traits::{Inv, One};

use crate::composite::Composite;
use crate::quantities::{Quantity, SystemOfQuantities};
use crate::Rational;

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

pub mod si {
	use std::borrow::Cow;

	use super::{BaseUnit as BaseUnitT, Unit, UnitSystem};
	use crate::quantities::isq::{BaseQuantity, ISQ};
	use crate::quantities::Quantity;

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

	#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
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

	impl Unit<SI> {
		pub fn newton() -> Self {
			Self::from_quantity(Quantity::force())
		}

		pub fn joule() -> Self {
			Self::from_quantity(Quantity::energy())
		}

		pub fn watt() -> Self {
			Self::from_quantity(Quantity::power())
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
	Debug(bound = "U::BaseUnit: std::fmt::Debug"),
	Clone(bound = "U::BaseUnit: Clone"),
	PartialEq(bound = "U::BaseUnit: Eq + Hash"),
	Eq(bound = "U::BaseUnit: Eq + Hash"),
	Default(bound = "")
)]
#[repr(transparent)]
pub struct Unit<U: UnitSystem>(pub(crate) Composite<U::BaseUnit>);

impl<U: UnitSystem> Mul for Unit<U>
where
	U::BaseUnit: Hash + Eq,
{
	type Output = Self;

	fn mul(self, rhs: Self) -> Self::Output {
		Self(self.0 * rhs.0)
	}
}

impl<U: UnitSystem> Div for Unit<U>
where
	U::BaseUnit: Hash + Eq,
{
	type Output = Self;

	fn div(self, rhs: Self) -> Self::Output {
		Self(self.0 / rhs.0)
	}
}

impl<U: UnitSystem> Inv for Unit<U> {
	type Output = Self;

	fn inv(self) -> Self::Output {
		Self(self.0.inv())
	}
}

impl<U: UnitSystem> Unit<U> {
	pub fn one() -> Self {
		Self::default()
	}
}

impl<U: UnitSystem> Unit<U> {
	pub fn into_quantity(self) -> Quantity<U::SystemOfQuantities>
	where
		U::BaseQuantity: Hash + Eq,
	{
		Quantity(self.0.map_keys(U::base_quantity))
	}

	pub fn to_quantity(&self) -> Quantity<U::SystemOfQuantities>
	where
		U::BaseQuantity: Hash + Eq,
		U::BaseUnit: Clone,
	{
		Quantity(self.0.clone().map_keys(U::base_quantity))
	}
}

impl<U: UnitSystem> Unit<U>
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
	pub unit: Unit<U>,
}

impl<U: UnitSystem, S> ScalableUnit<U, S> {
	pub fn new_dimensionless(scale: S) -> Self {
		Self {
			scale,
			unit: Unit::one(),
		}
	}

	pub fn to_quantity(&self) -> Quantity<U::SystemOfQuantities>
	where
		U::BaseQuantity: Hash + Eq,
		U::BaseUnit: Clone,
	{
		self.unit.to_quantity()
	}

	pub fn has_dimension<D>(&self, dim: D) -> bool
	where
		U::BaseUnit: Hash + Eq,
		D: Into<Quantity<U::SystemOfQuantities>>,
	{
		self.unit == Unit::from_quantity(dim.into())
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

impl<U: UnitSystem, S: One> From<Unit<U>> for ScalableUnit<U, S> {
	fn from(unit: Unit<U>) -> Self {
		Self {
			scale: S::one(),
			unit,
		}
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
