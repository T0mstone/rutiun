use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;
use std::ops::{Div, Mul};

use derivative::Derivative;
use num_traits::{Inv, One, Pow, Zero};

use crate::{Clopy, Rational};

#[derive(Debug, Clone, Derivative)]
#[derivative(Default(bound = ""), Eq(bound = "K: Eq + Hash"))]
pub struct Composite<K> {
	// maybe-todo: some sort of array storage option (to leverage Copy)
	pub powers: HashMap<K, Rational>,
}

impl<K: Eq + Hash> PartialEq for Composite<K> {
	fn eq(&self, other: &Self) -> bool {
		for (k, v) in self.powers.iter() {
			if other
				.powers
				.get(k)
				.map_or_else(|| !v.is_zero(), |v2| v != v2)
			{
				// if the value is different in other, return false
				// if the value doesn't exist in other, return false except when v == 0
				return false;
			}
		}
		// the same thing but backwards
		for (k, v) in other.powers.iter() {
			if self
				.powers
				.get(k)
				.map_or_else(|| !v.is_zero(), |v2| v != v2)
			{
				return false;
			}
		}
		true
	}
}

impl<K> Composite<K> {
	pub fn new_base_pow(base: K, power: Rational) -> Self
	where
		K: Eq + Hash,
	{
		let mut powers = HashMap::with_capacity(1);
		powers.insert(base, power);
		Self { powers }
	}

	pub fn new_base(base: K) -> Self
	where
		K: Eq + Hash,
	{
		Self::new_base_pow(base, Rational::one())
	}

	pub fn map_keys<F: FnMut(K) -> L, L: Eq + Hash>(self, mut f: F) -> Composite<L> {
		Composite {
			powers: self.powers.into_iter().map(|(k, v)| (f(k), v)).collect(),
		}
	}
}

impl<K> Mul for Composite<K>
where
	K: Eq + Hash,
{
	type Output = Self;

	fn mul(self, rhs: Self) -> Self::Output {
		let mut powers = self.powers;
		for (k, v) in rhs.powers {
			#[allow(clippy::suspicious_arithmetic_impl)]
			match powers.entry(k) {
				Entry::Occupied(mut e) => *e.get_mut() += v,
				Entry::Vacant(e) => {
					e.insert(v);
				}
			}
		}

		Self { powers }
	}
}

impl<K> Inv for Composite<K> {
	type Output = Self;

	fn inv(mut self) -> Self::Output {
		self.powers.values_mut().for_each(|v| {
			*v = -v.clopy();
		});

		self
	}
}

impl<K> Div for Composite<K>
where
	K: Eq + Hash,
{
	type Output = Self;

	#[allow(clippy::suspicious_arithmetic_impl)]
	fn div(self, rhs: Self) -> Self::Output {
		self * rhs.inv()
	}
}

impl<K> Pow<Rational> for Composite<K> {
	type Output = Self;

	fn pow(mut self, rhs: Rational) -> Self::Output {
		self.powers.values_mut().for_each(|v| {
			*v *= rhs.clopy();
		});

		self
	}
}
