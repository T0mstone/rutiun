use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;
use std::ops::{Div, Mul};

use derivative::Derivative;
use num_traits::{Inv, One};

use crate::Rational;

#[derive(Debug, Clone, Derivative)]
#[derivative(
	Default(bound = ""),
	PartialEq(bound = "K: Eq + Hash"),
	Eq(bound = "K: Eq + Hash")
)]
pub struct Composite<K> {
	pub powers: HashMap<K, Rational>,
}

impl<K> Composite<K> {
	pub fn new_base(base: K) -> Self
	where
		K: Eq + Hash,
	{
		let mut powers = HashMap::with_capacity(1);
		powers.insert(base, Rational::one());
		Self { powers }
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
			#[cfg(feature = "big-rational")]
			{
				*v = -v.clone();
			}
			#[cfg(not(feature = "big-rational"))]
			{
				*v = -*v;
			}
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
