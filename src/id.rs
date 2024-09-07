#[allow(unused_imports)]
use crate::*;

use std::hash::{ Hash, Hasher, DefaultHasher, BuildHasher };



/// For use with `Id`, to specify different named types.
/// 
/// ```rust
/// use rsrs::{ Id, IdType };
/// 
/// struct Object
/// {
///     name : Id<Self>
/// }
/// 
/// impl IdType for Object
/// {
///     fn id_display_prefix() -> &'static str
///     {
///         "obj:"
///     }
/// }
/// ```

pub trait IdType
{
	fn id_display_prefix() -> &'static str;
}



/// An `Id` is effectively a pre-hashed string, for efficient comparison, especially when
/// using pre-computed `Id`s (eg. loaded from a file).
/// 
/// Enabling the `name_table` feature will allow reverse lookup of any name that has been
/// computed at some point during program execution, so it can be displayed in a human-
/// readable format. This is generally expected to be an uncommon / debug-only operation.
/// 
/// The type parameter is used to differeniate different sources of identifiers for more
/// robust APIs. All types of `Id` end up in the same generic name table for reverse-lookup.

pub struct Id<T>
{
	value 	: u64,
	_t		: std::marker::PhantomData<T>,
}

impl<T> Id<T>
{
	pub fn compute_hash(text : &str) -> u64
	{
		let mut hasher = DefaultHasher::new();
		text.hash(&mut hasher);

		hasher.finish()
	}

	pub fn new(text : &str) -> Self
	{
		let id = Self
		{
			value 	: Self::compute_hash(text),
			_t		: Default::default(),
		};

		id_internal::try_ensure_cached_name(id, text);

		id
	}

	pub fn from_raw_value(value : u64) -> Self
	{
		Self
		{
			value,
			_t : Default::default(),
		}
	}
	
	pub fn from<Other>(id : Id<Other>) -> Self
	{
		Self::from_raw_value(id.raw_value())
	}

	pub fn raw_value(&self) -> u64
	{
		self.value
	}

	#[cfg(feature="name_cache")]
	pub fn lookup_str(&self) -> &'static str
	{
		id_internal::try_lookup_cached_name(*self)
	}
}

impl<T> Clone for Id<T>
{
	fn clone(&self) -> Self
	{
		*self
	}
}

impl<T> Copy for Id<T> {}

impl<T> PartialEq for Id<T>
{
	fn eq(&self, other : &Self) -> bool
	{
		self.value == other.value
	}
}

impl<T> Eq for Id<T> {}

impl<T> PartialOrd for Id<T>
{
	fn partial_cmp(&self, other : &Self) -> Option<std::cmp::Ordering>
	{
		Some(self.cmp(other))
	}
}

impl<T> Ord for Id<T>
{
	fn cmp(&self, other : &Self) -> std::cmp::Ordering
	{
		self.value.cmp(&other.value)
	}
}

impl<T> Hash for Id<T>
{
	fn hash<H : Hasher>(&self, hasher : &mut H)
	{
		self.value.hash(hasher)
	}
}

impl<T> std::fmt::Display for Id<T>
	where T : IdType
{
	fn fmt(&self, formatter : &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!
		{
			formatter,
			"{}{}",
				T::id_display_prefix(),
				id_internal::try_lookup_cached_name(*self),
		}
	}
}

impl<T> std::fmt::Debug for Id<T>
	where T : IdType
{
	fn fmt(&self, formatter : &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(formatter, "{}:{:0x}", self, self.raw_value())
	}
}



pub type IdMap<T, Value> =
	std::collections::HashMap<Id<T>, Value, id_internal::BuildIdHasher>;

pub trait ExtIdMap
{
	fn new() -> Self;
	fn with_capacity(capacity : usize) -> Self;
}

impl<T, TId> ExtIdMap for IdMap<T, TId>
{
	fn new() -> Self
	{
		Self::with_hasher(id_internal::BuildIdHasher::new())
	}
	
	fn with_capacity(capacity : usize) -> Self
	{
		Self::with_capacity_and_hasher(capacity, id_internal::BuildIdHasher::new())
	}
}



// Generic `Name` type can be used for convenience when disambiguation between
//  types of named objects isn't important

impl IdType for ()
{
	fn id_display_prefix() -> &'static str
	{
		"#"
	}
}

pub type Name = Id<()>;
pub type NameMap<Value> = IdMap<(), Value>;



mod id_internal
{
	use super::*;

	/// `Id`s are pre-hashed. `IdHasher` is thus a special `Hasher` type that just
	/// passes through that hashed value. It's not generally safe to use with any
	/// other type.

	#[derive(Default)]
	pub struct IdHasher
	{
		value : u64,
	}
	
	impl IdHasher
	{
		pub fn new() -> Self
		{
			Self
			{
				value : 0,
			}
		}
	}
	
	impl Hasher for IdHasher
	{
		fn finish(&self) -> u64
		{
			self.value
		}
		
		fn write(&mut self, bytes: &[u8])
		{
			debug_assert_eq!(self.value, 0);
			debug_assert_eq!(bytes.len(), 8);
	
			let bytes_as_u64 = bytes as *const [u8] as *const u8 as *const u64;
			let new_value = unsafe { *bytes_as_u64 };
	
			self.value = new_value;
		}
	}
	
	#[derive(Default)]
	pub struct BuildIdHasher {}
	
	impl BuildIdHasher
	{
		pub fn new() -> Self
		{
			Self {}
		}
	}
	
	impl BuildHasher for BuildIdHasher
	{
		type Hasher = IdHasher;
	
		fn build_hasher(&self) -> Self::Hasher
		{
			IdHasher::new()
		}
	}

	#[cfg(feature="name_cache")]
	lazy_static::lazy_static!
	{
		pub static ref NAME_TABLE : std::sync::RwLock<NameMap<String>> =
			std::sync::RwLock::new(IdMap::new());
	}

	#[cfg(feature="name_cache")]
	pub const UNKNOWN_NAME_STRING : &str = "[unknown]";
	pub const UNAVAILABLE_NAME_STRING : &str = "[unavailable]";

	#[allow(unused_variables)]
	pub fn try_ensure_cached_name<T>(id : Id<T>, text : &str)
	{
		#[cfg(feature="name_cache")]
		if let Ok(mut name_map) = NAME_TABLE.write()
		{
			let name = Name::from(id);
			name_map.entry(name).or_insert_with(|| String::from(text));
		}
	}

	#[allow(unused_variables)]
	pub fn try_lookup_cached_name<T>(id : Id<T>) -> &'static str
	{
		#[cfg(feature="name_cache")]
		{
			let name_map = match NAME_TABLE.read()
			{
				Ok(x) 	=> x,
				Err(_) 	=> return UNAVAILABLE_NAME_STRING,
			};
	
			let name = Name::from(id);

			// NOTE (rs) We never remove from NAME_TABLE or modify existing entries,
			//  and the underlying string data will never be moved, so it's safe to
			//  treat values as constant static references.

			return match name_map.get(&name)
			{
				Some(text) 	=> unsafe { &*(text.as_str() as *const str) as &'static str }
				None 		=> UNKNOWN_NAME_STRING,
			};
		}

		#[cfg(not(feature="name_cache"))]
		return UNAVAILABLE_NAME_STRING;
	}
}

#[allow(unused_variables)]
pub fn init_name_cache<'a, StringIter>(name_strings : StringIter)
	where StringIter : std::iter::Iterator<Item = &'a &'a str> + Clone
{
	#[cfg(feature="name_cache")]
	{
		if let Ok(mut name_map) = id_internal::NAME_TABLE.write()
		{
			name_map.reserve(name_strings.clone().count());
		}
	
		for name_string in name_strings
		{
			let _ = Name::new(name_string);
		}
	}
}



// Tests

#[test]
fn test_name()
{
	let name = Name::new("hello");
	let expected_value = 16156531084128653017u64;

	ASSERT!
	{
		name.raw_value() == expected_value,
		"Expected {}, found {}",
			expected_value,
			name.raw_value(),
	}
}

#[test]
fn test_name_types()
{
	let name = Name::new("hello");
	let expected_value = 16156531084128653017u64;

	struct Foo {}
	impl IdType for Foo
	{
		fn id_display_prefix() -> &'static str
		{
			"foo"
		}
	}

	let foo_id : Id<Foo> = Id::new("hello");

	ASSERT!
	{
		foo_id.raw_value() == expected_value,
		"Expected {}, found {}",
			expected_value,
			name.raw_value(),
	}

	ASSERT! { foo_id.raw_value() == name.raw_value() }

	println!("{} == {}", name, foo_id);
}

#[test]
fn test_name_map()
{
	let name_strings = ["foo", "bar", "baz"];
	let mut name_map = IdMap::with_capacity(1);

	for &name_string in name_strings.iter()
	{
		name_map.insert(Name::new(name_string), String::from(name_string));
	}

	for (name, value) in name_map.iter()
	{
		let mut h = id_internal::BuildIdHasher::new().build_hasher();
		name.hash(&mut h);
		let hash_value = h.finish();
		
		ASSERT! { name.raw_value() == hash_value }

		#[cfg(feature="name_cache")]
		ASSERT! { name.lookup_str() == value }
		let _ = value;

		println!
		{
			"{:?} == {:0x} == {}",
				name,
				hash_value,
				value,
		}
	}

	let name_to_check = Name::new("foo");
	ASSERT! { name_map.contains_key(&name_to_check) };
}

#[cfg(feature="name_cache")]
#[test]
fn test_init_name_cache()
{
	init_name_cache([].into_iter());

	let init_names = ["these", "are", "test", "names"];
	init_name_cache(init_names.iter());

	for &name_string in init_names.iter()
	{
		let raw_value = Name::compute_hash(name_string);
		let no_cache_name = Name::from_raw_value(raw_value);

		match no_cache_name.lookup_str()
		{
			id_internal::UNKNOWN_NAME_STRING |
			id_internal::UNAVAILABLE_NAME_STRING =>
			{
				FAIL!
				{
					"Failed to get cached text for {} = {:?}",
						name_string,
						no_cache_name
				};
			}
			cached_string =>
			{
				println!("{} == {:?}", name_string, no_cache_name);
				ASSERT! { name_string == cached_string }
			}
		}
	}
}

#[cfg(feature="name_cache")]
#[test]
fn test_name_lookup()
{
	let name_string = "foo";

	let name = Name::new(name_string);
	ASSERT! { name.lookup_str() == name_string }
	
	let name2 = Name::new(name_string);
	ASSERT! { name2.lookup_str() == name_string }
}