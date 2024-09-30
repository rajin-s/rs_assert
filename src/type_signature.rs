use std::hash::{ Hash, Hasher, DefaultHasher };



/// A runtime representation of a given type's data layout and naming

#[derive(Debug, Clone)]
pub struct TypeInfo
{
	name				: &'static str,
	byte_count 			: usize,
	alignment_size 		: usize,
	elements 			: &'static [TypeElement],
}

/// A type referenced within another type

#[derive(Debug, Clone)]
pub enum TypeElement
{
	Named(&'static str, &'static TypeInfo),
	Unnamed(&'static TypeInfo),
}

// Helper structs to differentiate "name-sensitive hash" vs one purely
//  based on size and elements (including element order)

struct HashTypeInfoWithNames<'a>(&'a TypeInfo);
struct HashTypeInfoWithoutNames<'a>(&'a TypeInfo);

impl<'a> Hash for HashTypeInfoWithNames<'a>
{
	fn hash<H>(&self, h : &mut H)
	where
		H : Hasher
	{
		let Self(info) = self;

		info.name.hash(h);
		info.byte_count.hash(h);
		info.alignment_size.hash(h);

		for element in info.elements
		{
			match element
			{
				TypeElement::Named(name, element_info) =>
				{
					name.hash(h);
					Self(element_info).hash(h);
				}
				TypeElement::Unnamed(element_info) =>
				{
					Self(element_info).hash(h);
				}
			}
		}
	}
}

impl<'a> Hash for HashTypeInfoWithoutNames<'a>
{
	fn hash<H>(&self, h : &mut H)
	where
		H : Hasher
	{
		let Self(info) = self;

		// NOTE (rs) "Without names" only applies to field names. The
		//  name of the type itself is still important! Otherwise, there
		//  would be no way to differentiate eg. f32 vs i32

		info.name.hash(h);

		info.byte_count.hash(h);
		info.alignment_size.hash(h);

		for element in info.elements
		{
			match element
			{
				TypeElement::Named(_, element_info)
				| TypeElement::Unnamed(element_info) => Self(element_info).hash(h),
			}
		}
	}
}



/// Trait to provide info about a type. `TYPE_NAME` must be provided manually,
/// as well as a slice reference for `TYPE_ELEMENTS` if the type references any
/// other types.
/// 
/// Typically, this will be generated via `#[derive(GetTypeInfo)]`. Note that
/// self-referential types are not supported (including in chains of references like
/// `A` -> `B` -> `C` -> `A`).

pub trait GetTypeInfo : Sized
{
	const TYPE_NAME 	: &'static str;
	const TYPE_ELEMENTS : &[TypeElement] = &[];

	const TYPE_INFO 	: TypeInfo = TypeInfo
	{
		name			: Self::TYPE_NAME,
		byte_count 		: std::mem::size_of::<Self>(),
		alignment_size 	: std::mem::align_of::<Self>(),
		elements 		: Self::TYPE_ELEMENTS,
	};

	/// Borrows the full type info struct for inspection

	fn type_info() -> &'static TypeInfo
	{
		&Self::TYPE_INFO
	}

	/// Computes a hash for the type hierarchy, taking into account
	/// type names and layout. Notably, this ignores element names so
	/// two structs like:
	/// 
	///     struct Foo
	///     {
	///         id    : u32,
	///         bytes : Vec<u8>,
	///     }
	///     
	///     struct Bar
	///     {
	///         index      : u32,
	///         patch_data : Vec<u8>,
	///     }
	/// 
	/// will end up with the same type hash since they're equivalent data,
	/// even if the intended use is different. The "newtype" pattern can
	/// be used to differeniate otherwise identical types if needed.

	fn type_hash() -> u64
	{
		let mut h = DefaultHasher::new();
		HashTypeInfoWithoutNames(Self::type_info()).hash(&mut h);

		h.finish()
	}

	/// Like `type_hash`, but takes fields' names into account, so two
	/// types with the same layout will end up producing different hashes
	/// unless all members' names are the same *in addition to* the names
	/// of the types themselves.

	fn named_type_hash() -> u64
	{
		let mut h = DefaultHasher::new();
		HashTypeInfoWithNames(Self::type_info()).hash(&mut h);

		h.finish()
	}
}



/// The `TypeElements` trait is used to define a const array of elements
/// for `GetTypeInfo` to reference. It is also typically auto-generated
/// via `#[derive(GetTypeInfo)]`

pub trait TypeElements<const COUNT : usize>
{
	const TYPE_ELEMENT_ARRAY : [TypeElement; COUNT];
}



// Implement type info for atomic types (ie. without references) so they
//  can be referenced in complex types

macro_rules! impl_get_type_info_atomic
{
	{ $($ty:ty),*$(,)? } =>
	{
		$(
			impl GetTypeInfo for $ty
			{
				const TYPE_NAME : &'static str = stringify!($ty);
			}
		)*
	};
}

impl_get_type_info_atomic!
{
	i8, i16, i32, i64, isize,
	u8, u16, u32, u64, usize,
	f32, f64,
	bool,
	String, &'static str,
}



// Implement type info for simple (single-value) reference types so they
//  can be referenced in complex types

macro_rules! impl_get_type_info_single
{
	{ $($generic:ident),*$(,)? } =>
	{
		$(
			impl<T> GetTypeInfo for $generic<T>
			where
				T : GetTypeInfo
			{
				const TYPE_NAME 	: &'static str = stringify!($generic);
				const TYPE_ELEMENTS : &[TypeElement] = &Self::TYPE_ELEMENT_ARRAY;
			}

			impl<T> TypeElements<1> for $generic<T>
			where
				T : GetTypeInfo
			{
				const TYPE_ELEMENT_ARRAY : [TypeElement; 1] =
				[
					TypeElement::Unnamed(&T::TYPE_INFO),
				];
			}
		)*
	}
}

impl_get_type_info_single!
{
	Vec, Box, Option,
}