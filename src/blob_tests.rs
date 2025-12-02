use super::*;


#[cfg(not(feature="mode.test"))]
compile_error!("Must be in mode.test for tests to run");



#[test]
fn primitive_layouts()
{
	macro_rules! assert_layout_eq
	{
		($t1:ty, type $t2:ty) =>
		{
			assert!(<$t1>::BYTE_COUNT == <$t2>::BYTE_COUNT);
			assert!(<$t1>::ALIGNMENT_SIZE == <$t2>::ALIGNMENT_SIZE);
		};
		($t1:ty, $generic:ident<$($t_arg:ident),*>) =>
		{
			assert_layout_eq!($t1, $generic<$($t_arg),*> with u8);
			assert_layout_eq!($t1, $generic<$($t_arg),*> with u64);
			assert_layout_eq!($t1, $generic<$($t_arg),*> with String);
			assert_layout_eq!($t1, $generic<$($t_arg),*> with Box<f32>);
			assert_layout_eq!($t1, $generic<$($t_arg),*> with Box<Vec<bool>>);
			assert_layout_eq!($t1, $generic<$($t_arg),*> with Vec<Box<*const u64>>);
			assert_layout_eq!($t1, $generic<$($t_arg),*> with ());
		};
		($t1:ty, $generic:ident<$($t_arg:ident),*> with $t2:ty) =>
		{
			{
				$( type $t_arg = $t2; )*
				assert_layout_eq!($t1, type $generic<$($t_arg),*>);
			}
		};
		($t1:ty, $t2:ty) =>
		{
			assert_layout_eq!($t1, type $t2)
		};
	}

	type ConstPointer<T> = *const T;
	type VecVec<T> = Vec<Vec<T>>;

	assert_layout_eq!(u64, usize);
	assert_layout_eq!(u64, BlobLocation);
	assert_layout_eq!(u64, ConstPointer<T>);
	
	assert_layout_eq!(u64, Box<T>);
	assert_layout_eq!(BlobLocation, Box<T>);
	
	assert_layout_eq!([u64; 3], Vec<T>);
	assert_layout_eq!([u64; 3], VecVec<T>);
	assert_layout_eq!(UnpatchedVec, Vec<T>);

	assert!(u64::ALIGNMENT_SIZE == BlobHeader::ALIGNMENT_SIZE);
	assert!(u64::ALIGNMENT_SIZE == <Vec<u8>>::ALIGNMENT_SIZE);
}

// BB (rs) Bad tests... trying to validate that Vec<u8> always allocates with
//  correct alignment for ANY type... Is this actually guaranteed? Any way
//  to hint in the blob code?

#[test]
fn blob_buffer_alignment()
{
	let values : Vec<(UnpatchedBlob, Vec<u8>)> =
		(0..100)
			.map(
				|i|
				(
					// Allocates a new blob buffer, asserting that it's aligned

					UnpatchedBlob::new(),

					// Try to throw extra wrenches into whatever allocation machinery
					//  Vec uses by making awkwardly-sized allocations

					{
						let mut v = Vec::new();
						v.reserve_exact(i * 3);
						v
					}
				)
			)
			.collect();

	ASSERT! { values.len() == 100 }
}

#[test]
fn vec_buffer_alignment()
{
	let vecs : Vec<Vec<u8>> =
		[
			1, 2, 3, 4, 5, 6, 7, 8, 16,
			33, 77, 99, 100, 1111, 1313,
			32, 64, 128, 256, 512, 1024,
		]
		.into_iter()
		.map(
			|size|
			vec![7; size as usize]
		)
		.collect();

	let empty_vec = Vec::new();

	for vec in vecs.iter()
	{
		ASSERT! { vec.as_ptr() != empty_vec.as_ptr() }

		let addr = vec.as_ptr() as usize;
		ASSERT! { addr % MAX_ALIGNMENT_SIZE == 0 }
	}

	ASSERT! { !vecs.is_empty() }
}



#[test]
fn is_aligned_to()
{
	fn test_alignment(
		loc : BlobLocation,
		valid_alignments : &[usize],
		invalid_alignments : &[usize]
	)
	{
		for &alignment_size in valid_alignments
		{
			ASSERT!
			{
				loc.is_aligned_to(alignment_size),
				"Expected {} to be {}B aligned",
					loc.offset,
					alignment_size,
			}
		}

		for &alignment_size in invalid_alignments
		{
			ASSERT!
			{
				!loc.is_aligned_to(alignment_size),
				"Expected {} to not be {}B aligned",
					loc.offset,
					alignment_size,
			}
		}
	}

	test_alignment(BlobLocation::new(0), &[1, 2, 4, 8, 16], &[]);
	test_alignment(BlobLocation::new(1), &[1], &[2, 4, 8, 16]);
	test_alignment(BlobLocation::new(2), &[1, 2], &[4, 8, 16]);
	test_alignment(BlobLocation::new(4), &[1, 2, 4], &[8, 16]);
	test_alignment(BlobLocation::new(5), &[1], &[2, 4, 8, 16]);
	test_alignment(BlobLocation::new(8), &[1, 2, 4, 8], &[16]);
	test_alignment(BlobLocation::new(10), &[1, 2], &[4, 8, 16]);
	test_alignment(BlobLocation::new(16), &[1, 2, 4, 8, 16], &[]);
	test_alignment(BlobLocation::new(32), &[1, 2, 4, 8, 16], &[]);
	test_alignment(BlobLocation::new(64), &[1, 2, 4, 8, 16], &[]);
	test_alignment(BlobLocation::new(65), &[1], &[2, 4, 8, 16]);
}

#[test]
fn padding_bytes_needed_for()
{
	fn test_padding<T>()
	{
		let mut loc = BlobLocation::new(0);
		
		for in_between_byte_count in [0, 1, 3, 4, 8, 12, 18, 243]
		{
			ASSERT! { loc.is_aligned_for::<T>() };

			loc = loc + in_between_byte_count;
			loc = loc + loc.padding_bytes_needed_for::<T>();

			ASSERT! { loc.is_aligned_for::<T>() };

			loc = loc + T::BYTE_COUNT;
		}
	}

	test_padding::<u8>();
	test_padding::<u16>();
	test_padding::<u32>();
	test_padding::<u64>();
	test_padding::<u128>();

	struct Test
	{
		_a : i8,
		_b : String,
		_c : u32,
	}

	test_padding::<Test>();
}



// UnpatchedBlob tests



#[test]
fn new_blob()
{
	let blob = UnpatchedBlob::new(); 
	ASSERT! { blob.bytes.len() == BlobHeader::BYTE_COUNT }
}