use crate::*;



// BB (rs) Helper trait purely for synactic nicety

trait LayoutInfo
{
	const BYTE_COUNT 		: usize;
	const ALIGNMENT_SIZE 	: usize;
}

impl<T> LayoutInfo for T
where
	T : Sized
{
	const BYTE_COUNT 		: usize = size_of::<T>();
	const ALIGNMENT_SIZE 	: usize = align_of::<T>();
}

const MAX_ALIGNMENT_SIZE : usize = u128::ALIGNMENT_SIZE;

/// Helper macro to validate that the size and alignment of two types are the
/// same, in case we end up trying to transmute between them.
/// 
/// BB (rs) Not feasible to do this at compile time with generic type params...

macro_rules! ASSERT_LAYOUT_EQ
{
	($ty1:ty, $ty2:ty) =>
	{
		ASSERT!
		{
			<$ty1>::BYTE_COUNT == <$ty2>::BYTE_COUNT,
			"Alignment of {} ({}B) isn't the same as {} ({}B)",
				stringify!($ty1),
				<$ty1>::BYTE_COUNT,
				stringify!($ty2),
				<$ty2>::BYTE_COUNT,
		}

		ASSERT!
		{
			<$ty1>::ALIGNMENT_SIZE == <$ty2>::ALIGNMENT_SIZE,
			"Alignment of {} ({}B) isn't the same as {} ({}B)",
				stringify!($ty1),
				<$ty1>::ALIGNMENT_SIZE,
				stringify!($ty2),
				<$ty2>::ALIGNMENT_SIZE,
		}
	};
}



/// Trait indicating that a type can be written to a data blob.
/// 
/// If something has references (Vec, Box, Rc, etc.) then it needs to recursively
/// call handle_references on those fields. In practice, this can be done by just
/// calling handle_references on every field, which is what `#[derive(WriteBlob)]`
/// does.
/// 
/// Structs implementing this trait MUST have a consistent memory layout, generally
/// achieved using `#[repr(C)]`

pub trait WriteBlob : GetTypeInfo
{
	fn unpatch_references<Pass>(&self, pass : &mut Pass, self_location : BlobLocation)
	where
		Pass : BlobWriterPass;
}



/// A `BlobLocation` is simply an offset into a the bytes of a data blob

#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, GetTypeInfo)]
pub struct BlobLocation
{
	// BB (rs) Could save on blob size if these were 32-bit offsets... but that would
	//  impose a theoretical max of 4GB for blob size...

	offset : usize
}

impl BlobLocation
{
	const fn new(offset : usize) -> Self
	{
		// BB (rs) Can't use ASSERT_LAYOUT_EQ since this is a const fn

		assert!(Self::BYTE_COUNT == usize::BYTE_COUNT);
		assert!(Self::ALIGNMENT_SIZE == usize::ALIGNMENT_SIZE);

		// A blob location is just a usize, which is important so we can
		//  trivially read/write locations within a data blob

		Self { offset }
	}

	const fn start() -> Self
	{
		Self::new(0)
	}

	fn is_aligned_to(&self, alignment_size : usize) -> bool
	{
		self.offset % alignment_size == 0
	}

	fn is_aligned_for<T>(&self) -> bool
	{
		self.is_aligned_to(T::ALIGNMENT_SIZE)
	}

	fn padding_bytes_needed_for<T>(&self) -> usize
	{
		if self.is_aligned_to(T::ALIGNMENT_SIZE)
		{
			0
		}
		else
		{
			T::ALIGNMENT_SIZE - (self.offset % T::ALIGNMENT_SIZE)
		}
	}
}

impl std::ops::Add<usize> for BlobLocation
{
	type Output = Self;

	fn add(self, rhs: usize) -> Self::Output
	{
		Self::new(self.offset + rhs)
	}
}



/// In an `UnpatchedBlob`, all references exist in "blob-local" space -- ie. offsets
/// into the data buffer. Once converted to a `PatchedBlob` they'll be replaced by
/// real pointers / rust structs.
/// 
/// BB (rs) Support non-default allocators for blob buffers?

pub struct UnpatchedBlob
{
	/// Raw buffer containing bookkeeping info and data

	bytes : Vec<u8>,
}

/// In an `PatchedBlob`, references have been "patched" from local buffer offsets into
/// real, usable pointers (contained within actual rust runtime structs)
/// 
/// BB (rs) Support non-default allocators for blob buffers?

pub struct PatchedBlob<T>
{
	/// Raw buffer containing bookkeeping info and data

	bytes 		: Vec<u8>,

	/// Cached pointer into data buffer representing a (hopefully) valid instance
	/// of the given type

	data_ptr	: *const T,
}

/// Every data blob starts with a `BlobHeader` which contains all the necessary
/// info to convert unpatched data into patched data

#[repr(C)]
struct BlobHeader
{
	/// Version identifier, to identify cases where the blob format changes

	signature			: u64,

	/// Total size of non-header data and location within block (usually right
	/// after the header, but could be later if there's any preceding padding)
	
	data_byte_count		: usize,
	data_location		: BlobLocation,

	/// Hash value to identify the layout of the data type, to protect against
	/// trying to read a different type than was written (or perhaps an older
	/// version of a type with a different layout)

	data_type_hash		: u64,

	/// Info about all vecs in the data buffer, for patching

	vec_table_count		: usize,
	vec_table_location 	: BlobLocation,

	/// Info about all boxes in the data buffer, for patching

	box_table_count		: usize,
	box_table_location 	: BlobLocation,
}

/// A `Blob` is simply an owned buffer containing a fixed-size header followed by
/// a bunch of data bytes. It can exist as either a Patched or an Unpatched state.

trait Blob
{
	/// Reading bytes for inspection is always okay (though, not especially safe when patched)
	
	fn as_bytes(&self) -> &[u8];

	/// Header access is generally safe for any initialized blob

	fn get_header(&self) -> &BlobHeader
	{
		// NOTE (rs) Not generally safe (if someone tries to implement Blob for some
		//  arbitrary type) but at least UnpatchedBlob and PatchedBlob will always
		//  have a valid buffer to read a BlobHeader.

		unsafe
		{
			let header = self.location_as_ref(BlobLocation::start());
			header.assume_init_ref()
		}
	}

	/// Reinterpret a position in the data blob as an arbitrary, possibly uninitialized data type.
	/// 
	/// Note that we consider getting the MaybeUninit reference to be "safe," but using it for
	/// anything isn't

	fn location_as_ref<T>(&self, location : BlobLocation) -> &std::mem::MaybeUninit<T>
	{
		if config::ENABLE_ASSERTS
		{
			let _ = self.check_valid_location::<T, TRIP_ASSERTS>(location);
		}

		let byte_ref = &self.as_bytes()[location.offset];
		unsafe { std::mem::transmute(byte_ref) }
	}

	// Helper assertions to sanity check that things are being used as expected

	fn check_valid_location<T, const DO_ASSERTS : DoAsserts>(
		&self,
		location : BlobLocation) -> bool
	{
		// Data must start and end within the buffer

		if location.offset >= self.as_bytes().len()
		{
			ASSERT!
			{
				DO_ASSERTS == SKIP_ASSERTS,
				"Accessing bytes at invalid location {:x} > {:x}",
					location.offset,
					self.as_bytes().len(),
			}

			return false;
		}
		else if location.offset + T::BYTE_COUNT > self.as_bytes().len()
		{
			ASSERT!
			{
				DO_ASSERTS == SKIP_ASSERTS,
				"Accessing {}B of data at invalid location {:x} ({}B left)",
					T::BYTE_COUNT,
					location.offset,
					self.as_bytes().len() - location.offset,
			}

			return false;
		}

		// Data must be aligned in blob-local space

		if !location.is_aligned_for::<T>()
		{
			ASSERT!
			{
				DO_ASSERTS == SKIP_ASSERTS,
				"Unaligned access at location {:x} (size={}B, alignment={}B, +{}B off)",
					location.offset,
					T::BYTE_COUNT,
					T::ALIGNMENT_SIZE,
					location.offset % T::BYTE_COUNT,
			}

			return false;
		}

		// Data must also be aligned in the actual system memory for the buffer

		let byte_ptr = &self.as_bytes()[location.offset] as *const u8;
		let address = byte_ptr as usize;
		let alignment_error = address % T::ALIGNMENT_SIZE;

		if alignment_error != 0
		{
			ASSERT!
			{
				DO_ASSERTS == SKIP_ASSERTS,
				"Location {:x} is aligned, but buffer mapping at {:p} isn't! (size={}B, alignment={}B, +{}B off)",
					location.offset,
					byte_ptr,
					T::BYTE_COUNT,
					T::ALIGNMENT_SIZE,
					alignment_error,
			}

			return false;
		}

		true
	}

	fn assert_valid_buffer(&self)
	{
		// We're going to be writing/reading the header directly at the start of the
		//  buffer, so it's important that it's always properly aligned even though the
		//  Vec allocation doesn't know how it's being used

		// BB (rs) A bit shaky... any way to request an explicit alignment? I believe
		//  on all the relevant platforms the default global allocator always uses
		//  8B alignment, so we're fine in practice...

		// BB (rs) AVX tho?

		let allocated_buffer_address = self.as_bytes().as_ptr() as usize;
	
		ASSERT!
		{
			allocated_buffer_address % MAX_ALIGNMENT_SIZE == 0,
			"Expected allocated blob buffer to always be aligned to {}B ({}B off)",
				MAX_ALIGNMENT_SIZE,
				allocated_buffer_address % MAX_ALIGNMENT_SIZE,
		}

		ASSERT!
		{
			MAX_ALIGNMENT_SIZE % BlobHeader::ALIGNMENT_SIZE == 0,
			"Expected blob header to be able to be {}B aligned (found {}B)",
				MAX_ALIGNMENT_SIZE,
				BlobHeader::ALIGNMENT_SIZE,
		}
	}
}

impl Blob for UnpatchedBlob
{
	fn as_bytes(&self) -> &[u8]
	{
		&self.bytes	
	}
}

impl From<UnpatchedBlob> for Vec<u8>
{
	fn from(value: UnpatchedBlob) -> Self
	{
		value.bytes
	}
}

impl UnpatchedBlob
{
	/// Creates a new empty, unpatched blob to be written to

	fn new() -> Self
	{
		const EMPTY_HEADER : BlobHeader = BlobHeader
		{
			signature			: BLOB_SIGNATURE,

			data_byte_count 	: 0,
			data_location		: BlobLocation::new(EMPTY_BLOB_SIGNATURE),

			data_type_hash		: UNKNOWN_TYPE_SIGNATURE,

			vec_table_count 	: 0,
			vec_table_location 	: BlobLocation::new(EMPTY_VEC_TABLE_SIGNATURE),

			box_table_count 	: 0,
			box_table_location 	: BlobLocation::new(EMPTY_VEC_TABLE_SIGNATURE),
		};

		let mut blob = Self
		{
			bytes : vec![0x00; BlobHeader::BYTE_COUNT]
		};

		blob.assert_valid_buffer();

		// NOTE (rs) Safe, since we know we have an uninitialized buffer that's
		//  sufficiently sized and aligned.

		let header = blob.location_as_mut(BlobLocation::start());
		header.write(EMPTY_HEADER);

		blob
	}

	pub fn write<T>(value : &T) -> Self
	where
		T : WriteBlob
	{
		write_blob(value)
	}

	fn get_header_mut(&mut self) -> &mut BlobHeader
	{
		// NOTE (rs) Safe, since we always initialize a Blob with a valid header

		unsafe
		{
			let header = self.location_as_mut(BlobLocation::start());
			header.assume_init_mut()
		}
	}
	
	/// Reinterpret a position in the data blob as an arbitrary, possibly uninitialized data type.
	/// 
	/// Note that we consider getting the MaybeUninit reference to be "safe," but using it for
	/// anything isn't
	
	fn location_as_mut<T>(&mut self, location : BlobLocation) -> &mut std::mem::MaybeUninit<T>
	{
		if config::ENABLE_ASSERTS
		{
			let _ = self.check_valid_location::<T, TRIP_ASSERTS>(location);
		}

		let byte_ref = &mut self.bytes[location.offset];
		unsafe { std::mem::transmute(byte_ref) }
	}
}



#[derive(Debug)]
pub enum ReadBlobError
{
	/// Blob doesn't have enough data to read a header

	FailedToGetHeader,

	/// Blob header starts with an invalid signature
	///   (could be garbage, or an old blob version)
	
	InvalidBlobSignature,

	/// Info in the blob's header about the contained data doesn't make sense

	InvalidHeaderData,

	/// The blob might contain valid data, but it doesn't match the requested
	/// runtime type (because it was written with different data layout)

	InvalidHeaderTypeHash,

	/// Info in the blob's header about the vec table doesn't make sense
	
	InvalidHeaderVecTable,
	
	/// Info in the blob's header about the box table doesn't make sense
	
	InvalidHeaderBoxTable,

	/// A reference table entry (vec, box, etc.) refers to an invalid location

	InvalidReferenceTableEntry,

	/// A reference refers to an invalid location

	InvalidReferenceLocation,
}

impl<T> Blob for PatchedBlob<T>
{
	fn as_bytes(&self) -> &[u8]
	{
		&self.bytes
	}
}

impl<T> PatchedBlob<T>
where T : WriteBlob
{
	/// # Safety
	/// 
	/// By the time we have a PatchedBlob instance we're reasonably certain that it
	/// should be able to be interpreted as a real data. The contents have been
	/// validated, including checking that the layout matches the expected type, and
	/// all references have been patched.
	/// 
	/// However, since we're dealing with arbitrary input data it's not possible to
	/// be 100% sure that the content really is valid, hence marking this as unsafe.

	pub unsafe fn as_ref(&self) -> &T
	where
		T : WriteBlob
	{
		&*self.data_ptr
	}

	/// Takes in an arbitrary buffer and tries to read it into a *patched* Blob, ready
	/// to be reinterpreted. Will return an error if we detect malformed input data,
	/// though it's still possible to produce false positives which will result in UB.

	pub fn try_read(bytes : Vec<u8>) -> Result<Self, ReadBlobError>
	{
		let blob = UnpatchedBlob
		{
			bytes,
		};

		blob.assert_valid_buffer();
		
		if !blob.check_valid_location::<BlobHeader, SKIP_ASSERTS>(BlobLocation::start())
		{
			return Err(ReadBlobError::FailedToGetHeader);
		}

		let header = blob.get_header();
		if header.signature != BLOB_SIGNATURE
		{
			return Err(ReadBlobError::InvalidBlobSignature);
		}

		if header.data_type_hash != T::type_hash()
		{
			return Err(ReadBlobError::InvalidHeaderTypeHash);
		}

		// TODO (rs) Add source data layout signature to header, CRC?

		let patched = unsafe { Self::try_patch(blob) ? };
		Ok(patched)
	}

	/// Try to convert buffer offsets into real pointers, converting an `UnpatchedBlob`
	/// into a `PatchedBlob`
	
	unsafe fn try_patch(mut blob : UnpatchedBlob) -> Result<Self, ReadBlobError>
	{
		// NOTE (rs) By the time we're in here we know we should at least be
		//  able to read the blob header without dying.

		let vec_table_count;
		let vec_table_location;

		let box_table_count;
		let box_table_location;

		let data_location;

		{
			let header = blob.get_header();

			if header.data_byte_count > 0 &&
				!blob.check_valid_location::<T, SKIP_ASSERTS>(header.data_location)
			{
				return Err(ReadBlobError::InvalidHeaderData);
			}

			data_location = header.data_location;

			if header.vec_table_count > 0 &&
				!blob.check_valid_location::<BlobLocation, SKIP_ASSERTS>(header.vec_table_location)
			{
				return Err(ReadBlobError::InvalidHeaderVecTable);
			}

			vec_table_count = header.vec_table_count;
			vec_table_location = header.vec_table_location;

			if header.box_table_count > 0 &&
				!blob.check_valid_location::<BlobLocation, SKIP_ASSERTS>(header.box_table_location)
			{
				return Err(ReadBlobError::InvalidHeaderBoxTable);
			}

			box_table_count = header.box_table_count;
			box_table_location = header.box_table_location;
		};

		Self::try_patch_reference_table::<UnpatchedVec>(
			&mut blob,
			vec_table_count,
			vec_table_location) ?;

		Self::try_patch_reference_table::<UnpatchedBox>(
			&mut blob,
			box_table_count,
			box_table_location) ?;

		// Package the data back up now that it's been successfully patched

		let data_ptr = blob.location_as_ref(data_location).as_ptr();

		let patched = Self
		{
			bytes : blob.bytes,
			data_ptr
		};

		Ok(patched)
	}

	unsafe fn try_patch_reference_table<ReferenceType>(
		blob 			: &mut UnpatchedBlob,
		location_count 	: usize,
		table_location 	: BlobLocation
	) -> Result<(), ReadBlobError>
	where
		ReferenceType : PatchReference
	{
		// No patching if there are no locations to patch

		if location_count == 0
		{
			return Ok(());
		}

		// Get the reference table following the info from the blob's header

		let table_ptr = blob.location_as_ref::<BlobLocation>(table_location).as_ptr();
		let locations = std::slice::from_raw_parts(table_ptr, location_count);

		// Run the conversion at each location

		for &location in locations
		{
			ReferenceType::try_patch(blob, location) ?;
		}

		Ok(())
	}
}



/// Trait for having an unpatched struct convert itself into a real rust reference
/// struct during the blob patching process

trait PatchReference : Sized+Copy
{
	type Patched;

	unsafe fn try_patch(
		blob 			: &mut UnpatchedBlob,
		self_location 	: BlobLocation)
	-> Result<(), ReadBlobError>
	{
		if !blob.check_valid_location::<Self, SKIP_ASSERTS>(self_location)
		{
			return Err(ReadBlobError::InvalidReferenceTableEntry);
		}

		// Create two views of the same data, first copying the unpatched data out
		//  of the references's original location, then switching to treat it as an
		//  uninitialized region to write a real (patched) reference

		// NOTE (rs) We lose any info about the reference's original type

		let unpatched : Self = blob.location_as_ref::<Self>(self_location).assume_init();

		let self_ptr = blob.location_as_mut::<Self::Patched>(self_location)
						as *mut std::mem::MaybeUninit<Self::Patched>;
		
		// Create the real patched reference type (pointing into the data buffer)
		
		let patched = unpatched.try_into_patched(blob) ?;
		
		// ... and emplace it into the original buffer location (making sure not to drop
		//  the old, non-initialized data)

		(*self_ptr).write(patched);

		Ok(())
	}

	unsafe fn try_into_patched(self, blob : &mut UnpatchedBlob) -> Result<Self::Patched, ReadBlobError>;
}

#[repr(C)]
#[derive(Clone, Copy)]
struct UnpatchedVec
{
	signature 		: usize,
	count 			: usize,
	data_location	: BlobLocation,
}

impl UnpatchedVec
{
	pub fn new(count : usize, data_location : BlobLocation) -> Self
	{
		// NOTE (rs) We do ASSERT_LAYOUT_EQ!(Vec<T>, UnpatchedVec) elsewhere to
		//  ensure that we can emplace vecs wherever we write a UnpatchedVec

		Self
		{
			signature : VEC_BLOB_SIGNATURE,
			count,
			data_location,
		}
	}
}

impl PatchReference for UnpatchedVec
{
	type Patched = Vec<u8>;
	
	unsafe fn try_into_patched(self, blob : &mut UnpatchedBlob) -> Result<Self::Patched, ReadBlobError>
	{
		if !blob.check_valid_location::<Self::Patched, SKIP_ASSERTS>(self.data_location)
		{
			return Err(ReadBlobError::InvalidReferenceLocation);
		}

		let data_ptr = blob.location_as_mut::<u8>(self.data_location).as_mut_ptr();
		Ok(Vec::from_raw_parts(data_ptr, self.count, self.count))
	}
}

#[repr(C)]
#[derive(Clone, Copy)]
struct UnpatchedBox
{
	data_location : BlobLocation,
}

impl UnpatchedBox
{
	pub fn new(data_location : BlobLocation) -> Self
	{
		ASSERT_LAYOUT_EQ!(Self, Box<u8>);
		ASSERT_LAYOUT_EQ!(Self, Box<String>);

		Self
		{
			data_location,
		}
	}
}

impl PatchReference for UnpatchedBox
{
	type Patched = Box<u8>;

	unsafe fn try_into_patched(self, blob : &mut UnpatchedBlob) -> Result<Self::Patched, ReadBlobError>
	{
		if !blob.check_valid_location::<Self::Patched, SKIP_ASSERTS>(self.data_location)
		{
			return Err(ReadBlobError::InvalidReferenceLocation);
		}

		let data_ptr = blob.location_as_mut::<u8>(self.data_location).as_mut_ptr();
		Ok(Box::from_raw(data_ptr))
	}
}



/// Core blob writing routine. Most of its complexity revolves around
/// making sure we only allocate once for everything we're going to write
/// to the blob (including top-level data, references, and metadata)

fn write_blob<T>(value : &T) -> UnpatchedBlob
where
	T : WriteBlob
{
	let single_slice = std::slice::from_ref(value);

	let mut writer = BlobWriter
	{
		blob 			: UnpatchedBlob::new(),
		vec_locations 	: Vec::new(),
		box_locations	: Vec::new(),
	};

	// First, do a "dry run" which just counts the total number of bytes we're
	//  going to write (including padding and all referenced data)

	let mut dry_run = BlobWriterDryRun::new(&writer);
	let expected_padding_start_locaton = dry_run.next_location();
	let expected_data_start_location = dry_run.add_slice(single_slice).expect("Writing no data?");
	let expected_data_end_location = dry_run.next_location();

	// Then do the same for writing out reference metadata, pre-allocating and
	//  handing allocations off the actual writer to avoid redundant work

	let mut dummy_vec_locations = vec![BlobLocation::start(); dry_run.vec_count];
	let expected_vec_table_start_location = dry_run.add_slice(&dummy_vec_locations);
	let expected_vec_table_end_location = dry_run.next_location();

	dummy_vec_locations.clear();
	writer.vec_locations = dummy_vec_locations;
	
	let mut dummy_box_locations = vec![BlobLocation::start(); dry_run.box_count];
	let expected_box_table_start_location = dry_run.add_slice(&dummy_box_locations);
	let expected_box_table_end_location = dry_run.next_location();

	dummy_box_locations.clear();
	writer.box_locations = dummy_box_locations;

	let _ = dry_run.add_slice(std::slice::from_ref(&END_BLOB_SIGNATURE));
	let expected_blob_end_location = dry_run.end_location;

	// Pre-allocate enough space to fit the total size we computed for both data
	//  and reference metadata

	let expected_additional_byte_count =
		expected_blob_end_location.offset - expected_padding_start_locaton.offset;
	
	writer.blob.bytes.reserve_exact(expected_additional_byte_count);
	let expected_allocation_size = writer.blob.bytes.capacity();

	// Write out the actual data and sanity check that sizes match the dry run

	let data_start_location = writer.add_slice(single_slice).expect("Writing no data?");
	let data_end_location = writer.next_location();

	ASSERT! { expected_data_start_location == data_start_location }
	ASSERT! { expected_data_end_location == data_end_location };

	// Write out reference metadata and sanity check that sizes match the dry run

	// BB (rs) Need to do a bit of shuffling to avoid mostly-reasonable borrow
	//  checker semantics when calling self.write_slice(...). It could technically
	//  end up modifying self.vec/box_locations, though we know it won't because we
	//  aren't adding any new references, just blob locations.

	let vec_table = std::mem::take(&mut writer.vec_locations);
	let vec_table_location = writer.add_slice(&vec_table);
	ASSERT! { vec_table.len() == dry_run.vec_count }
	ASSERT! { expected_vec_table_start_location == vec_table_location }
	ASSERT! { expected_vec_table_end_location == writer.next_location() }

	let box_table = std::mem::take(&mut writer.box_locations);
	let box_table_location = writer.add_slice(&box_table);
	ASSERT! { box_table.len() == dry_run.box_count }
	ASSERT! { expected_box_table_start_location == box_table_location }
	ASSERT! { expected_box_table_end_location == writer.next_location() }

	ASSERT! { writer.vec_locations.is_empty(), "Added new vecs when writing reference tables?" }
	ASSERT! { writer.box_locations.is_empty(), "Added new boxes when writing reference tables?" }

	// Add and ending signature, purely as a debugging aide

	let _ = writer.add_slice(std::slice::from_ref(&END_BLOB_SIGNATURE));
	ASSERT! { expected_blob_end_location == writer.next_location() }

	// Track metadata in the blob header, starting with the data region

	let header = writer.blob.get_header_mut();

	header.data_byte_count = data_end_location.offset - data_start_location.offset;
	header.data_location = data_start_location;
	header.data_type_hash = T::type_hash();

	ASSERT! { header.data_byte_count > 0 }

	// Then track info for reference patching

	ASSERT! { header.vec_table_count == 0 };
	header.vec_table_count = vec_table.len();
	header.vec_table_location = match vec_table_location
	{
		Some(location) 	=> location,
		None			=> BlobLocation::new(EMPTY_VEC_TABLE_SIGNATURE),
	};
	
	ASSERT! { header.box_table_count == 0 };
	header.box_table_count = box_table.len();
	header.box_table_location = match box_table_location
	{
		Some(location) 	=> location,
		None			=> BlobLocation::new(EMPTY_BOX_TABLE_SIGNATURE),
	};

	// Check to see if we ended up needing to reallocate. Not really a problem per-se,
	//  but it's wasteful, hence all the work up front to get a total size and pre-allocate
	
	ASSERT! { expected_allocation_size == writer.blob.bytes.capacity() }
	ASSERT! { expected_allocation_size == writer.blob.bytes.len() }

	// Unwrap the blob data for consumption

	writer.blob
}

/// `BlobWriterPass` handles two things:
///   1. Recursion over the given structure, via the WriteBlob trait
///   2. Accounting for padding between data with different alignment

pub trait BlobWriterPass : Sized
{
	fn next_location(&self) -> BlobLocation;
	fn add_bytes(&mut self, bytes : &[u8]);

	fn on_handle_vec(
		&mut self,
		count 					: usize,
		self_location 			: BlobLocation,
		slice_start_location 	: BlobLocation);

	fn on_handle_box(
		&mut self,
		self_location : BlobLocation,
		data_location : BlobLocation);

	// Common blob writer implementation, not expected to be overridden

	/// Returns the starting location of the first slice element, or `None` if
	/// there aren't any values to write (it's up to the caller to interpret `None`,
	/// possibly writing out a recognizable signature value)

	fn add_slice<T>(&mut self, values : &[T]) -> Option<BlobLocation>
	where
		T : WriteBlob
	{
		// Early-out if there is no data to write

		if values.is_empty()
		{
			return None;
		}

		// Ensure data is aligned, then write out a contiguous sequence containing each value

		self.add_padding_for::<T>();
		let slice_start_location = self.next_location();
		
		ASSERT!
		{
			slice_start_location.is_aligned_for::<T>(),
			"Incorrect number of padding bytes when aligning slice start (expected {:x} @ {:x})?",
			T::ALIGNMENT_SIZE,
			slice_start_location.offset,
		}

		for (value_index, value) in values.iter().enumerate()
		{
			// Sanity check that each element is ending up at the location we expect
			//  in the second loop below

			let expected_value_location = slice_start_location + T::BYTE_COUNT * value_index;
			ASSERT! { self.next_location() == expected_value_location }

			self.add_slice_element(value);
		}

		// Now that the contiguous slice has been written, recurse down into any child
		//  references, writing values and setting up bookkeeping so we can reconstruct
		//  them when reading the image later

		for (value_index, value) in values.iter().enumerate()
		{
			let value_location = slice_start_location + T::BYTE_COUNT * value_index;
			value.unpatch_references(self, value_location);
		}

		// Return the location of the slice, so the caller can know where the data ended
		//  up after adding any padding

		Some(slice_start_location)
	}

	fn add_padding_for<T>(&mut self)
	{
		let padding_byte_count = self.next_location().padding_bytes_needed_for::<T>();
		self.add_bytes(get_padding_bytes(padding_byte_count));
	}

	fn add_slice_element<T>(&mut self, value : &T)
	{
		// NOTE (rs) add_slice is responsible for aligning the start of each slice. Once
		//  aligned, we're writing sizeof(T) bytes for each element, which is guaranteed to
		//  be a multiple of alignof(T) -- https://doc.rust-lang.org/nomicon/repr-rust.html

		ASSERT! { T::BYTE_COUNT % T::ALIGNMENT_SIZE == 0 }

		ASSERT!
		{
			self.next_location().is_aligned_for::<T>(),
			"Writing element at offset ({:x}) with invalid alignment (expected {})",
			self.next_location().offset,
			T::ALIGNMENT_SIZE,
		}
		
		// Write out raw data

		let data_ptr = value as *const T as *const u8;
		let data_bytes = unsafe { std::slice::from_raw_parts(data_ptr, T::BYTE_COUNT) };
	
		self.add_bytes(data_bytes);
	}

	fn unpatch_vec<T>(&mut self, self_location : BlobLocation, values : &Vec<T>)
	where
		T : WriteBlob
	{
		// Write out a contigious slice continaining all the values

		let slice_start_location = match self.add_slice(values.as_slice())
		{
			Some(location) 	=> location,
			None 			=> BlobLocation::new(EMPTY_VEC_SIGNATURE),
		};

		// Keep track of vec references

		self.on_handle_vec(values.len(), self_location, slice_start_location);

		// NOTE (rs) Treating rust Vec as a black box with no field layout guarantees

		ASSERT_LAYOUT_EQ!(UnpatchedVec, Vec<T>);
	}

	#[allow(clippy::borrowed_box)]
	fn unpatch_box<T>(&mut self, self_location : BlobLocation, value : &Box<T>)
	where
		T : WriteBlob
	{
		// Write out the referenced value

		let data_location = self.add_slice(std::slice::from_ref(value.as_ref()))
									.expect("Wrote a box blob with no data?");

		// Keep track of box references

		self.on_handle_box(self_location, data_location);

		ASSERT_LAYOUT_EQ!(UnpatchedBox, Box<T>);
	}
}

/// `BlobWriter` uses the `BlobWriterPass` machinery to create and add data to a `Blob`.
/// Once all data is written, it appends additional metadata needed to patch intra-blob
/// references (`Vec`s, etc.)

struct BlobWriter
{
	blob 			: UnpatchedBlob,
	vec_locations 	: Vec<BlobLocation>,
	box_locations 	: Vec<BlobLocation>,
}

impl BlobWriterPass for BlobWriter
{
	fn next_location(&self) -> BlobLocation
	{
		BlobLocation::new(self.blob.bytes.len())
	}

	fn add_bytes(&mut self, bytes : &[u8])
	{
		let remaining_capacity = self.blob.bytes.capacity() - self.blob.bytes.len();

		ASSERT!
		{
			remaining_capacity >= bytes.len(),
			"Reallocating while writing {}B to blob ({}B remaining capacity)",
				bytes.len(),
				remaining_capacity,
		}

		self.blob.bytes.extend_from_slice(bytes);
	}

	fn on_handle_vec(
		&mut self,
		count 					: usize,
		self_location 			: BlobLocation,
		slice_start_location 	: BlobLocation)
	{
		// Overwrite original buffer region with data using a known, fixed layout so we
		//  can read it back in later, even if the layout of the Vec type changes

		let unpatched_vec_ptr = self.blob.location_as_mut(self_location);
		unpatched_vec_ptr.write(UnpatchedVec::new(count, slice_start_location));

		// Keep track of all vecs so we know where to look when we go to reconstruct them

		self.vec_locations.push(self_location);
	}
	
	fn on_handle_box(
		&mut self,
		self_location : BlobLocation,
		data_location : BlobLocation)
	{
		// Overwrite original buffer region with an unpatched location

		let unpatched_box_ptr = self.blob.location_as_mut(self_location);
		unpatched_box_ptr.write(UnpatchedBox::new(data_location));

		// Keep track of all boxes so we know where to look when we go to reconstruct them

		self.box_locations.push(self_location);
	}

	
}

/// `BlobWriterDryRun` is a pass that simply walks the input structure to simulate
/// what `BlobWriter` will do. This lets it produce an expected size of the entire
/// data blob so `BlobWriter` can pre-allocate enough space to avoid reallocating

struct BlobWriterDryRun
{
	end_location 	: BlobLocation,
	vec_count 		: usize,
	box_count		: usize,
}

impl BlobWriterDryRun
{
	fn new(writer : &BlobWriter) -> Self
	{
		Self
		{
			end_location 	: writer.next_location(),
			vec_count 		: writer.vec_locations.len(),
			box_count 		: writer.box_locations.len(),
		}
	}
}

impl BlobWriterPass for BlobWriterDryRun
{
	fn next_location(&self) -> BlobLocation
	{
		self.end_location
	}

	fn add_bytes(&mut self, bytes : &[u8])
	{
		self.end_location = self.end_location + bytes.len()
	}

	fn on_handle_vec(
		&mut self,
		_count 					: usize,
		_self_location 			: BlobLocation,
		_slice_start_location 	: BlobLocation)
	{
		// Count vecs so we know how big the vec table will be

		self.vec_count += 1;
	}
	
	fn on_handle_box(
		&mut self,
		_self_location : BlobLocation,
		_data_location : BlobLocation)
	{
		// Count boxes so we know how big the box table will be

		self.box_count += 1;
	}
}



// Anything that's plain ol' data can be trivially imaged

macro_rules! impl_has_no_references
{
	{$($type:ty),*,} =>
	{
		$(
			impl WriteBlob for $type
			{
				fn unpatch_references<Pass>(&self, _ : &mut Pass, _ : BlobLocation)
				where
					Pass : BlobWriterPass
				{}
			}
		)*
	};
}

impl_has_no_references!
{
	u8, u16, u32, u64, usize,
	i8, i16, i32, i64, isize,
	f32, f64,
	bool,
	BlobLocation,
}



// Standard reference types have special handling inside BlobWriterPass

impl<T> WriteBlob for Vec<T>
where
	T : WriteBlob
{
	fn unpatch_references<Pass>(&self, pass : &mut Pass, self_location : BlobLocation)
	where
		Pass : BlobWriterPass
	{
		pass.unpatch_vec(self_location, self);
	}
}

impl<T> WriteBlob for Box<T>
where
	T : WriteBlob
{
	fn unpatch_references<Pass>(&self, pass : &mut Pass, self_location : BlobLocation)
	where
		Pass : BlobWriterPass
	{
		pass.unpatch_box(self_location, self);
	}
}



// Helper macro for recursing down through fields when implementing WriteBlob

#[macro_export]
macro_rules! handle_field_references
{
	($pass:expr, $self_location:expr, $self:ident.$field:ident) =>
	{
		$self.$field.handle_references(
			$pass,
			$self_location + std::mem::offset_of!(Self, $field),
		);
	}
}



// Misc helpers

// BB (rs) The real issue with endianness is writing blobs with one endianness that then
//  get read on a system with a different endianness. Embed this into the blob header?

#[cfg(target_endian = "big")]
compile_error!("Data blob module only supports little-endian systems");

/// Initial blob version
const BLOB_SIGNATURE : u64 = compute_signature("BLOB*001");

const END_BLOB_SIGNATURE 		: usize = compute_signature("BLOB*END");
const EMPTY_BLOB_SIGNATURE 		: usize = compute_signature("NO*DATA*");
const UNKNOWN_TYPE_SIGNATURE 	: u64 	= compute_signature("NO*TYPE*");
const EMPTY_VEC_TABLE_SIGNATURE : usize = compute_signature("ZERO*VEC");
const EMPTY_BOX_TABLE_SIGNATURE : usize = compute_signature("ZERO*BOX");
const VEC_BLOB_SIGNATURE 		: usize = compute_signature("VEC*BLOB");
const EMPTY_VEC_SIGNATURE 		: usize = compute_signature("EMPTYVEC");

const fn compute_signature<T>(text : &'static str) -> T
where
	T : Sized+Copy
{
	let bytes = text.as_bytes();
	assert!(bytes.len() == T::BYTE_COUNT);

	unsafe
	{
		let eight_bytes = std::mem::transmute::<&u8, &[u8;8]>(&bytes[0]);
		std::mem::transmute_copy(eight_bytes)
	}
}

fn get_padding_bytes(padding_byte_count : usize) -> &'static [u8]
{
	let full_sequence = "PADDING";

	ASSERT! { full_sequence.len() == 7 };
	ASSERT! { padding_byte_count <= 7 };

	&full_sequence.as_bytes()[0..padding_byte_count]
}



// Tests

#[cfg(test)]
#[path = "./blob_tests.rs"]
mod blob_tests;