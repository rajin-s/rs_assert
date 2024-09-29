use crate::*;



// BB (rs) Helper trait purely for synactic nicety

trait LayoutInfo
{
	const BYTE_COUNT 		: usize;
	const ALIGNMENT_SIZE 	: usize;
}

impl<T> LayoutInfo for T
	where T : Sized
{
	const BYTE_COUNT 		: usize = size_of::<T>();
	const ALIGNMENT_SIZE 	: usize = align_of::<T>();
}

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
/// Anything that is just data with a fixed layout (eg. integral types) can implement
/// `HasNoReferences` to get a blanket impl of `WriteBlob` that does nothing.
/// 
/// Otherwise, if something has references (Vec, Box, Rc, etc.) then it needs to
/// recursively call handle_references on those fields. In practice, this can be
/// done by just calling handle_references on every field, which is what
/// `#[derive(WriteBlob)]` does (TODO: implement derive)
/// 
/// Structs implementing this trait MUST have a consistent memory layout, generally
/// achieved using `#[repr(C)]`

pub trait WriteBlob
{
	fn handle_references<T>(&self, pass : &mut T, self_location : BlobLocation)
		where T : BlobWriterPass;
}



/// A `BlobLocation` is simply an offset into a the bytes of a data blob

#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlobLocation
{
	// BB (rs) Could save on blob size if these were 32-bit offsets... but that would
	//  impose a theoretical max of 4GB for blob size...

	offset : usize
}

impl BlobLocation
{
	fn new(offset : usize) -> Self
	{
		// A blob location is just a usize, which is important so we can
		//  trivially read/write locations within a data blob

		ASSERT_LAYOUT_EQ!(Self, usize);

		Self { offset }
	}

	fn start() -> Self
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

	/// Info in the blob's header about the vec table doesn't make sense
	
	InvalidHeaderVecTable,
}




/// Initial blob version

const BLOB_SIGNATURE : u64 = compute_signature("BLOB*001");

/// A `Blob` is simply an owned buffer containing a fixed-size header followed by
/// a bunch of data bytes.
/// 
/// BB (rs) Support non-default allocators for blob buffers?

pub struct Blob
{
	/// Raw buffer containing bookkeeping info and data

	bytes : Vec<u8>,

	/// True: 	References in the blob are buffer offsets
	/// False: 	References in the blob have been "patched" into real rust structs

	is_patched : bool,
}

#[repr(C)]
struct BlobHeader
{
	/// Version identifier, to identify cases where the blob format changes

	signature			: u64,

	/// Total size of non-header data and location within block (usually right
	/// after the header, but could be later if there's any preceding padding)
	
	data_byte_count		: usize,
	data_location		: BlobLocation,

	/// Info about the vec table, for patching

	vec_table_count		: usize,
	vec_table_location 	: BlobLocation,
}

impl Blob
{
	/// Reading bytes for inspection is always okay (though, not especially safe when patched)

	pub fn as_bytes(&self) -> &[u8]
	{
		&self.bytes
	}

	/// Generally, it's only valid to convert a blob into a bunch of bytes when it's
	/// unpatched. Otherwise, dumping it and trying to load it again will only produce
	/// errant behavior.

	pub fn into_unpatched_bytes(self) -> Vec<u8>
	{
		assert!(!self.is_patched, "Can't call into_bytes on a blob that's been patched");
		self.bytes
	}

	/// Takes in an arbitrary buffer and tries to read it into a *patched* Blob, ready
	/// to be reinterpreted. Will return an error if we detect malformed input data,
	/// though it's still possible to produce false positives which will result in UB

	pub fn try_read(bytes : Vec<u8>) -> Result<Self, ReadBlobError>
	{
		use ReadBlobError::*;
		
		Blob::assert_valid_buffer(&bytes);

		let mut blob = Self
		{
			bytes,
			is_patched: false,
		};
		
		if !blob.check_valid_location_for_type::<BlobHeader, SKIP_ASSERTS>(BlobLocation::start())
		{
			return Err(FailedToGetHeader);
		}

		let header = blob.get_header();
		if header.signature != BLOB_SIGNATURE
		{
			return Err(InvalidBlobSignature);
		}

		if header.data_byte_count > 0 &&
			!blob.check_valid_location_for_type::<u8, SKIP_ASSERTS>(header.data_location)
		{
			return Err(InvalidHeaderData);
		}

		if header.vec_table_count > 0 &&
			!blob.check_valid_location_for_type::<u8, SKIP_ASSERTS>(header.vec_table_location)
		{
			return Err(InvalidHeaderVecTable);
		}

		// TODO (rs) Add source data layout signature to header, CRC?

		unsafe { blob.patch_references(); }

		Ok(blob)

	}

	/// Creates a new empty, unpatched blob to be written to

	pub fn empty() -> Self
	{
		const HEADER_BYTE_COUNT : usize = size_of::<BlobHeader>();

		let mut raw = Self
		{
			bytes 		: vec![0x00; HEADER_BYTE_COUNT],
			is_patched 	: false,
		};

		Blob::assert_valid_buffer(&raw.bytes);

		// NOTE (rs) get_header_* is typically safe, except right here since it relies
		//  on us initializing with enough space to store the header

		#[allow(unused_unsafe)]
		unsafe
		{
			*raw.get_header_mut() = BlobHeader
			{
				signature			: BLOB_SIGNATURE,
	
				data_byte_count 	: 0,
				data_location		: BlobLocation::new(EMPTY_BLOB_SIGNATURE),
	
				vec_table_count 	: 0,
				vec_table_location 	: BlobLocation::new(EMPTY_VEC_TABLE_SIGNATURE),
			};
		}

		raw
	}

	pub fn write<T>(value : &T) -> Self
		where T : WriteBlob
	{
		let mut writer = BlobWriter::new();
		let data_start_location = writer.write_slice(std::slice::from_ref(value));
		let blob = writer.finish(data_start_location);

		assert!(!blob.is_patched, "BlobWriter always returns an unpatched blob");

		blob
	}

	pub unsafe fn as_ref<T>(&self) -> &T
	{
		assert!(self.is_patched, "Can't call as_ref on a blob that hasn't been patched");

		let data_location = self.get_header().data_location;
		&*self.location_as_ptr(data_location)
	}

	// Header access is generally safe, for any initialized blob

	fn get_header_mut(&mut self) -> &mut BlobHeader
	{
		// NOTE (rs) Safe, since we always initialize a Blob with enough space for a header

		unsafe { &mut *self.location_as_ptr_mut(BlobLocation::start()) }
	}

	fn get_header(&self) -> &BlobHeader
	{
		unsafe { &*self.location_as_ptr(BlobLocation::start()) }
	}

	/// Reinterpret a position in the data blob as some arbitrary data type

	fn location_as_ptr<T>(&self, location : BlobLocation) -> *const T
	{
		if config::ENABLE_ASSERTS
		{
			let _ = self.check_valid_location_for_type::<T, TRIP_ASSERTS>(location);
		}

		let byte_ptr = &self.bytes[location.offset] as *const u8;
		byte_ptr as *const T
	}
	
	/// Reinterpret a position in the data blob as some arbitrary data type
	
	fn location_as_ptr_mut<T>(&mut self, location : BlobLocation) -> *mut T
	{
		if config::ENABLE_ASSERTS
		{
			let _ = self.check_valid_location_for_type::<T, TRIP_ASSERTS>(location);
		}

		let byte_ptr = &mut self.bytes[location.offset] as *mut u8;
		byte_ptr as *mut T
	}

	/// Convert buffer offsets into real pointers

	unsafe fn patch_references(&mut self)
	{
		assert!(!self.is_patched, "Can't call patch_references on a blob that's already patched");

		// Get the vec table from the blob's header

		// NOTE (rs) By the time we're in here we know the vec table contains
		//  at least somewhat reasonable data.

		let (vec_table_count, vec_table_location) =
		{
			let header = self.get_header();
			(header.vec_table_count, header.vec_table_location)
		};

		let vec_table_ptr = self.location_as_ptr::<BlobLocation>(vec_table_location)
								as *const BlobLocation;

		// Convert the VecBlob at each location

		let vec_locations = std::slice::from_raw_parts(vec_table_ptr, vec_table_count);
		for &vec_location in vec_locations
		{
			// NOTE (rs) We lose any info about the vec's original type, hence treating
			//  them as Vec<u8> in all cases here.

			type ErasedVec = Vec<u8>;
			type UninitializedVec = std::mem::MaybeUninit<ErasedVec>;

			// Create two views of the same data, first copying the unpatched data out
			//  of the vec's original location, then switching to treat it as an
			//  uninitialized region to write a real (patched) vec

			let vec_blob = *self.location_as_ptr::<VecBlob>(vec_location);
			let patched_vec_target = &mut *self.location_as_ptr_mut::<UninitializedVec>(vec_location);

			// Construct the real vec (pointing into the data buffer)

			let patched_vec =
			{
				let vec_data_ptr = self.location_as_ptr_mut::<u8>(vec_blob.data_location) as *mut u8;
				Vec::from_raw_parts(vec_data_ptr, vec_blob.count, vec_blob.count)
			};

			// And emplace it into the appropriate buffer location

			patched_vec_target.write(patched_vec);
		}

		// Mark the blob as patched

		self.is_patched = true;
	}

	// Helper assertions to sanity check that things are being used as expected

	fn assert_valid_buffer(bytes : &[u8])
	{
		// We're going to be writing/reading the header directly at the start of the
		//  buffer, so it's important that it's always properly aligned even though the
		//  Vec allocation doesn't know how it's being used

		// BB (rs) A bit shaky... any way to request an explicit alignment? I believe
		//  on all the relevant platforms the default global allocator always uses
		//  8B alignment, so we're fine in practice...

		// BB (rs) AVX tho?

		let allocated_buffer_address = bytes.as_ptr() as usize;

		const MAX_ALIGNMENT_SIZE : usize = 8;
	
		ASSERT!
		{
			allocated_buffer_address % MAX_ALIGNMENT_SIZE == 0,
			"Expected allocated blob buffer to always be aligned to {}B (found {}B)",
				MAX_ALIGNMENT_SIZE,
				allocated_buffer_address % MAX_ALIGNMENT_SIZE,
		}
	}

	fn check_valid_location_for_type<T, const DO_ASSERTS : DoAsserts>(
		&self,
		location : BlobLocation) -> bool
	{
		// Data must start and end within the buffer

		if location.offset >= self.bytes.len()
		{
			ASSERT!
			{
				DO_ASSERTS == SKIP_ASSERTS,
				"Accessing bytes at invalid location {:x} > {:x}",
					location.offset,
					self.bytes.len(),
			}

			return false;
		}
		else if location.offset + T::BYTE_COUNT > self.bytes.len()
		{
			ASSERT!
			{
				DO_ASSERTS == SKIP_ASSERTS,
				"Accessing {}B of data at invalid location {:x} ({}B left)",
					T::BYTE_COUNT,
					location.offset,
					self.bytes.len() - location.offset,
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

		let byte_ptr = &self.bytes[location.offset] as *const u8;
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
}



#[repr(C)]
#[derive(Clone, Copy)]
struct VecBlob
{
	signature 		: usize,
	count 			: usize,
	data_location	: BlobLocation,
}

impl VecBlob
{
	pub fn new(count : usize, data_location : BlobLocation) -> Self
	{
		Self
		{
			signature : VEC_BLOB_SIGNATURE,
			count,
			data_location,
		}
	}
}



/// `BlobWriterPass` handles two things:
/// 	1. Recursion over the given structure, via the WriteBlob trait
/// 	2. Accounting for padding between data with different alignment

pub trait BlobWriterPass : Sized
{
	fn next_location(&self) -> BlobLocation;
	fn add_bytes(&mut self, bytes : &[u8]);

	fn on_handle_vec(
		&mut self,
		count 					: usize,
		self_location 			: BlobLocation,
		slice_start_location 	: BlobLocation);

	// Common blob writer implementation, not expected to be overridden

	fn add_slice<T>(&mut self, values : &[T]) -> BlobLocation
		where T : WriteBlob
	{
		// Early-out if there is no data to write. The specific offset returned doesn't
		//  matter since it'll never be read, so use a recognizable signature

		if values.is_empty()
		{
			return BlobLocation::new(EMPTY_VEC_SIGNATURE);
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
			value.handle_references(self, value_location);
		}

		// Return the location of the slice, so other the caller can know where the
		//  data ended up within the blob

		slice_start_location
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

		let value_ptr = value as *const T as *const u8;
		let data_bytes = unsafe { std::slice::from_raw_parts(value_ptr, T::BYTE_COUNT) };
	
		self.add_bytes(data_bytes);
	}

	fn handle_vec<T>(&mut self, self_location : BlobLocation, values : &Vec<T>)
		where T : WriteBlob
	{
		// Write out a contigious slice continaining all the values

		let slice_start_location = self.add_slice(values.as_slice());

		// Rewrite original vec data to put it in blob-local space (if applicable)

		self.on_handle_vec(values.len(), self_location, slice_start_location);

		// NOTE (rs) Treating rust Vec as a black box with no field layout guarantees

		ASSERT_LAYOUT_EQ!(VecBlob, Vec<T>);
	}
}

/// `BlobWriter` uses the `BlobWriterPass` machinery to create and add data to a `Blob`.
/// Once all data is written, it appends additional metadata needed to patch itra-blob
/// references (`Vec`s, etc.)

struct BlobWriter
{
	blob 					: Blob,
	vec_locations 			: Vec<BlobLocation>,
}

impl BlobWriter
{
	fn new() -> Self
	{
		Self
		{
			blob 			: Blob::empty(),
			vec_locations 	: Vec::new(),
		}
	}

	fn finish(mut self, data_start_location : BlobLocation) -> Blob
	{
		// Track the total size of the data region

		let data_end_location = self.next_location();
		let data_byte_count = data_end_location.offset - data_start_location.offset;

		ASSERT! { data_byte_count > 0 }

		let header = self.blob.get_header_mut();

		header.data_byte_count = data_byte_count;
		header.data_location = data_start_location;

		// Track all the vecs we added to the blob

		self.add_vec_table();

		// Unwrap the blob data for consumption

		self.blob
	}

	/// returns location of the start of the slice (post-padding)

	fn write_slice<T>(&mut self, slice : &[T]) -> BlobLocation
		where T : WriteBlob
	{
		ASSERT! { !slice.is_empty(), "Should early exit before calling write_slice with no data" }

		let padding_start_location = self.next_location();

		// First, do a "dry run" which just counts the total number of bytes we're
		//  going to write (including padding and all reference fields)
		
		let expected_end_location =
		{
			let mut dry_run = BlobWriterDryRun::new(padding_start_location);
			dry_run.add_slice(slice);
			
			dry_run.next_location()
		};

		// Pre-allocate enough space to fit the total size we computed and write data

		let expected_bytes_written = expected_end_location.offset - padding_start_location.offset;
		self.blob.bytes.reserve_exact(expected_bytes_written);

		let slice_start_location = self.add_slice(slice);
		let actual_end_location = self.next_location();

		// Reallocation isn't problematic, but can be costly, hence all the effort to
		//  count bytes and pre-allocate up front

		ASSERT!
		{
			actual_end_location == expected_end_location,
			"Reallocated during blob write (expected {}B, wrote {}B)",
				expected_bytes_written,
				actual_end_location.offset - padding_start_location.offset,
		}

		slice_start_location
	}

	fn add_vec_table(&mut self)
	{
		// Don't bother writing anything if there aren't any locations to write. This
		//  keeps the empty vec table signature around, which can be helpful for debugging.

		if self.vec_locations.is_empty()
		{
			return;
		}

		// First, append all the vec locations for the blob to the end of the data

		// BB (rs) Need to do a bit of shuffling to avoid mostly-reasonable borrow
		//  checker semantics when calling self.write_slice(...). It could technically
		//  end up modifying self.vec_locations, though we know it won't because we
		//  aren't adding a new vecs.

		// NOTE (rs) In theory we could swap vec_locations back into self.vec_locations,
		//  but in practice we no longer need it, so just dropping it here.

		let vec_table = std::mem::take(&mut self.vec_locations);
		let vec_table_location = self.write_slice(&vec_table);

		ASSERT! { self.vec_locations.is_empty() }

		// Second, cache off info about the newly-written vec table so we can reconstruct it later

		let header = self.blob.get_header_mut();
		ASSERT! { header.vec_table_count == 0, "Called add_vec_table multiple times?" };

		header.vec_table_count = vec_table.len();
		header.vec_table_location = vec_table_location;
	}
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
		//  can read it back in later, even if the layout of Vec itself changes

		let vec_img : &mut VecBlob = unsafe { &mut *self.blob.location_as_ptr_mut(self_location) };
		*vec_img = VecBlob::new(count, slice_start_location);

		// Keep track of all vecs so we know where to look when we go to reconstruct them

		self.vec_locations.push(self_location);
	}
}

/// `BlobWriterDryRun` is a pass that simply walks the input structure to simulate
/// what `BlobWriter` will do. This lets it produce an expected size of the entire
/// data blob so `BlobWriter` can pre-allocate enough space to avoid reallocating

struct BlobWriterDryRun
{
	end_location : BlobLocation,
}

impl BlobWriterDryRun
{
	fn new(start_location : BlobLocation) -> Self
	{
		Self
		{
			end_location : start_location,
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
		// Don't care about patching when we're just counting bytes
	}
}



// Anything that's plain ol' data can be trivially imaged

pub trait HasNoReferences : Copy+Sized {}

impl<T> WriteBlob for T
where
	T : HasNoReferences
{
	fn handle_references<Pass>(&self, _pass : &mut Pass, _self_location : BlobLocation)
		where Pass : BlobWriterPass
	{}
}

macro_rules! impl_has_no_references
{
	{$($type:ty),*,} =>
	{
		$(impl HasNoReferences for $type {})*
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
	where T : WriteBlob
{
	fn handle_references<Pass>(&self, pass : &mut Pass, self_location : BlobLocation)
		where Pass : BlobWriterPass
	{
		pass.handle_vec(self_location, self);
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

const EMPTY_BLOB_SIGNATURE 		: usize = compute_signature("NO*DATA*");
const EMPTY_VEC_TABLE_SIGNATURE : usize = compute_signature("NO*VECS*");
const VEC_BLOB_SIGNATURE 		: usize = compute_signature("VEC*BLOB");
const EMPTY_VEC_SIGNATURE 		: usize = compute_signature("EMPTYVEC");

const fn compute_signature<T>(text : &'static str) -> T
	where T : Sized+Copy
{
	let bytes = text.as_bytes();
	assert!(bytes.len() == T::BYTE_COUNT);

	unsafe
	{
		let eight_bytes = std::mem::transmute::<&u8, &[u8;8]>(&bytes[0]);
		std::mem::transmute_copy(eight_bytes)
	}

	// assert!(bytes.len() <= 8);

	// let mut signature = 0u64;

	// // Copy text bytes in reverse order, to match the order of bytes
	// //  when inspecting memory in a little-endian system

	// let mut next_byte_index = bytes.len();
	// while next_byte_index > 0
	// {
	// 	next_byte_index -= 1;
		
	// 	let chunk = (bytes[next_byte_index] as u64) << (next_byte_index * 8);
	// 	signature |= chunk;
	// }
		
	// unsafe
	// {
	// 	std::mem::transmute_copy(&signature)
	// }
}

fn get_padding_bytes(padding_byte_count : usize) -> &'static [u8]
{
	let full_sequence = "PADDING";

	ASSERT! { full_sequence.len() == 7 };
	ASSERT! { padding_byte_count <= 7 };

	&full_sequence.as_bytes()[0..padding_byte_count]
}
