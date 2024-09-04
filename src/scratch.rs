use crate::*;

use std::alloc::{ Allocator, Layout, AllocError };
use std::cell::RefCell;
use std::ptr::NonNull;



#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Address
{
	ptr : *const u8,
}

impl Address
{
	pub fn from_ptr<T>(ptr : *const T) -> Self
	{
		Self
		{
			ptr : ptr as *const u8,
		}
	}

	pub fn from_ref<T>(x : &T) -> Self
	{
		Self::from_ptr(x as *const T)
	}

	pub fn byte_offset(&self, byte_offset : usize) -> Self
	{
		// NOTE (rs) Using byte_offset is safe, since it's impossible to dereference
		//  this pointer. The resulting address could well be invalid, but it's up to
		//  the caller to decide if they care / how they want to handle it.

		unsafe
		{
			Self::from_ptr(self.ptr.offset(byte_offset as isize))
		}
	}
}



enum InitAllocation
{
	None,
	Zeroed,
}



// NOTE (rs) If some workload requries more than the available scratch space then it probably
//  shouldn't be working in scratch memory. If we ever need to support such a situation then
//  we could split ScratchContext to have a version that takes an arbitrary ScratchSpace.

pub const DEFAULT_SCRATCH_SIZE : usize = 10240; // 10KiB

// BB (rs) Does anything ever need > 8 bytes alignment???

const SCRATCH_ALIGNMENT : usize = 8;




struct ScratchSpaceState
{
	next_alloc_offset		: usize,
	current_context_depth	: u32,
}

struct ScratchSpace<const SIZE : usize>
{
	state 	: ScratchSpaceState,
	buffer 	: [u8; SIZE],
}

struct ScratchSpaceRef<'a>
{
	state	: &'a mut ScratchSpaceState,
	buffer	: &'a mut [u8],
}

impl<const SIZE : usize> ScratchSpace<SIZE>
{
	pub fn new() -> Self
	{
		Self
		{
			state : ScratchSpaceState
			{
				next_alloc_offset 		: 0,
				current_context_depth	: 0,
			},
			buffer : [0x77; SIZE],
		}
	}

	pub fn get_ref<'a>(&'a mut self) -> ScratchSpaceRef<'a>
	{
		ScratchSpaceRef
		{
			state 	: &mut self.state,
			buffer 	: &mut self.buffer,
		}
	}
}

impl ScratchSpaceRef<'_>
{
	pub fn allocated_range(&self) -> std::ops::Range<Address>
	{
		let start_addr = Address::from_ref(&self.buffer[0]);
		start_addr..start_addr.byte_offset(self.state.next_alloc_offset)
	}

	pub fn context_range(&self, context : &ScratchContextState) -> std::ops::Range<Address>
	{
		let buffer_start_addr = Address::from_ref(&self.buffer[0]);

		let start_addr = buffer_start_addr.byte_offset(context.offset_range.start);
		let end_addr = buffer_start_addr.byte_offset(context.offset_range.end);

		start_addr..end_addr
	}

	pub fn byte_count_with_padding(layout : Layout) -> usize
	{
		ASSERT!
		{
			layout.align() <= SCRATCH_ALIGNMENT,
			"Allocating with {}B alignment, but scratch allocator is fixed to {}B alignment",
				layout.align(),
				SCRATCH_ALIGNMENT,
		}

		let padding_byte_count = layout.padding_needed_for(SCRATCH_ALIGNMENT);
		layout.size() + padding_byte_count
	}

	pub fn push_new_context(&mut self) -> ScratchContextState
	{
		self.state.current_context_depth += 1;

		// NOTE (rs) Safe since we just added one, and thus the value can never be zero

		let nz_depth = unsafe
		{
			std::num::NonZeroU32::new_unchecked(self.state.current_context_depth)
		};

		let start_offset = self.state.next_alloc_offset;

		ScratchContextState
		{
			nz_depth,
			offset_range		: start_offset..start_offset,
			live_alloc_count	: 0,
		}
	}

	pub fn pop_context(&mut self, context : &ScratchContextState)
	{
		ASSERT!
		{
			self.state.current_context_depth >= context.depth(),
			"Popping outer context (depth={}) before inner context (depth={})",
				context.depth(),
				self.state.current_context_depth,
		}

		ASSERT!
		{
			self.state.current_context_depth <= context.depth(),
			"Popping invalid context (depth={}, current depth={})",
				context.depth(),
				self.state.current_context_depth,
		}

		ASSERT!
		{
			context.live_alloc_count == 0,
			"Popping context (depth={}) with {} live allocations",
				context.depth(),
				context.live_alloc_count,
		}

		self.state.current_context_depth = context.depth() - 1;
		self.state.next_alloc_offset = context.offset_range.start;
	}

	pub fn reserve_space_add_padding(
		&mut self,
		layout 	: Layout,
		init	: InitAllocation,
		context : &mut ScratchContextState) -> Result<NonNull<[u8]>, AllocError>
	{
		ASSERT!
		{
			context.depth() >= self.state.current_context_depth,
			"Cannot allocate from outer context (depth={}) while inner context is active (depth={})",
				context.depth(),
				self.state.current_context_depth,
		}

		ASSERT!
		{
			context.depth() <= self.state.current_context_depth,
			"Allocating from invalid context (depth={}, current depth={})",
				context.depth(),
				self.state.current_context_depth,
		}

		ASSERT!
		{
			context.offset_range.end == self.state.next_alloc_offset,
			"Context allocation range ({}..{}) got out of sync with buffer allocation offset ({})",
				context.offset_range.start,
				context.offset_range.end,
				self.state.next_alloc_offset,
		}

		let buffer_size = self.buffer.len();

		let data_byte_count = layout.size();
		let total_byte_count = Self::byte_count_with_padding(layout);

		let alloc_offset = self.state.next_alloc_offset;
		let new_next_alloc_offset = alloc_offset + total_byte_count;

		if new_next_alloc_offset > buffer_size
		{
			FAIL!
			{
				"Failed to allocate {}B ({} + {} padding) ({}/{}B remaining)",
					total_byte_count,
					data_byte_count,
					total_byte_count - data_byte_count,
					buffer_size - self.state.next_alloc_offset,
					buffer_size,
			}

			return Err(AllocError);
		}
		
		// Update scratch space allocation pointer

		self.state.next_alloc_offset = new_next_alloc_offset;

		// Update context bookkeeping

		context.offset_range.end = new_next_alloc_offset;

		// Zero out reserved space, if desired

		match init
		{
			InitAllocation::Zeroed =>
			{
				// NOTE (rs) This compiles to a single memset call, thanks to optimization

				for offset in alloc_offset..alloc_offset + data_byte_count
				{
					self.buffer[offset] = 0x00;
				}
			}
			InitAllocation::None => {},
		}
		
		// Allocation was successful, go ahead and use it

		let alloc_bytes = &self.buffer[alloc_offset..new_next_alloc_offset];
		Ok(alloc_bytes.into())
	}

	pub fn allocate(
		&mut self,
		layout 	: Layout,
		init	: InitAllocation,
		context : &mut ScratchContextState) -> Result<NonNull<[u8]>, AllocError>
	{
		// Try to reserve scratch buffer space

		let alloc_bytes = match self.reserve_space_add_padding(layout, init, context)
		{
			Ok(x) 		=> x,
			Err(err) 	=> return Err(err),
		};

		// Update context bookkeeping

		context.live_alloc_count += 1;

		// Convert offet range into buffer

		Ok(alloc_bytes)
	}

	pub fn grow(
		&mut self,
		ptr			: NonNull<u8>,
		old_layout 	: Layout,
		new_layout 	: Layout,
		init		: InitAllocation,
		context 	: &mut ScratchContextState) -> Result<NonNull<[u8]>, AllocError>
	{
		// Sanity check that this is was a real allocation

		let addr = Address::from_ptr(ptr.as_ptr());

		ASSERT!
		{
			self.context_range(context).contains(&addr),
			"Growing invalid allocation {:p}",
				ptr,
		}

		// Can only grow if we haven't allocated anything new past the original allocation

		let old_padded_byte_count = Self::byte_count_with_padding(old_layout);
		let old_end_addr = addr.byte_offset(old_padded_byte_count);

		let allocated_range = self.allocated_range();

		if old_end_addr != allocated_range.end
		{
			FAIL!
			{
				"Can't grow allocation (by {}B) after something else has been allocated. \
				 Can it be pre-allocated to a known max size?",
					new_layout.size() - old_layout.size(),
			}

			return Err(AllocError);
		}

		// Can't change alignment

		if old_layout.align() != new_layout.align()
		{
			FAIL!
			{
				"Can't change allocation alignment ({}B -> {}B) when growing",
					old_layout.align(),
					new_layout.align(),
			}

			return Err(AllocError);
		}

		// Advance the allocation index as much as needed to extend the original allocation

		let new_padded_byte_count = Self::byte_count_with_padding(new_layout);

		ASSERT!
		{
			new_padded_byte_count >= old_padded_byte_count,
			"Asking to grow to a smaller allocation ({} -> {}B)",
				old_padded_byte_count,
				new_padded_byte_count,
		}

		// NOTE (rs) Could end up growing an allocation into its own padding, meaning 0
		//  new allocated bytes

		let added_byte_count = new_padded_byte_count - old_padded_byte_count;
		if added_byte_count == 0
		{
			return Ok(NonNull::slice_from_raw_parts(ptr, new_padded_byte_count));
		}

		// Reserve the space...

		let layout_additional = Layout::from_size_align(added_byte_count, new_layout.align())
											.expect("invalid layout");

		match self.reserve_space_add_padding(layout_additional, init, context)
		{
			Ok(_) 		=> {},
			Err(err)	=> return Err(err),
		}

		// ... and consider the original allocation grown

		Ok(NonNull::slice_from_raw_parts(ptr, new_padded_byte_count))
	}

	pub fn deallocate(
		&mut self,
		ptr			: NonNull<u8>,
		layout 		: Layout,
		context 	: &mut ScratchContextState)
	{
		// Sanity check that this was a real allocation

		let addr = Address::from_ptr(ptr.as_ptr());
		
		ASSERT!
		{
			self.context_range(context).contains(&addr),
			"Deallocating unexpected pointer {:p}",
				ptr,
		}
		
		ASSERT!
		{
			context.live_alloc_count > 0,
			"Scratch context has no tracked allocations to deallocate",
		}
		
		// Remove it from the set of live allocations
		
		context.live_alloc_count -= 1;
		
		// If this was the last thing we allocated then roll back the next pointer
		//  to reclaim the space for a future allocation

		// NOTE (rs) You're allowed to deallocate something from a previous context,
		//  but we can't reclaim the space because that would invalidate any inner
		//  context's start_offset. Could relax this if start_offset was set on
		//  first allocation instead of on creation, but that makes it harder to
		//  reason about nested context behavior.

		if context.depth() != self.state.current_context_depth
		{
			return;
		}
		
		// NOTE (rs) If we allocated something else in the meantime then the buffer space won't
		//  be reclaimed until the current context is popped

		let padded_byte_count = Self::byte_count_with_padding(layout);
		let alloc_end_addr = addr.byte_offset(padded_byte_count);

		if alloc_end_addr == self.allocated_range().end
		{
			self.state.next_alloc_offset -= padded_byte_count;
			context.offset_range.end -= padded_byte_count;
		}
	}
}



// Scratch context

struct ScratchContextState
{
	nz_depth 			: std::num::NonZeroU32,
	offset_range		: std::ops::Range<usize>,
	live_alloc_count	: usize,
}

impl ScratchContextState
{
	pub fn depth(&self) -> u32
	{
		self.nz_depth.into()
	}
}

// BB (rs) Can't make this generic over scratch space size, since thread_local / static
//  doesn't support generics

pub struct ScratchContext
{
	// NOTE (rs) Must be shoved into a RefCell so Allocator::allocate can take &self

	state : RefCell<ScratchContextState>,
}

impl ScratchContext
{
	thread_local!
	{
		static SPACE : RefCell<ScratchSpace<DEFAULT_SCRATCH_SIZE>> =
			RefCell::new(ScratchSpace::new());
	}

	pub fn new() -> Self
	{
		Self::SPACE.with(|space| Self::new_impl(space.borrow_mut().get_ref()))
	}

	pub fn new_vec<T>(&self) -> Vec<T, &Self>
	{
		Vec::new_in(self)
	}

	pub fn vec_with_capacity<T>(&self, alloc_count : usize) -> Vec<T, &Self>
	{
		Vec::with_capacity_in(alloc_count, self)
	}

	pub fn new_box<T : Sized>(&self, value : T) -> Box<T, &Self>
	{
		Box::new_in(value, self)
	}

	fn new_impl(mut space : ScratchSpaceRef) -> Self
	{
		let state = space.push_new_context();
		Self { state : RefCell::new(state) }
	}

	fn drop_impl(&mut self, mut space : ScratchSpaceRef)
	{
		space.pop_context(&mut self.state.borrow_mut());
	}

	fn allocate_impl(
		&self,
		layout 		: Layout,
		init 		: InitAllocation,
		mut space 	: ScratchSpaceRef) -> Result<NonNull<[u8]>, AllocError>
	{
		space.allocate(layout, init, &mut self.state.borrow_mut())
	}

	fn grow_impl(
		&self,
		ptr			: NonNull<u8>,
		old_layout 	: Layout,
		new_layout 	: Layout,
		init		: InitAllocation,
		mut space 	: ScratchSpaceRef) -> Result<NonNull<[u8]>, AllocError>
	{
		space.grow(ptr, old_layout, new_layout, init, &mut self.state.borrow_mut())
	}

	fn deallocate_impl(
		&self,
		ptr			: NonNull<u8>,
		layout 		: Layout,
		mut space 	: ScratchSpaceRef)
	{
		space.deallocate(ptr, layout, &mut self.state.borrow_mut())
	}
}

impl Drop for ScratchContext
{
	fn drop(&mut self)
	{
		Self::SPACE.with(|space| self.drop_impl(space.borrow_mut().get_ref()))
	}
}

unsafe impl Allocator for ScratchContext
{
	fn allocate(&self, layout : Layout) -> Result<NonNull<[u8]>, std::alloc::AllocError>
	{
		ScratchContext::SPACE.with(|space|
			self.allocate_impl(
					layout,
					InitAllocation::None,
					space.borrow_mut().get_ref()))
	}

	fn allocate_zeroed(&self, layout : Layout) -> Result<NonNull<[u8]>, std::alloc::AllocError>
	{
		ScratchContext::SPACE.with(|space|
			self.allocate_impl(
					layout,
					InitAllocation::Zeroed,
					space.borrow_mut().get_ref()))
	}
	
	unsafe fn grow(
        &self,
        ptr			: NonNull<u8>,
        old_layout	: Layout,
        new_layout	: Layout,
    ) -> Result<NonNull<[u8]>, AllocError>
	{
		ScratchContext::SPACE.with(|space|
			self.grow_impl(
					ptr,
					old_layout,
					new_layout,
					InitAllocation::None,
					space.borrow_mut().get_ref()))
	}

	unsafe fn grow_zeroed(
        &self,
        ptr			: NonNull<u8>,
        old_layout	: Layout,
        new_layout	: Layout,
    ) -> Result<NonNull<[u8]>, AllocError>
	{
		ScratchContext::SPACE.with(|space|
			self.grow_impl(
					ptr,
					old_layout,
					new_layout,
					InitAllocation::Zeroed,
					space.borrow_mut().get_ref()))
	}

	unsafe fn deallocate(&self, ptr : NonNull<u8>, layout : Layout)
	{
		ScratchContext::SPACE.with(|space|
			self.deallocate_impl(ptr, layout, space.borrow_mut().get_ref()))
	}

	unsafe fn shrink(
        &self,
        ptr			: NonNull<u8>,
        old_layout	: Layout,
        new_layout	: Layout,
    ) -> Result<NonNull<[u8]>, AllocError>
	{
		// Can't change alignment

		if old_layout.align() != new_layout.align()
		{
			FAIL!
			{
				"Can't change allocation alignment ({}B -> {}B) when growing",
					old_layout.align(),
					new_layout.align(),
			}

			return Err(AllocError);
		}

		// BB (rs) Could do the same space reclaiming that deallocation does, but
		//  for now just saying that the memory will be reclaimed when the context pops

		ASSERT!
		{
			new_layout.size() < old_layout.size(),
			"Asking to shrink to a larger allocation ({} -> {}B)",
				old_layout.size(),
				new_layout.size(),
		}

		Ok(NonNull::slice_from_raw_parts(ptr, new_layout.size()))
	}
}