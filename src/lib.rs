#![allow(clippy::assertions_on_constants)] // Allow asserts on constants (for const functions)

#![feature(allocator_api)]			// for scratch allocator
#![feature(alloc_layout_extra)]		//  ...



// Proc macros

pub use proc_macro::*;

// General library config

pub mod config;

// Assertions

pub mod assert_internal;
pub use assert_macro::*;

// Scratch allocation

pub mod scratch;
pub use scratch::*;

// ID types

pub mod id;
pub use id::*;

// Memory images

pub mod blob;
pub mod type_signature;
pub use type_signature::*;



// Tests
// BB (rs) Managing features for tests like this feels gross

#[test]
#[should_panic]
fn valid_build_config()
{
	ASSERT! { false, "Expected panic_on_asserts for tests" }
}