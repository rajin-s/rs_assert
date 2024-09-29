#![feature(allocator_api)]			// for scratch allocator
#![feature(alloc_layout_extra)]		//  ...



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