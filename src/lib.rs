// rs_assert library

pub mod config;
pub mod assert;
pub mod assert_internal;

#[allow(unused_imports)] // Consumers want macros in crate scope
pub use assert::*;