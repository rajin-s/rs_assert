


#[derive(Debug, PartialEq, Eq)]
pub enum FailureMode
{
	Panic,
	Ignore,
	Interactive,
}

#[cfg(feature="panic_on_failures")]
mod cfg
{
	use super::FailureMode;
	
	pub const ENABLE_ASSERTS 	: bool 			= true;
	pub const FAILURE_MODE 		: FailureMode	= FailureMode::Panic;
}

#[cfg(not(feature="panic_on_failures"))]
mod cfg
{
	use super::FailureMode;

	#[cfg(feature="enable_asserts")] 			pub const ENABLE_ASSERTS 	: bool 			= true;
	#[cfg(not(feature="enable_asserts"))] 		pub const ENABLE_ASSERTS 	: bool 			= false;

	#[cfg(feature="interactive_failures")] 		pub const FAILURE_MODE 		: FailureMode	= FailureMode::Interactive;
	#[cfg(not(feature="interactive_failures"))] pub const FAILURE_MODE 		: FailureMode	= FailureMode::Ignore;
}

pub use cfg::*;

