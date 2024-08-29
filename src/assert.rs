#![allow(unused_imports)]

use crate::*;



// BB (rs) Not portable! Not using std::intrinsics::breakpoint() since that causes
//  the breakpoint to "appear" in this file, instead of the file where the ASSERT
//  check is written.

// NOTE (rs) The fact that this is a macro is important, again, to prevent the breakpoint
//  from appearing in this file instead of the relevant source.

#[cfg(target_arch="x86_64")]
#[macro_export]
macro_rules! debug_break
{
	() => { unsafe { std::arch::asm!("int 3"); } }
}



// Common macro for validating some code assumption. Checks can be compiled out
//  entirely (based on config features), so shouldn't produce any side effects.

#[macro_export]
macro_rules! ASSERT
{
	{ $f_check:expr, $( $e:expr ),*, } =>
	{
		if assert_internal::SKIP_CHECKS
		{
			// Don't even run the check if asserts aren't enabled
		}
		else if ($f_check) == false
		{
			// Check has failed...

			unsafe
			{
				// Print some general information about the check

				static mut CHECK_INFO : assert_internal::CheckInfo =
					assert_internal::CheckInfo::new(
													stringify!($f_check),
													file!(),
													line!());

				eprint!("ERROR @ {}:{} :: ", CHECK_INFO.file_name(), CHECK_INFO.line_number);
				eprintln!($($e,)*);

				assert_internal::print_nice_backtrace();

				// Let interactive assert machinery do its thing, if desired

				match CHECK_INFO.on_fail()
				{
					assert_internal::OnFailure::Ignore 	=> {}
					assert_internal::OnFailure::Break 	=> { debug_break!(); }
				}
			}
		}
	};
	{ $f_check:expr, $( $e:expr ),* } =>
	{
		ASSERT!{ $f_check, $($e,)* };
	};
	{ $f_check:expr } =>
	{
		ASSERT!{ $f_check, "Failed {}", stringify!($f_check) };
	};
}



#[macro_export]
macro_rules! FAIL
{
	{ $( $e:expr ),*, } =>
	{
		ASSERT!{ false, $($e,)* }
	};
	{ $( $e:expr ),* } =>
	{
		FAIL!{ $($e,)* }
	};
}