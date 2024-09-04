use crate::*;

use std::io::{ Read, Write };



pub const SKIP_CHECKS : bool = !config::ENABLE_ASSERTS;

pub enum OnFailure
{
	Ignore,
	Break,
}

pub struct CheckInfo
{
	pub check_string	: &'static str,
		file_path 		: &'static str,
	pub line_number 	: u32,
		ignore 			: bool
}

impl CheckInfo
{
	pub const fn new(
		check_string : &'static str,
		file_path : &'static str,
		line_number : u32) -> Self
	{
		Self
		{
			check_string,
			file_path,
			line_number,
			ignore : false,
		}
	}

	pub fn file_name(&self) -> &str
	{
		match self.file_path.rfind(std::path::MAIN_SEPARATOR)
		{
			Some(index) 	=> &self.file_path[index + 1..],
			None 			=> self.file_path,
		}
	}

	pub fn on_fail(&mut self) -> OnFailure
	{
		match config::FAILURE_MODE
		{
			config::FailureMode::Ignore =>
			{
				OnFailure::Ignore
			}
			config::FailureMode::Panic =>
			{
				panic!("panic on failed assert: {} @ {}:{}", self.check_string, self.file_name(), self.line_number);
			}
			config::FailureMode::Interactive =>
			{
				const DO_BREAK 		: u8 = b'w';
				const SKIP_CHECK 	: u8 = b'a';
				const SKIP_INSTANCE : u8 = b's';
				const SKIP_ALL 		: u8 = b'd';
		
				// BB (rs) Not very thread-safe... do we care?
		
				static mut IGNORE_ALL : bool = false;
		
				unsafe
				{
					if self.ignore || IGNORE_ALL
					{
						return OnFailure::Ignore;
					}
				}

				// Display controls prompt
				
				print!("\n");
				println!("  [{}] break", char::from(DO_BREAK));
				println!("  [{}] ignore instance", char::from(SKIP_INSTANCE));
				println!("  [{}] ignore future instances", char::from(SKIP_CHECK));
				println!("  [{}] ignore all checks", char::from(SKIP_ALL));
				print!  ("  > ");

				let _ = std::io::stdout().flush();

				// Wait for input to match
		
				let mut input_buffer = [0; 8];
		
				loop
				{
					let _ = std::io::stdin().read(&mut input_buffer);
			
					match input_buffer
					{
						[SKIP_INSTANCE, ..] =>
						{
							// Ignore this instance, but ask again next time
			
							return OnFailure::Ignore;
						}
						[SKIP_CHECK, ..] =>
						{
							// Ignore this instance and all future ones
			
							self.ignore = true;
							return OnFailure::Ignore;
						}
						[SKIP_ALL, ..] =>
						{
							// Ignore all future asserts
		
							unsafe
							{
								IGNORE_ALL = true;
							}
		
							return OnFailure::Ignore;
						}
						[DO_BREAK, ..] =>
						{
							return OnFailure::Break;
						}
						_ =>
						{
							// Continue waiting for valid input

							continue;
						}
					}
				}
			}
		}
	}
}

pub fn print_header(file_name : &str, line_number : u32, description : &str)
{
	use colored::*;
	eprintln!(
		"{} {}{}{}{} :: {}",
		"🛑 ERROR ".on_red(),
		"at ".dimmed(),
		file_name.dimmed(),
		":".dimmed(),
		format!("{}", line_number).dimmed(),
		description.red())
}

pub fn print_nice_backtrace()
{
	use colored::*;

	let backtrace = std::backtrace::Backtrace::force_capture();

	// BB (rs) AFAIK it's currently not possible to get nicely printed frames while
	//  also skipping the first/last N frame structs, hence futzing with strings here

	let backtrace_string = format!("{}", backtrace);

	struct Frame<'a>
	{
		function 	: String,
		source		: &'a str,
	}

	let mut include_frames = false;
	let mut frames = Vec::new();
	let mut lines = backtrace_string.lines();

	while let Some(function_line) = lines.next()
	{
		let source = match lines.next()
		{
			Some(source_line) =>
			{
				let trimmed = source_line.trim();
				match trimmed.strip_prefix("at ")
				{
					Some(clean) => clean,
					None 		=> trimmed,
				}
			}
			None => "",
		};

		// Ignore this call (and everything downstream) at the start of the trace

		if !include_frames
		{
			if function_line.contains("print_nice_backtrace")
			{
				include_frames = true;
			}

			continue;
		}

		// Ignore the last few internal bits at the end of the trace

		if function_line.contains("BaseThreadInitThunk")
		{
			break;
		}

		// Convert raw function string to a more useful version

		let raw_function = match function_line.find(": ")
		{
			Some(index) => &function_line[index+2..],
			None 		=> function_line,
		};

		// Replace outermost <> brackets with <...>

		let mut function = String::new();
		let mut bracket_depth = 0;
		for ch in raw_function.chars()
		{
			match ch
			{
				'<' =>
				{
					bracket_depth += 1;

					if bracket_depth > 1
					{
						continue;
					}
				}
				'>' =>
				{
					bracket_depth -= 1;

					if bracket_depth > 0
					{
						continue;
					}
					else
					{
						function.push_str("...");
					}
				}
				_ =>
				{
					if bracket_depth > 0
					{
						continue;
					}
				}
			}

			function.push(ch);
		}

		// Trim down function length. Internal calls can get pretty wild, and generally
		//  are't things we actually care about

		const MAX_FUNCTION_LENGTH : usize = 64;

		if function.chars().count() > MAX_FUNCTION_LENGTH
		{
			const PREFIX_TRIMMED : &str = "... ";

			let start_byte_offset = match function
											.char_indices()
											.rev()
											.nth(MAX_FUNCTION_LENGTH - 1)
			{
				Some((byte_index, _ch)) => byte_index,
				None => 0,
			};

			function = format!("{}{}", PREFIX_TRIMMED, &function.as_str()[start_byte_offset..]);
		}

		frames.push(Frame { function, source });

		// If we're running from the main function of the program end the trace there

		// BB (rs) To avoid tripping on some other random function called main, check
		//  that there's only one :: in the name.

		if raw_function.ends_with("::main") &&
			raw_function.rfind("::") == raw_function.find("::")
		{
			break;
		}
	}

	let mut max_function_width = 0;

	for frame in frames.iter()
	{
		let function_width = frame.function.chars().count();
		if function_width > max_function_width
		{
			max_function_width = function_width;
		}
	}

	// Collapse internal rust stuff, since we generally don't care about it
	
	fn is_internal_frame(frame : &Frame) -> bool
	{
		frame.source.contains("/rustc/")
	}

	let mut iter_frames = frames.iter().enumerate();

	while let Some((depth, frame)) = iter_frames.next()
	{
		if is_internal_frame(frame)
		{
			// Count internal frames after this one

			let mut last_internal_frame = frame;
			let mut last_internal_depth = depth;

			for (upcoming_depth, upcoming_frame) in iter_frames.clone()
			{
				if !is_internal_frame(upcoming_frame)
				{
					break;
				}

				last_internal_frame = upcoming_frame;
				last_internal_depth = upcoming_depth;
			}

			// Convert depth range to string
			
			let depth_string =
				if depth == last_internal_depth
				{
					format!("{}", depth)
				}
				else
				{
					format!("{}-{}", depth, last_internal_depth)
				};

			// Skip past subsequent internal frames

			let skip_count = last_internal_depth - depth;
			for _ in 0..skip_count
			{
				let _ = iter_frames.next();
			}

			// Print collapsed line

			const DISPLAY_LAST_INTERNAL_FRAME_FUNCTION : bool = true;

			let displayed_function = match DISPLAY_LAST_INTERNAL_FRAME_FUNCTION
			{
				true 	=> last_internal_frame.function.as_str(),
				false 	=> "...",
			};

			eprintln!(
				"{:>6} | {:max_function_width$} | {}",
				depth_string.blue(),
				displayed_function,
				"rust_internal".dimmed());
		}
		else
		{
			eprintln!(
				"{:>6} | {:max_function_width$} | {}{}",
				format!("{}", depth).blue(),
				frame.function,
				"at ".dimmed(),
				frame.source.dimmed());
		}
	}
}