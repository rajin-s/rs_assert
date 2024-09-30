extern crate proc_macro;

use quote::quote;

#[proc_macro_derive(WriteBlob)]
pub fn derive_write_blob(input_stream : proc_macro::TokenStream) -> proc_macro::TokenStream
{
	let parsed_input = syn::parse_macro_input!(input_stream as syn::DeriveInput);

	match parsed_input.data
	{
		syn::Data::Struct(struct_data) =>
		{
			let struct_name = parsed_input.ident;
			let field_names = struct_data.fields.iter().map(|field| field.ident.as_ref().unwrap());

			// Struct just needs to pass through reference handling to all its fields,
			//  which are assumed to implement WriteBlob, either trivially (u8, etc.),
			//  as a fundamental reference type (Vec, Box, etc.), or as an arbitrary
			//  writeable type (ie. that has derived WriteBlob)

			let impl_block = quote!
			{
				#[automatically_derived]
				impl WriteBlob for #struct_name
				{
					fn handle_references<Pass>(
						&self,
						pass : &mut Pass,
						self_location : BlobLocation)
					where
						Pass : BlobWriterPass
					{
						#(
							self.#field_names.handle_references(
								pass,
								self_location + std::mem::offset_of!(Self, #field_names)
							);
						)*
					}
				}
			};
			
			impl_block.into()
		}
		syn::Data::Enum(_enum_data) => unimplemented!("Enums not yet supported"),
		syn::Data::Union(_union_data) => unimplemented!("Unions not yet supported"),
	}
}

#[proc_macro_derive(GetTypeInfo)]
pub fn derive_get_type_signature(input_stream : proc_macro::TokenStream) -> proc_macro::TokenStream
{
	let parsed_input = syn::parse_macro_input!(input_stream as syn::DeriveInput);

	match parsed_input.data
	{
		syn::Data::Struct(struct_data) =>
		{
			let struct_name = parsed_input.ident;
			let field_count = struct_data.fields.len();
			
			let field_types = struct_data.fields.iter().map(|field| &field.ty);
			let field_names = struct_data.fields.iter().map(
				|field| field.ident.as_ref().expect("Tuple structs not yet supported"));

			let impl_elements_and_signature = quote!
			{
				#[automatically_derived]
				impl GetTypeInfo for #struct_name
				{
					const TYPE_NAME 	: &'static str = stringify!(#struct_name);
					const TYPE_ELEMENTS : &[TypeElement] = &Self::TYPE_ELEMENT_ARRAY;
				}

				#[automatically_derived]
				impl TypeElements<#field_count> for #struct_name
				{
					const TYPE_ELEMENT_ARRAY : [TypeElement; #field_count] =
					[
						#(
							TypeElement::Named(
								stringify!(#field_names),
								&<#field_types>::TYPE_INFO
							),
						)*
					];
				}
			};
			
			impl_elements_and_signature.into()
		}
		syn::Data::Enum(_enum_data) => unimplemented!("Enums not yet supported"),
		syn::Data::Union(_union_data) => unimplemented!("Unions not yet supported"),
	}
}