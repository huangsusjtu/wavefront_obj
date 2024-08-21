//! Parsers for wavefront's `.obj` and `.mtl` file format for loading meshes.
#![crate_type = "lib"]
#![deny(warnings)]
#![deny(missing_docs)]
#![deny(unreachable_pub)]

pub use lex::ParseError;

mod lex;
mod util;

mod context;
pub mod mtl;
pub mod obj;
