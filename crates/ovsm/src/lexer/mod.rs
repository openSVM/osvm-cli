//! Lexical analysis for OVSM
//!
//! Converts source text into a stream of tokens.

mod scanner;
mod token;

pub use scanner::Scanner;
pub use token::{Token, TokenKind};
