//! Lexical analysis for OVSM
//!
//! Converts source text into a stream of tokens.

mod token;
mod scanner;

pub use token::{Token, TokenKind};
pub use scanner::Scanner;
