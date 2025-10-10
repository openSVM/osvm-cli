//! # OVSM Interpreter
//!
//! A runtime interpreter for the OVSM (Open Versatile Seeker Mind) language.
//!
//! ## Quick Start
//!
//! ```rust,no_run
//! use ovsm::{Scanner};
//!
//! let mut scanner = Scanner::new("$x = 42");
//! let tokens = scanner.scan_tokens().unwrap();
//! println!("Tokens: {:?}", tokens);
//! ```

#![allow(dead_code)]  // Remove after implementation
#![warn(missing_docs)]

// Module declarations
pub mod error;
pub mod lexer;

// Re-export main types
pub use error::{Error, Result};
pub use lexer::{Scanner, Token, TokenKind};
