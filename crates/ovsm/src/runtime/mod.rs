//! Runtime execution for OVSM programs

mod value;
mod environment;
mod evaluator;

pub use value::Value;
pub use environment::Environment;
pub use evaluator::Evaluator;
