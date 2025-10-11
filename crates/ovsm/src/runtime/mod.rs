//! Runtime execution for OVSM programs

mod environment;
mod evaluator;
mod value;

pub use environment::Environment;
pub use evaluator::Evaluator;
pub use value::Value;
