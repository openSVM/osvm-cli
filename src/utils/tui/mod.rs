// TUI module for OSVM - Beautiful terminal interface
pub mod app;
pub mod graph;
pub mod widgets;
pub mod events;
pub mod logger;

pub use app::OsvmApp;
pub use graph::{WalletGraph, WalletNodeType, TransferData};
pub use logger::{init_file_logger, log};
