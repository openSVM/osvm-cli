// TUI module for OSVM - Beautiful terminal interface
pub mod app;
pub mod code;
pub mod degen;
pub mod events;
pub mod graph;
pub mod graph_forensics;
pub mod logger;
pub mod screenshot_test;
pub mod swap;
pub mod widgets;

pub use app::OsvmApp;
pub use graph::{TransferData, WalletGraph, WalletNodeType};
pub use logger::{init_file_logger, log};
pub use screenshot_test::{
    ColorAssertionError,
    ColorAssertions,
    ColorRegion,
    ColorSummary,
    PaneValidation,
    ScreenshotDiff,
    ScreenshotTestRunner,
    // Tmux tiling support (btop-style)
    TileLayout,
    TiledCaptureResult,
    TiledTestBuilder,
    TiledTestResult,
    TiledTestScenario,
    // Basic tmux capture
    TmuxCapture,
    TmuxPane,
    TmuxTiledSession,
    TuiScreenshot,
};
