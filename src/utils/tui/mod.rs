// TUI module for OSVM - Beautiful terminal interface
pub mod app;
pub mod graph;
pub mod widgets;
pub mod events;
pub mod logger;
pub mod screenshot_test;

pub use app::OsvmApp;
pub use graph::{WalletGraph, WalletNodeType, TransferData};
pub use logger::{init_file_logger, log};
pub use screenshot_test::{
    TuiScreenshot, ScreenshotDiff, ScreenshotTestRunner,
    ColorRegion, ColorSummary, ColorAssertions, ColorAssertionError,
    // Tmux tiling support (btop-style)
    TileLayout, TmuxTiledSession, TmuxPane, TiledCaptureResult,
    TiledTestBuilder, TiledTestScenario, TiledTestResult, PaneValidation,
    // Basic tmux capture
    TmuxCapture,
};
