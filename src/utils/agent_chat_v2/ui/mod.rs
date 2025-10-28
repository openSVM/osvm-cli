//! User interface components and management

// Original modules
pub mod accessibility;
pub mod components;
pub mod display;
pub mod error_handling;
pub mod handlers;
pub mod input_validation;
pub mod layout;
pub mod loading;
pub mod onboarding;
pub mod theme;
pub mod visual_feedback;

// New UX enhancement modules
pub mod animations;
pub mod autocomplete;
pub mod cursor_management;
pub mod effects;
pub mod key_diagnostics;
pub mod layouts;
pub mod message_rendering;
pub mod search;
pub mod text_area_wrapper;
pub mod themes;
pub mod toast;
pub mod ux_orchestrator;

// Re-exports for convenience
pub use accessibility::*;
pub use components::*;
pub use display::*;
pub use error_handling::ChatError;
pub use handlers::*;
pub use input_validation::*;
pub use layout::*;
pub use loading::*;
pub use onboarding::*;
pub use visual_feedback::*;

// Export new UX components
pub use cursor_management::{CursorContext, SmartCursorManager, UserAction};
pub use message_rendering::{MessageRenderer, MessageType};
pub use ux_orchestrator::{setup_ux_orchestrator, UXOrchestrator};
