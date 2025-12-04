//! OVSM Language Server
//!
//! Run this binary to start the OVSM LSP server.
//! The server communicates via stdin/stdout using the LSP protocol.
//!
//! # Usage
//!
//! ```bash
//! # Run the LSP server (usually invoked by your editor)
//! ovsm-lsp
//!
//! # With debug logging
//! RUST_LOG=debug ovsm-lsp
//! ```
//!
//! # Editor Integration
//!
//! ## VS Code
//!
//! Install the OVSM extension or configure manually in settings.json:
//! ```json
//! {
//!     "ovsm.lsp.path": "/path/to/ovsm-lsp"
//! }
//! ```
//!
//! ## Neovim (with nvim-lspconfig)
//!
//! ```lua
//! require('lspconfig').ovsm.setup{
//!     cmd = { "ovsm-lsp" },
//!     filetypes = { "ovsm" },
//! }
//! ```
//!
//! ## Helix
//!
//! Add to `~/.config/helix/languages.toml`:
//! ```toml
//! [[language]]
//! name = "ovsm"
//! scope = "source.ovsm"
//! file-types = ["ovsm"]
//! language-server = { command = "ovsm-lsp" }
//! ```

use ovsm_lsp::OvsmLanguageServer;
use tower_lsp::{LspService, Server};
use tracing_subscriber::{fmt, layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};

#[tokio::main]
async fn main() {
    // Initialize logging
    tracing_subscriber::registry()
        .with(EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")))
        .with(fmt::layer().with_writer(std::io::stderr))
        .init();

    tracing::info!(
        "Starting OVSM Language Server v{}",
        env!("CARGO_PKG_VERSION")
    );

    // Create the LSP service
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(OvsmLanguageServer::new);

    // Run the server
    Server::new(stdin, stdout, socket).serve(service).await;

    tracing::info!("OVSM Language Server shutting down");
}
