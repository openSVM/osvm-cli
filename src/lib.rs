//! OSVM CLI Library
//! 
//! This library provides the core functionality for the OSVM CLI.
//! It includes utilities for managing SVMs, nodes, and SSH deployments.
//!
//! # Features
//!
//! - SVM Management: List, get details, and install SVMs
//! - Node Management: Deploy, monitor, and control validator and RPC nodes
//! - SSH Deployment: Deploy SVMs and nodes to remote hosts
//! - Interactive Dashboards: Monitor SVMs and nodes in real-time
//!
//! # Architecture
//!
//! The OSVM CLI is organized into several modules:
//!
//! - `utils`: Core utilities for SVM and node management
//! - `clparse`: Command-line parsing and argument definitions
//! - `main`: Main entry point and command handlers

pub mod utils;
pub mod clparse;
pub mod prelude;