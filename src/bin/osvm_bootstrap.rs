//! OSVM Bootstrap Binary
//!
//! This binary serves as the entry point for OSVM. It determines whether to:
//! 1. Launch OSVM runtime in a microVM (when on host)
//! 2. Run OSVM agent directly (when already inside microVM)

use anyhow::{Context, Result};
use std::env;

use osvm::services::microvm_launcher::{
    get_default_osvm_config, is_running_in_microvm, MicroVmLauncher,
};

#[tokio::main]
async fn main() -> Result<()> {
    // Check if we should skip microVM isolation
    let skip_microvm = env::var("OSVM_SKIP_MICROVM")
        .map(|v| v == "1" || v.to_lowercase() == "true")
        .unwrap_or(false);

    if skip_microvm {
        println!("⚠️  OSVM_SKIP_MICROVM is set - running without microVM isolation");
        return run_osvm_agent_directly().await;
    }

    // Determine execution mode
    if is_running_in_microvm() {
        println!("🔐 Running inside microVM - starting OSVM agent");
        run_osvm_agent_directly().await
    } else {
        println!("🚀 Running on host - launching OSVM in microVM");
        launch_in_microvm().await
    }
}

/// Launch OSVM runtime inside a microVM
async fn launch_in_microvm() -> Result<()> {
    println!("🚀 Launching OSVM in microVM for enhanced security...");

    // Check for Firecracker availability
    match check_firecracker_available() {
        Ok(()) => {
            println!("✅ Firecracker detected");
        }
        Err(_e) => {
            println!("⚠️  Firecracker not found - falling back to direct execution");
            println!("   For enhanced security, install Firecracker:");
            println!("   https://github.com/firecracker-microvm/firecracker");
            println!();
            return run_osvm_agent_directly().await;
        }
    }

    // Create microVM launcher
    let launcher = match MicroVmLauncher::new() {
        Ok(l) => l,
        Err(e) => {
            eprintln!("❌ Failed to create microVM launcher: {}", e);
            println!("⚠️  MicroVM launcher initialization failed - using direct execution");
            return run_osvm_agent_directly().await;
        }
    };

    // Get default configuration
    let config = get_default_osvm_config();

    println!("🔧 MicroVM Configuration:");
    println!("   Memory: {} MB", config.memory_mb);
    println!("   vCPUs: {}", config.vcpus);
    println!();

    // Launch the microVM
    match launcher.launch_osvm_runtime(config) {
        Ok(handle) => {
            println!("✅ OSVM runtime microVM launched successfully");
            println!("🔐 Agent running in isolated environment");
            println!();

            // Keep the process alive and monitor the microVM
            // In a real implementation, this would set up communication
            // and forward user interactions to the microVM
            println!("ℹ️  MicroVM is running. Press Ctrl+C to stop.");

            // Wait for Ctrl+C
            tokio::signal::ctrl_c()
                .await
                .context("Failed to listen for Ctrl+C")?;

            println!("\n🛑 Shutting down OSVM microVM...");
            if let Err(e) = handle.shutdown() {
                eprintln!("❌ Error during microVM shutdown: {}", e);
            }

            println!("✅ OSVM shutdown complete");
            Ok(())
        }
        Err(e) => {
            eprintln!("❌ Failed to launch microVM: {}", e);
            println!("⚠️  Falling back to direct execution");
            println!();
            run_osvm_agent_directly().await
        }
    }
}

/// Run OSVM agent directly (fallback or when in microVM)
async fn run_osvm_agent_directly() -> Result<()> {
    // Set environment variable to indicate we're in the agent runtime
    env::set_var("OSVM_AGENT_MODE", "1");

    // Call the original OSVM agent chat interface
    osvm::utils::agent_chat_v2::run_advanced_agent_chat()
        .await
        .context("Failed to run OSVM agent")?;

    Ok(())
}

/// Check if Firecracker is available on the system
fn check_firecracker_available() -> Result<()> {
    use std::process::Command;

    let output = Command::new("which")
        .arg("firecracker")
        .output()
        .context("Failed to check for Firecracker")?;

    if !output.status.success() {
        return Err(anyhow::anyhow!("Firecracker binary not found in PATH"));
    }

    Ok(())
}
