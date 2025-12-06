use clap::{Arg, ArgAction, Command};

/// Build the doctor command
pub fn build_doctor_command() -> Command {
    Command::new("doctor")
        .about("Comprehensive system health check and repair")
        .arg(
            Arg::new("check_all")
                .long("check-all")
                .action(ArgAction::SetTrue)
                .help("Run comprehensive health check"),
        )
        .arg(
            Arg::new("fix")
                .long("fix")
                .action(ArgAction::SetTrue)
                .help("Attempt to fix detected issues automatically"),
        )
        .arg(
            Arg::new("system_only")
                .long("system-only")
                .action(ArgAction::SetTrue)
                .help("Check only system-level dependencies"),
        )
        .arg(
            Arg::new("user_only")
                .long("user-only")
                .action(ArgAction::SetTrue)
                .help("Check only user-level dependencies"),
        )
        .arg(
            Arg::new("verbose")
                .long("verbose")
                .short('v')
                .action(ArgAction::Count)
                .help("Detailed diagnostic output"),
        )
        .after_help(r#"
TOP 10 POPULAR USAGES:
━━━━━━━━━━━━━━━━━━━━━━━━

  1. osvm doctor
     Run quick health check for common issues.
     💡 Checks: config, RPC connectivity, dependencies.

  2. osvm doctor --fix
     Automatically fix detected issues.
     💡 Safe fixes only - creates backups before modifying.

  3. osvm doctor --check-all
     Comprehensive check of all system components.
     💡 Includes: Solana config, MCP servers, microVM setup.

  4. osvm doctor --system-only
     Check only system-level dependencies.
     💡 Checks: Rust, Solana CLI, system libraries.

  5. osvm doctor --user-only
     Check only user-level configuration.
     💡 Checks: keypair, config files, OSVM settings.

  6. osvm doctor -v
     Verbose output with detailed diagnostics.
     💡 Use -vv or -vvv for even more detail.

  7. osvm doctor --check-all --fix
     Full check with automatic repair.
     💡 Best for initial setup or after updates.

  8. osvm doctor && osvm mcp list
     Verify health before checking MCP servers.
     💡 Good workflow: doctor → mcp → chat

  9. RUST_LOG=debug osvm doctor -vvv
     Maximum verbosity for debugging issues.
     💡 Useful for reporting bugs to maintainers.

 10. osvm doctor --fix && osvm chat
     Fix issues then launch chat immediately.
     💡 Ensures clean state before using AI features.

💡 WHAT DOCTOR CHECKS:
  ┌─────────────────────────────────────────────┐
  │ System Level                                │
  ├─────────────────────────────────────────────┤
  │ ✓ Rust toolchain installed                  │
  │ ✓ Solana CLI available                      │
  │ ✓ Required system libraries                 │
  │ ✓ MicroVM support (KVM/VT-x)               │
  ├─────────────────────────────────────────────┤
  │ User Level                                  │
  ├─────────────────────────────────────────────┤
  │ ✓ Solana config valid                       │
  │ ✓ Keypair accessible                        │
  │ ✓ RPC endpoints reachable                   │
  │ ✓ OSVM directories exist                    │
  │ ✓ MCP server configurations                 │
  └─────────────────────────────────────────────┘

COMMON ISSUES & FIXES:
  • "No keypair found" → osvm uses ~/.config/solana/id.json
  • "RPC unreachable" → Check network or try different endpoint
  • "MicroVM not available" → Requires Linux with KVM support
"#)
}
