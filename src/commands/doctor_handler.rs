use crate::utils::diagnostics::DiagnosticCoordinator;
use std::process::exit;

pub async fn handle_doctor_command(
    matches: &clap::ArgMatches,
) -> Result<(), Box<dyn std::error::Error>> {
    // Handle the doctor command for system diagnostics and repair
    let diagnostic_coordinator = DiagnosticCoordinator::new();

    if matches.contains_id("fix") {
        // Run diagnostics and attempt repairs
        println!("🩺 OSVM System Health Check & Repair");
        println!("===================================");

        match diagnostic_coordinator.run_detailed_diagnostics().await {
            Ok(results) => {
                // Display current status
                println!("📊 System Status: {:?}", results.summary.overall_health);
                println!(
                    "🔍 Checks: {}/{} passed",
                    results.summary.passed_checks, results.summary.total_checks
                );

                if results.summary.critical_issues > 0 || results.summary.warnings > 0 {
                    println!("\n🛠️  Issues detected - attempting automatic repair...");

                    // Extract repairable errors from health check
                    let health = &results.system_health;
                    let mut repairable_errors = Vec::new();

                    // Convert health issues to repairable errors
                    for issue in &health.issues {
                        match issue.category {
                            crate::utils::diagnostics::IssueCategory::SystemDependencies => {
                                if issue.title.contains("System tuning") {
                                    repairable_errors.push(crate::utils::self_repair::RepairableError::SystemTuningRequired);
                                } else if issue.title.contains("Missing dependency") {
                                    let dep_name = issue.title.replace("Missing dependency: ", "");
                                    repairable_errors.push(crate::utils::self_repair::RepairableError::MissingSystemDependencies(vec![dep_name]));
                                } else if issue.title.contains("Update available") {
                                    repairable_errors.push(crate::utils::self_repair::RepairableError::OutdatedSystemPackages);
                                }
                            }
                            crate::utils::diagnostics::IssueCategory::UserConfiguration => {
                                if issue.title.contains("Solana CLI not installed") {
                                    repairable_errors.push(crate::utils::self_repair::RepairableError::MissingSolanaCli);
                                } else if issue.title.contains("config directory missing") {
                                    repairable_errors.push(crate::utils::self_repair::RepairableError::MissingConfigDirectory);
                                } else if issue.title.contains("keypair missing") {
                                    // Extract keypair path from CLI or config
                                    let cli_config = solana_cli_config::Config::load(
                                        "~/.config/osvm/config.yml",
                                    )
                                    .unwrap_or_default();
                                    let default_keypair_path = matches
                                        .get_one::<String>("keypair")
                                        .map(|s| s.to_string())
                                        .unwrap_or_else(|| cli_config.keypair_path.clone());
                                    repairable_errors.push(
                                        crate::utils::self_repair::RepairableError::MissingKeypair(
                                            default_keypair_path,
                                        ),
                                    );
                                }
                            }
                            _ => {}
                        }
                    }

                    if !repairable_errors.is_empty() {
                        let repair_system =
                            crate::utils::self_repair::SelfRepairSystem::with_default_config();
                        match repair_system.repair_automatically(repairable_errors).await {
                            Ok(crate::utils::self_repair::RepairResult::Success(msg)) => {
                                println!("✅ {}", msg);
                            }
                            Ok(result) => {
                                println!("⚠️  Repair result: {:?}", result);
                            }
                            Err(e) => {
                                println!("❌ Repair failed: {}", e);
                            }
                        }
                    } else {
                        println!("ℹ️  No automatically repairable issues found");
                    }
                } else {
                    println!("🎉 All systems healthy! No repairs needed.");
                }
            }
            Err(e) => {
                eprintln!("Error running diagnostics: {}", e);
                exit(1);
            }
        }
    } else {
        // Just run diagnostics without repair
        let check_all = matches.contains_id("check_all");
        let system_only = matches.contains_id("system_only");
        let user_only = matches.contains_id("user_only");
        let verbose = matches.get_count("verbose") > 0;

        if check_all || (!system_only && !user_only) {
            println!("🩺 OSVM Comprehensive System Health Check");
            println!("==========================================");

            match diagnostic_coordinator.run_detailed_diagnostics().await {
                Ok(results) => {
                    // Display summary
                    println!("\n📊 SUMMARY");
                    println!("├── Overall Health: {:?}", results.summary.overall_health);
                    println!("├── Total Checks: {}", results.summary.total_checks);
                    println!("├── Passed: {}", results.summary.passed_checks);
                    println!("├── Failed: {}", results.summary.failed_checks);
                    println!("├── Critical Issues: {}", results.summary.critical_issues);
                    println!("└── Warnings: {}", results.summary.warnings);

                    // Display detailed results if verbose
                    if verbose {
                        println!("\n🔍 DETAILED RESULTS");
                        for (name, check) in &results.detailed_checks {
                            let status = if check.passed { "✅" } else { "❌" };
                            println!("  {} {}: {}", status, name, check.message);
                            if let Some(details) = &check.details {
                                println!("     └── {}", details);
                            }
                        }
                    }

                    // Display issues and recommendations
                    let health = &results.system_health;
                    if !health.issues.is_empty() {
                        println!("\n⚠️  ISSUES FOUND:");
                        for issue in &health.issues {
                            let severity_icon = match issue.severity {
                                crate::utils::diagnostics::IssueSeverity::Critical => "🔴",
                                crate::utils::diagnostics::IssueSeverity::Error => "🟠",
                                crate::utils::diagnostics::IssueSeverity::Warning => "🟡",
                                crate::utils::diagnostics::IssueSeverity::Info => "🔵",
                            };
                            println!("  {} {}: {}", severity_icon, issue.title, issue.description);
                            if let Some(fix) = &issue.suggested_fix {
                                println!("     💡 Suggested fix: {}", fix);
                            }
                        }
                    }

                    if !health.recommendations.is_empty() {
                        println!("\n💡 RECOMMENDATIONS:");
                        for rec in &health.recommendations {
                            println!("  • {}", rec);
                        }
                    }

                    if health.issues.is_empty() {
                        println!("\n🎉 All systems healthy!");
                    } else {
                        println!("\nℹ️  Use 'osvm doctor --fix' to attempt automatic repairs");
                    }
                }
                Err(e) => {
                    eprintln!("Error running diagnostics: {}", e);
                    exit(1);
                }
            }
        } else {
            println!("🩺 OSVM Targeted Health Check");
            println!("=============================");

            match diagnostic_coordinator.check_system_health().await {
                Ok(health) => {
                    if system_only {
                        println!("\n🖥️  SYSTEM DEPENDENCIES:");
                        for dep in &health.system_dependencies {
                            let status = if dep.installed { "✅" } else { "❌" };
                            let update_info = if dep.update_available {
                                " (update available)"
                            } else {
                                ""
                            };
                            println!(
                                "  {} {}: {}{}",
                                status,
                                dep.name,
                                dep.version.as_deref().unwrap_or("not installed"),
                                update_info
                            );
                        }
                    }

                    if user_only {
                        println!("\n👤 USER CONFIGURATION:");
                        let config = &health.user_configuration;
                        println!(
                            "  {} Solana CLI: {}",
                            if config.cli_installed { "✅" } else { "❌" },
                            if config.cli_installed {
                                config.cli_version.as_deref().unwrap_or("unknown version")
                            } else {
                                "not installed"
                            }
                        );
                        println!(
                            "  {} Config directory: {}",
                            if config.config_dir_exists {
                                "✅"
                            } else {
                                "❌"
                            },
                            if config.config_dir_exists {
                                "exists"
                            } else {
                                "missing"
                            }
                        );
                        println!(
                            "  {} Keypair: {}",
                            if config.keypair_exists { "✅" } else { "❌" },
                            if config.keypair_exists {
                                config.keypair_path.as_deref().unwrap_or("unknown path")
                            } else {
                                "missing"
                            }
                        );
                        println!(
                            "  {} Network: {}",
                            if config.current_network.is_some() {
                                "✅"
                            } else {
                                "❌"
                            },
                            config
                                .current_network
                                .as_deref()
                                .unwrap_or("not configured")
                        );
                    }
                }
                Err(e) => {
                    eprintln!("Error checking system health: {}", e);
                    exit(1);
                }
            }
        }
    }

    Ok(())
}
