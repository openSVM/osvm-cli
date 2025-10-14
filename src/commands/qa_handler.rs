use crate::services::qa_agent_service::{QaAgentService, TestCategory};

/// Handle QA command for automated testing and bug detection
pub async fn handle_qa_command(
    app_matches: &clap::ArgMatches,
    matches: &clap::ArgMatches,
) -> Result<(), Box<dyn std::error::Error>> {
    let debug_mode = app_matches.get_flag("debug");
    let verbose = app_matches.get_count("verbose");

    // Create QA service
    let qa_service = QaAgentService::new(debug_mode, verbose);

    match matches.subcommand() {
        Some(("run", sub_m)) => {
            // Check if specific scenario or all
            let category = if sub_m.get_flag("all") || sub_m.get_one::<String>("scenario").is_some() {
                // If specific scenario or all, run all for now (TODO: filter by scenario name)
                TestCategory::All
            } else {
                // Default to chat tests for testing
                TestCategory::Chat
            };

            let create_issues = sub_m.get_flag("create_issues");
            let github_repo = sub_m
                .get_one::<String>("github_repo")
                .map(|s| s.to_string())
                .unwrap_or_else(|| "opensvm/osvm-cli".to_string());

            println!("🤖 Running QA tests for category: {:?}", category);
            if create_issues {
                println!("📝 GitHub issues will be created for bugs found");
                println!("📦 Repository: {}", github_repo);
            }

            // Run tests
            let results = qa_service.run_tests(category).await?;

            // Report results
            let passed = results.iter().filter(|r| r.passed).count();
            let failed = results.iter().filter(|r| !r.passed).count();
            let bugs_found = results
                .iter()
                .map(|r| r.bugs_found.len())
                .sum::<usize>();

            println!("\n╔════════════════════════════════════════╗");
            println!("║         QA Test Results                ║");
            println!("╠════════════════════════════════════════╣");
            println!("║ ✅ Passed:      {:>3}                    ║", passed);
            println!("║ ❌ Failed:      {:>3}                    ║", failed);
            println!("║ 🐛 Bugs Found:  {:>3}                    ║", bugs_found);
            println!("╚════════════════════════════════════════╝");

            // Create GitHub issues if requested
            if create_issues && bugs_found > 0 {
                println!("\n📝 Creating GitHub issues for bugs...");
                for result in &results {
                    for bug in &result.bugs_found {
                        match qa_service.create_github_issue(bug).await {
                            Ok(url) => println!("✅ Created issue: {}", url),
                            Err(e) => eprintln!("❌ Failed to create issue: {}", e),
                        }
                    }
                }
            }

            // Generate summary report
            qa_service
                .generate_summary_report(&results, category)
                .await?;

            // Exit with error code if any tests failed
            if failed > 0 {
                std::process::exit(1);
            }
        }
        Some(("interactive", _)) => {
            println!("🎮 Starting interactive QA session...");
            qa_service.run_interactive().await?;
        }
        Some(("list", _)) => {
            println!("📋 Available test scenarios:");
            qa_service.list_scenarios();
        }
        Some(("reports", _)) => {
            println!("📊 Recent QA reports:");
            qa_service.show_reports().await?;
        }
        Some(("visual", sub_m)) => {
            use crate::services::tui_test_agent::TuiTestAgent;

            println!("🎨 Running visual TUI tests...");

            // Parse arguments
            let cols = sub_m
                .get_one::<String>("cols")
                .and_then(|s| s.parse::<u16>().ok())
                .unwrap_or(120);
            let rows = sub_m
                .get_one::<String>("rows")
                .and_then(|s| s.parse::<u16>().ok())
                .unwrap_or(30);
            let test_name = sub_m.get_one::<String>("test").map(|s| s.as_str());
            let save_screenshots = sub_m.get_flag("screenshots");
            let save_as_png = sub_m.get_flag("png");
            let _headless = sub_m.get_flag("headless");

            println!("🖥️  Terminal size: {}x{}", cols, rows);
            if save_as_png && save_screenshots {
                println!("📸 PNG screenshot mode enabled");
            }
            if let Some(name) = test_name {
                println!("🔍 Running test: {}", name);
            } else {
                println!("🔍 Running all visual tests");
            }

            // Create TUI test agent
            let agent = TuiTestAgent::new(cols, rows)?;

            // Launch advanced chat
            println!("🚀 Launching advanced chat TUI...");
            agent.launch_advanced_chat().await?;

            // Wait for UI to initialize
            tokio::time::sleep(std::time::Duration::from_secs(2)).await;

            // Get test scenarios
            let scenarios = crate::services::tui_test_agent::create_default_tui_scenarios();

            // Filter scenarios if specific test requested
            let scenarios_to_run: Vec<_> = if let Some(name) = test_name {
                scenarios
                    .into_iter()
                    .filter(|s| s.name.contains(name))
                    .collect()
            } else {
                scenarios
            };

            if scenarios_to_run.is_empty() {
                eprintln!("❌ No matching test scenarios found");
                agent.stop().await?;
                std::process::exit(1);
            }

            let mut all_passed = true;
            let mut total_tests = 0;
            let mut passed_tests = 0;

            // Run scenarios
            for scenario in &scenarios_to_run {
                total_tests += 1;
                println!("\n🧪 Running: {}", scenario.name);
                println!("   Description: {}", scenario.description);

                let result = agent.run_scenario(scenario, save_as_png && save_screenshots).await?;

                if result.passed {
                    passed_tests += 1;
                    println!("   ✅ PASSED in {:.2}s", result.duration_seconds);
                } else {
                    all_passed = false;
                    println!("   ❌ FAILED in {:.2}s", result.duration_seconds);
                    for error in &result.errors {
                        println!("      - {}", error);
                    }
                }

                if save_screenshots && !result.screenshots.is_empty() {
                    println!("   📸 Screenshots saved: {:?}", result.screenshots);
                }
            }

            // Stop the agent
            agent.stop().await?;

            // Print summary
            println!("\n╔════════════════════════════════════════╗");
            println!("║     Visual TUI Test Results            ║");
            println!("╠════════════════════════════════════════╣");
            println!("║ ✅ Passed:      {:>3}/{:<3}                ║", passed_tests, total_tests);
            println!("║ ❌ Failed:      {:>3}/{:<3}                ║", total_tests - passed_tests, total_tests);
            println!("╚════════════════════════════════════════╝");

            if save_screenshots {
                println!("\n📸 Screenshots saved to: ~/.osvm/qa/screenshots/");
            }

            // Exit with error code if any tests failed
            if !all_passed {
                std::process::exit(1);
            }
        }
        _ => {
            eprintln!("❌ Unknown QA subcommand. Use 'osvm qa --help' for usage.");
            std::process::exit(1);
        }
    }

    Ok(())
}
