use crate::services::mcp_service::McpService;
use crate::services::ovsm_service::OvsmService;
use crate::utils::mcp_bridge::McpBridgeTool;
use std::sync::Arc;

/// Handle OVSM command for script execution and management
pub async fn handle_ovsm_command(
    matches: &clap::ArgMatches,
) -> Result<(), Box<dyn std::error::Error>> {
    use crate::services::ovsm_service::OvsmService;

    match matches.subcommand() {
        Some(("run", run_matches)) => {
            let script = run_matches.get_one::<String>("script").expect("required");
            let verbose = run_matches.get_count("verbose") > 0;
            let debug = run_matches.get_flag("debug");
            let json = run_matches.get_flag("json");
            // Always enable RPC tools for ovsm run
            use crate::utils::rpc_bridge::create_rpc_registry;

            let mut registry = create_rpc_registry();

            // Dynamically register MCP tools from configured servers
            let mut mcp_service = McpService::new_with_debug(debug);
            let _ = mcp_service.load_config();
            let mcp_arc = Arc::new(tokio::sync::Mutex::new(mcp_service));

            // Discover and register tools from all configured MCP servers
            {
                let mut svc = mcp_arc.lock().await;
                let mut total_tools = 0;

                // Get all configured servers
                let servers: Vec<String> = svc
                    .list_servers()
                    .iter()
                    .map(|(id, _)| (*id).clone())
                    .collect();

                for server_id in servers {
                    // Initialize server
                    if let Err(e) = svc.initialize_server(&server_id).await {
                        if debug {
                            eprintln!("⚠️  Failed to initialize MCP server '{}': {}", server_id, e);
                        }
                        continue;
                    }

                    // List tools from this server
                    match svc.list_tools(&server_id).await {
                        Ok(tools) => {
                            if debug {
                                println!(
                                    "📦 Discovered {} tools from MCP server '{}'",
                                    tools.len(),
                                    server_id
                                );
                            }
                            total_tools += tools.len();

                            // Register each tool
                            drop(svc); // Release lock before registering
                            for tool in tools {
                                registry
                                    .register(McpBridgeTool::new(&tool.name, Arc::clone(&mcp_arc)));
                            }
                            svc = mcp_arc.lock().await; // Re-acquire lock
                        }
                        Err(e) => {
                            if debug {
                                eprintln!("⚠️  Failed to list tools from '{}': {}", server_id, e);
                            }
                        }
                    }
                }

                if debug && total_tools > 0 {
                    println!("✅ Registered {} MCP tools total\n", total_tools);
                }
            }
            let mut service = OvsmService::with_registry(registry, verbose, debug);

            println!("🚀 Executing OVSM script: {}", script);

            match service.execute_file(script) {
                Ok(result) => {
                    if json {
                        println!("{}", service.format_value_json(&result)?);
                    } else {
                        println!("✨ Result: {}", service.format_value(&result));
                    }
                }
                Err(e) => {
                    // Use enhanced error message if it's an OVSM error with available fields
                    let error_msg = if let Some(ovsm_err) = e.downcast_ref::<ovsm::error::Error>() {
                        ovsm_err.enhanced_message()
                    } else {
                        e.to_string()
                    };
                    eprintln!("❌ Execution failed: {}", error_msg);
                    std::process::exit(1);
                }
            }
        }
        Some(("repl", _repl_matches)) => {
            println!("🎯 OVSM Interactive REPL");
            println!("Type 'exit' or 'quit' to exit, 'help' for help\n");

            let mut service = OvsmService::with_verbose(false);
            let stdin = std::io::stdin();

            loop {
                print!("ovsm> ");
                std::io::Write::flush(&mut std::io::stdout())?;

                let mut input = String::new();
                stdin.read_line(&mut input)?;

                let input = input.trim();

                if input.is_empty() {
                    continue;
                }

                if matches!(input, "exit" | "quit") {
                    println!("👋 Goodbye!");
                    break;
                }

                if input == "help" {
                    println!("OVSM REPL Commands:");
                    println!("  exit, quit  - Exit the REPL");
                    println!("  help        - Show this help message");
                    println!("\nOVSM Language Features:");
                    println!("  Variables:  $var = value");
                    println!("  Control:    IF/THEN/ELSE, FOR, WHILE, BREAK, CONTINUE");
                    println!("  Data Types: Int, Float, String, Bool, Arrays, Objects");
                    println!("  Return:     RETURN value");
                    continue;
                }

                match service.execute_code(input) {
                    Ok(result) => {
                        println!("=> {}", service.format_value(&result));
                    }
                    Err(e) => {
                        eprintln!("❌ Error: {}", e);
                    }
                }
            }
        }
        Some(("eval", eval_matches)) => {
            let code = eval_matches.get_one::<String>("code").expect("required");
            let json = eval_matches.get_flag("json");

            let mut service = OvsmService::new();

            match service.execute_code(code) {
                Ok(result) => {
                    if json {
                        println!("{}", service.format_value_json(&result)?);
                    } else {
                        println!("{}", service.format_value(&result));
                    }
                }
                Err(e) => {
                    eprintln!("❌ Error: {}", e);
                    std::process::exit(1);
                }
            }
        }
        Some(("check", check_matches)) => {
            let script = check_matches.get_one::<String>("script").expect("required");

            let service = OvsmService::with_verbose(true);

            println!("🔍 Checking syntax: {}", script);

            match service.check_file_syntax(script) {
                Ok(_) => {
                    println!("✅ Syntax check passed!");
                }
                Err(e) => {
                    eprintln!("❌ Syntax error: {}", e);
                    std::process::exit(1);
                }
            }
        }
        Some(("examples", examples_matches)) => {
            let category = examples_matches.get_one::<String>("category");
            let list = examples_matches.get_flag("list");
            let show = examples_matches.get_one::<String>("show");

            if list {
                println!("📚 OVSM Example Categories:");
                println!("  basics      - Basic language features");
                println!("  blockchain  - Blockchain operations");
                println!("  automation  - Automation scripts");
                println!("  mcp         - MCP tool integration");
                println!("  advanced    - Advanced techniques");
                println!("\nUse: osvm ovsm examples --category <name> to see examples");
                return Ok(());
            }

            if let Some(name) = show {
                println!("📄 Example: {}", name);
                println!("(Example scripts will be added in examples/ovsm_scripts/)");
                return Ok(());
            }

            if let Some(cat) = category {
                println!("📚 OVSM Examples - Category: {}", cat);
                match cat.as_str() {
                    "basics" => {
                        println!("\n## Basic Variables and Arithmetic");
                        println!("```ovsm");
                        println!("$x = 10");
                        println!("$y = 20");
                        println!("$sum = $x + $y");
                        println!("RETURN $sum");
                        println!("```");
                    }
                    "blockchain" => {
                        println!("\n## Get Balance (coming soon)");
                        println!("```ovsm");
                        println!("// Get SOL balance from blockchain");
                        println!("$address = \"4Nd1mBQtrMJVYVfKf2PJy9NZUZdTAsp7D4xWLs4gDB4T\"");
                        println!("$balance = GET_BALANCE($address)");
                        println!("RETURN $balance");
                        println!("```");
                    }
                    _ => {
                        println!("Examples for category '{}' coming soon!", cat);
                    }
                }
            } else {
                println!("📚 OVSM Examples\n");
                println!("Use --list to see categories");
                println!("Use --category <name> to see examples in a category");
                println!("Use --show <name> to display a specific example");
            }
        }
        Some(("generate", gen_matches)) => {
            let description = gen_matches
                .get_one::<String>("description")
                .expect("required");
            let _output = gen_matches.get_one::<String>("output");
            let _interactive = gen_matches.get_flag("interactive");

            println!("🤖 Generating OVSM script from description:");
            println!("   {}", description);
            println!("\n⚠️  AI script generation coming soon!");
            println!("This feature will use the AI service to generate OVSM scripts.");
        }
        Some(("library", lib_matches)) => match lib_matches.subcommand() {
            Some(("list", _)) => {
                println!("📚 OVSM Script Library");
                println!("(Library management coming soon)");
            }
            Some(("install", _)) => {
                println!("📥 Installing script...");
                println!("(Library management coming soon)");
            }
            Some(("remove", _)) => {
                println!("🗑️  Removing script...");
                println!("(Library management coming soon)");
            }
            Some(("run", _)) => {
                println!("🚀 Running library script...");
                println!("(Library management coming soon)");
            }
            Some(("update", _)) => {
                println!("🔄 Updating scripts...");
                println!("(Library management coming soon)");
            }
            _ => {
                eprintln!("❌ Unknown library subcommand");
                std::process::exit(1);
            }
        },
        _ => {
            eprintln!("❌ Unknown ovsm subcommand");
            eprintln!("   Run 'osvm ovsm --help' for usage");
            std::process::exit(1);
        }
    }

    Ok(())
}
