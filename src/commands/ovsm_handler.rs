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
        Some(("compile", compile_matches)) => {
            let script = compile_matches.get_one::<String>("script").expect("required");
            let output = compile_matches.get_one::<String>("output");
            let opt_level = *compile_matches.get_one::<u8>("opt-level").unwrap_or(&2);
            let verify = compile_matches.get_flag("verify");
            let emit_ir = compile_matches.get_flag("emit-ir");
            let analyze = compile_matches.get_flag("analyze");

            use ovsm::compiler::{Compiler, CompileOptions};

            println!("🔧 Compiling OVSM to sBPF: {}", script);

            // Read source
            let source = std::fs::read_to_string(script)?;

            // Run register analysis if requested
            if analyze {
                use ovsm::{SExprScanner, SExprParser};
                use ovsm::compiler::{TypeChecker, IrGenerator, RegAllocAnalyzer};

                println!("\n📊 Running Register Pressure Analysis...\n");

                // Parse
                let mut scanner = SExprScanner::new(&source);
                let tokens = scanner.scan_tokens()?;
                let mut parser = SExprParser::new(tokens);
                let program = parser.parse()?;

                // Type check
                let mut type_checker = TypeChecker::new();
                let typed_program = type_checker.check(&program)?;

                // Generate IR
                let mut ir_gen = IrGenerator::new();
                let ir_program = ir_gen.generate(&typed_program)?;

                // Analyze
                let analyzer = RegAllocAnalyzer::new();
                let report = analyzer.analyze(&ir_program);

                println!("{}", report.format());

                // Exit if critical issues found
                let critical_count = report.issues.iter()
                    .filter(|i| i.severity == "critical")
                    .count();

                if critical_count > 0 {
                    eprintln!("\n🚨 {} critical register allocation issue(s) detected!", critical_count);
                    eprintln!("   These may cause runtime failures. Review before deploying.\n");
                }
            }

            // Compile
            let options = CompileOptions {
                opt_level,
                compute_budget: 200_000,
                debug_info: emit_ir,
                source_map: false,
                sbpf_version: ovsm::compiler::SbpfVersion::V1,  // V1 with relocations for comparison
                enable_solana_abi: false,  // Disabled until opcode issues are fixed
            };

            let compiler = Compiler::new(options);
            match compiler.compile(&source) {
                Ok(result) => {
                    // Determine output path
                    let out_path = output.cloned().unwrap_or_else(|| {
                        let p = std::path::Path::new(script);
                        p.with_extension("so").to_string_lossy().to_string()
                    });

                    // Write ELF
                    std::fs::write(&out_path, &result.elf_bytes)?;

                    println!("✅ Compiled successfully!");
                    println!("   Output: {}", out_path);
                    println!("   Size: {} bytes", result.elf_bytes.len());
                    println!("   IR instructions: {}", result.ir_instruction_count);
                    println!("   sBPF instructions: {}", result.sbpf_instruction_count);
                    println!("   Estimated CU: {}", result.estimated_cu);

                    if !result.warnings.is_empty() {
                        println!("\n⚠️  Warnings:");
                        for w in &result.warnings {
                            println!("   - {}", w);
                        }
                    }

                    if verify {
                        if let Some(ref v) = result.verification {
                            println!("\n📋 Verification:");
                            println!("   Valid: {}", v.valid);
                            if !v.errors.is_empty() {
                                for e in &v.errors {
                                    println!("   ❌ {}", e);
                                }
                            }
                        }
                    }
                }
                Err(e) => {
                    eprintln!("❌ Compilation failed: {}", e);
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
        Some(("fmt", fmt_matches)) => {
            let script = fmt_matches.get_one::<String>("script").expect("required");
            let write = fmt_matches.get_flag("write");
            let check = fmt_matches.get_flag("check");
            let indent_size = *fmt_matches.get_one::<u8>("indent").unwrap_or(&2) as usize;

            // Read the script
            let source = std::fs::read_to_string(script)?;

            // Format the source
            let formatted = format_ovsm_script(&source, indent_size);

            if check {
                // Check mode: exit 1 if not formatted
                if source == formatted {
                    println!("✅ {} is correctly formatted", script);
                } else {
                    eprintln!("❌ {} needs formatting", script);
                    std::process::exit(1);
                }
            } else if write {
                // Write back to file
                if source == formatted {
                    println!("✅ {} is already formatted", script);
                } else {
                    std::fs::write(script, &formatted)?;
                    println!("✅ Formatted {}", script);
                }
            } else {
                // Print to stdout
                print!("{}", formatted);
            }
        }
        Some(("lint", lint_matches)) => {
            let script = lint_matches.get_one::<String>("script").expect("required");
            let verbose = lint_matches.get_flag("verbose");
            let fix = lint_matches.get_flag("fix");

            println!("🔍 Linting OVSM script: {}", script);

            // Read the script
            let source = std::fs::read_to_string(script)?;

            // Run structural analysis
            let report = lint_ovsm_script(&source, verbose);

            // Display results
            println!("\n{}", report.summary);

            if !report.instruction_boundaries.is_empty() {
                println!("\n📋 Instruction Boundaries (discriminator checks):");
                for boundary in &report.instruction_boundaries {
                    let status = if boundary.depth_at_start == boundary.expected_start_depth {
                        "✅"
                    } else {
                        "⚠️ "
                    };
                    println!(
                        "  {} Line {}: {} (depth: {} → {})",
                        status,
                        boundary.line,
                        boundary.instruction_name,
                        boundary.depth_at_start,
                        boundary.depth_at_end
                    );
                }
            }

            if !report.issues.is_empty() {
                println!("\n🚨 Issues Found:");
                for issue in &report.issues {
                    println!("  {} Line {}: {}", issue.severity, issue.line, issue.message);
                    if let Some(suggestion) = &issue.suggestion {
                        println!("     💡 Suggestion: {}", suggestion);
                    }
                }
            }

            if report.issues.is_empty() {
                println!("\n✅ No structural issues found!");
            } else if fix {
                // Apply auto-fix if requested
                if let Some(fixed_source) = attempt_auto_fix(&source, &report) {
                    let backup_path = format!("{}.bak", script);
                    std::fs::write(&backup_path, &source)?;
                    std::fs::write(script, &fixed_source)?;
                    println!("\n🔧 Auto-fix applied! Backup saved to: {}", backup_path);
                } else {
                    println!("\n⚠️  Could not auto-fix issues. Manual review required.");
                }
            }

            // Final paren balance check
            println!("\n📊 Overall Statistics:");
            println!("   Total lines: {}", report.total_lines);
            println!("   Max depth: {}", report.max_depth);
            println!("   Final balance: {}", report.final_balance);
            if report.final_balance != 0 {
                println!("   ⚠️  {} unclosed parentheses", report.final_balance.abs());
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
        Some(("idl", idl_matches)) => {
            let script = idl_matches.get_one::<String>("script").expect("required");
            let output = idl_matches.get_one::<String>("output");
            let name_override = idl_matches.get_one::<String>("name");
            let address = idl_matches.get_one::<String>("address");
            let compact = idl_matches.get_flag("compact");

            use ovsm::compiler::anchor_idl::IdlGenerator;

            println!("📋 Generating Anchor IDL from: {}", script);

            let source = std::fs::read_to_string(script)?;
            let mut generator = IdlGenerator::new(&source);

            if let Some(name) = name_override {
                generator = generator.with_name(name);
            }

            match generator.generate() {
                Ok(mut idl) => {
                    if let Some(addr) = address {
                        idl.metadata = Some(ovsm::compiler::anchor_idl::IdlMetadata {
                            address: Some(addr.clone()),
                        });
                    }

                    let json_result = if compact {
                        serde_json::to_string(&idl)
                    } else {
                        serde_json::to_string_pretty(&idl)
                    };

                    match json_result {
                        Ok(json) => {
                            if let Some(out_path) = output {
                                std::fs::write(out_path, &json)?;
                                println!("✅ IDL written to: {}", out_path);
                            } else {
                                println!("{}", json);
                            }

                            println!("\n📊 IDL Summary:");
                            println!("   Program: {}", idl.name);
                            println!("   Version: {}", idl.version);
                            println!("   Instructions: {}", idl.instructions.len());
                            println!("   Accounts: {}", idl.accounts.len());
                            println!("   Types: {}", idl.types.len());
                            println!("   Errors: {}", idl.errors.len());

                            if !idl.instructions.is_empty() {
                                println!("\n📝 Instructions:");
                                for instr in &idl.instructions {
                                    let disc = instr.discriminator.as_ref()
                                        .map(|d| d.iter().map(|b| b.to_string()).collect::<Vec<_>>().join(","))
                                        .unwrap_or_default();
                                    println!("   [{}] {} ({} accounts, {} args)",
                                        disc, instr.name, instr.accounts.len(), instr.args.len());
                                }
                            }
                        }
                        Err(e) => {
                            eprintln!("❌ JSON serialization failed: {}", e);
                            std::process::exit(1);
                        }
                    }
                }
                Err(e) => {
                    eprintln!("❌ IDL generation failed: {}", e);
                    std::process::exit(1);
                }
            }
        }
        Some(("validate-instruction", val_matches)) => {
            let idl_path = val_matches.get_one::<String>("idl").expect("required");
            let data_parts: Vec<&String> = val_matches.get_many::<String>("data")
                .expect("required")
                .collect();
            let verbose = val_matches.get_flag("verbose");

            use ovsm::compiler::anchor_idl::{AnchorIdl, IdlType};

            println!("🔍 Validating instruction data against IDL: {}", idl_path);

            let idl_content = std::fs::read_to_string(idl_path)?;
            let idl: AnchorIdl = serde_json::from_str(&idl_content).map_err(|e| {
                std::io::Error::new(std::io::ErrorKind::InvalidData, format!("Invalid IDL JSON: {}", e))
            })?;

            // Parse hex instruction data
            let combined_hex: String = data_parts.iter()
                .map(|s| s.trim_start_matches("0x").trim_start_matches("0X"))
                .collect::<Vec<_>>()
                .join("");

            let instruction_data: Vec<u8> = (0..combined_hex.len())
                .step_by(2)
                .filter_map(|i| {
                    if i + 2 <= combined_hex.len() {
                        u8::from_str_radix(&combined_hex[i..i + 2], 16).ok()
                    } else {
                        None
                    }
                })
                .collect();

            if instruction_data.is_empty() {
                eprintln!("❌ No valid hex data provided");
                std::process::exit(1);
            }

            if verbose {
                println!("\n📊 Raw Data:");
                println!("   Length: {} bytes", instruction_data.len());
                println!("   Hex: {}", instruction_data.iter()
                    .map(|b| format!("{:02x}", b)).collect::<Vec<_>>().join(" "));
            }

            // Find matching instruction by discriminator
            let discriminator_byte = instruction_data[0];
            let matching = idl.instructions.iter().find(|instr| {
                instr.discriminator.as_ref()
                    .map(|d| !d.is_empty() && d[0] == discriminator_byte)
                    .unwrap_or(false)
            });

            match matching {
                Some(instr) => {
                    println!("\n✅ Matched Instruction: {}", instr.name);
                    let disc_str = instr.discriminator.as_ref()
                        .map(|d| d.iter().map(|b| format!("{:02x}", b)).collect::<Vec<_>>().join(" "))
                        .unwrap_or_default();
                    println!("   Discriminator: [{}]", disc_str);

                    if !instr.accounts.is_empty() {
                        println!("\n📋 Required Accounts:");
                        for (i, acc) in instr.accounts.iter().enumerate() {
                            let mut flags = Vec::new();
                            if acc.is_signer { flags.push("signer"); }
                            if acc.is_mut { flags.push("writable"); }
                            let flag_str = if flags.is_empty() { String::new() }
                                else { format!(" [{}]", flags.join(", ")) };
                            println!("   {}. {}{}", i, acc.name, flag_str);
                        }
                    }

                    if !instr.args.is_empty() {
                        println!("\n🔢 Arguments:");
                        let mut offset = 1; // Skip discriminator
                        for arg in &instr.args {
                            let (value_str, bytes) = decode_idl_type(&arg.ty, &instruction_data, offset, verbose);
                            let type_name = format_idl_type(&arg.ty);
                            println!("   {} ({}): {}", arg.name, type_name, value_str);
                            if verbose && bytes > 0 {
                                let end = (offset + bytes).min(instruction_data.len());
                                let hex: String = instruction_data[offset..end].iter()
                                    .map(|b| format!("{:02x}", b)).collect::<Vec<_>>().join(" ");
                                println!("      Bytes [{}..{}]: {}", offset, end, hex);
                            }
                            offset += bytes;
                        }

                        if offset < instruction_data.len() {
                            println!("\n⚠️  {} extra bytes after arguments", instruction_data.len() - offset);
                        } else if offset > instruction_data.len() {
                            println!("\n❌ Insufficient data: expected {} bytes, got {}", offset, instruction_data.len());
                        }
                    } else if instruction_data.len() > 1 {
                        println!("\n⚠️  No arguments defined but {} extra bytes present", instruction_data.len() - 1);
                    } else {
                        println!("\n   (no arguments)");
                    }
                }
                None => {
                    eprintln!("\n❌ No instruction found with discriminator: 0x{:02x}", discriminator_byte);
                    eprintln!("\n📝 Available discriminators:");
                    for instr in &idl.instructions {
                        if let Some(disc) = &instr.discriminator {
                            let disc_str = disc.iter().map(|b| format!("{:02x}", b)).collect::<Vec<_>>().join(" ");
                            eprintln!("   [{}] = {}", disc_str, instr.name);
                        }
                    }
                    std::process::exit(1);
                }
            }
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

// ============================================================================
// OVSM Lint Analysis
// ============================================================================

/// Report from linting an OVSM script
#[derive(Debug)]
struct LintReport {
    summary: String,
    instruction_boundaries: Vec<InstructionBoundary>,
    issues: Vec<LintIssue>,
    total_lines: usize,
    max_depth: i32,
    final_balance: i32,
}

/// Information about an instruction boundary (discriminator check)
#[derive(Debug)]
struct InstructionBoundary {
    line: usize,
    instruction_name: String,
    depth_at_start: i32,
    depth_at_end: i32,
    expected_start_depth: i32,
}

/// A lint issue found in the script
#[derive(Debug)]
struct LintIssue {
    line: usize,
    message: String,
    suggestion: Option<String>,
    severity: LintSeverity,
}

/// Severity level for lint issues
#[derive(Debug, Clone, Copy, PartialEq)]
enum LintSeverity {
    Error,
    Warning,
    Info,
}

impl std::fmt::Display for LintSeverity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LintSeverity::Error => write!(f, "❌"),
            LintSeverity::Warning => write!(f, "⚠️ "),
            LintSeverity::Info => write!(f, "ℹ️ "),
        }
    }
}

/// Track variable definitions and usage
#[derive(Debug, Clone)]
struct VariableInfo {
    name: String,
    defined_at: usize,
    used_at: Vec<usize>,
    scope_depth: i32,
}

/// Analyze OVSM script for structural issues
fn lint_ovsm_script(source: &str, verbose: bool) -> LintReport {
    let lines: Vec<&str> = source.lines().collect();
    let mut depth: i32 = 0;
    let mut max_depth: i32 = 0;
    let mut instruction_boundaries = Vec::new();
    let mut issues = Vec::new();

    // Track the depth at each line for verbose output
    let mut line_depths: Vec<(usize, i32, i32)> = Vec::new(); // (line_num, start_depth, end_depth)

    // Pattern to detect instruction boundary: (if (= discriminator N)
    let discriminator_pattern = regex::Regex::new(
        r"\(if\s+\(=\s+discriminator\s+(\d+)\)"
    ).unwrap();

    // Pattern to detect closing of instruction block: null))
    let null_close_pattern = regex::Regex::new(r"null\)*\s*$").unwrap();

    // Patterns for additional checks
    let define_pattern = regex::Regex::new(r"\(define\s+([a-zA-Z_][a-zA-Z0-9_-]*)\s").unwrap();
    let set_pattern = regex::Regex::new(r"\(set!\s+([a-zA-Z_][a-zA-Z0-9_-]*)\s").unwrap();
    let identifier_pattern = regex::Regex::new(r"\b([a-zA-Z_][a-zA-Z0-9_-]*)\b").unwrap();

    // Track variables for unused/shadowing detection
    let mut variables: std::collections::HashMap<String, VariableInfo> = std::collections::HashMap::new();
    let mut scope_stack: Vec<(i32, Vec<String>)> = Vec::new(); // (depth, vars defined at this depth)

    // Auto-detect expected base depth from first instruction
    let mut expected_base_depth: Option<i32> = None;

    // First pass: find first instruction to establish base depth
    let mut temp_depth: i32 = 0;
    for line in lines.iter() {
        let (opens, closes) = count_parens_smart(line);

        if discriminator_pattern.is_match(line) {
            expected_base_depth = Some(temp_depth);
            break;
        }
        temp_depth += opens - closes;
    }

    let base_depth = expected_base_depth.unwrap_or(1);

    // Track if we just saw a return/abort (for unreachable code detection)
    let mut after_return = false;
    let mut return_depth: i32 = 0;

    // Reserved/built-in identifiers that shouldn't be shadowed
    let builtins: std::collections::HashSet<&str> = [
        "define", "set!", "if", "do", "let", "for", "while", "break", "continue",
        "true", "false", "null", "and", "or", "not", "range", "length", "get",
        "sol_log_", "sol_log_64_", "mem-load", "mem-store", "mem-load1", "mem-store1",
        "account-data-ptr", "account-is-signer", "instruction-data-ptr",
    ].iter().cloned().collect();

    for (line_idx, line) in lines.iter().enumerate() {
        let line_num = line_idx + 1;
        let start_depth = depth;

        // Skip comments and empty lines for some checks
        let trimmed = line.trim();
        let is_comment = trimmed.starts_with(";;");

        // Count parens on this line (ignoring those in strings and comments)
        let (opens, closes) = count_parens_smart(line);
        depth += opens - closes;

        if depth > max_depth {
            max_depth = depth;
        }

        let end_depth = depth;
        line_depths.push((line_num, start_depth, end_depth));

        // Track scope changes
        if end_depth > start_depth {
            scope_stack.push((end_depth, Vec::new()));
        } else if end_depth < start_depth {
            // Pop scopes and check for unused variables
            while let Some((scope_depth, vars)) = scope_stack.last() {
                if *scope_depth > end_depth {
                    for var_name in vars {
                        if let Some(info) = variables.get(var_name) {
                            if info.used_at.is_empty() && !var_name.starts_with('_') {
                                issues.push(LintIssue {
                                    line: info.defined_at,
                                    message: format!("Unused variable '{}'", var_name),
                                    suggestion: Some(format!(
                                        "Remove or use this variable, or prefix with '_' to suppress warning"
                                    )),
                                    severity: LintSeverity::Warning,
                                });
                            }
                        }
                        variables.remove(var_name);
                    }
                    scope_stack.pop();
                } else {
                    break;
                }
            }
        }

        // Check for unreachable code after return
        if after_return && !is_comment && !trimmed.is_empty() && end_depth >= return_depth {
            // Only flag if we're still at the same or deeper nesting level
            if !trimmed.starts_with(')') {
                issues.push(LintIssue {
                    line: line_num,
                    message: "Unreachable code after return/abort".to_string(),
                    suggestion: Some("Remove this code or restructure the control flow".to_string()),
                    severity: LintSeverity::Warning,
                });
            }
        }

        // Detect return/abort patterns
        if !is_comment {
            if trimmed.contains("(abort") || trimmed.contains("(return") {
                after_return = true;
                return_depth = end_depth;
            }
            // Also check for numeric returns at end of blocks (like "0)" or "1)")
            if regex::Regex::new(r"^\d+\)*$").unwrap().is_match(trimmed) && end_depth < start_depth {
                // This is a return value at end of block - reset unreachable tracking
                after_return = false;
            }
            // Reset if we've exited the return's scope
            if end_depth < return_depth {
                after_return = false;
            }
        }

        // Track variable definitions
        if !is_comment {
            for caps in define_pattern.captures_iter(line) {
                let var_name = caps.get(1).unwrap().as_str().to_string();

                // Check for shadowing
                if let Some(existing) = variables.get(&var_name) {
                    issues.push(LintIssue {
                        line: line_num,
                        message: format!(
                            "Variable '{}' shadows definition at line {}",
                            var_name, existing.defined_at
                        ),
                        suggestion: Some("Use a different name to avoid confusion".to_string()),
                        severity: LintSeverity::Warning,
                    });
                }

                // Check for shadowing builtins
                if builtins.contains(var_name.as_str()) {
                    issues.push(LintIssue {
                        line: line_num,
                        message: format!("Variable '{}' shadows a built-in function", var_name),
                        suggestion: Some("Use a different name - shadowing builtins is confusing".to_string()),
                        severity: LintSeverity::Warning,
                    });
                }

                variables.insert(var_name.clone(), VariableInfo {
                    name: var_name.clone(),
                    defined_at: line_num,
                    used_at: Vec::new(),
                    scope_depth: end_depth,
                });

                // Track in current scope
                if let Some((_, vars)) = scope_stack.last_mut() {
                    vars.push(var_name);
                }
            }

            // Track variable uses (but not in define/set! positions)
            let line_without_defines = define_pattern.replace_all(line, "(define __PLACEHOLDER__ ");
            let line_without_sets = set_pattern.replace_all(&line_without_defines, "(set! __PLACEHOLDER__ ");

            for caps in identifier_pattern.captures_iter(&line_without_sets) {
                let ident = caps.get(1).unwrap().as_str();
                if let Some(info) = variables.get_mut(ident) {
                    if !info.used_at.contains(&line_num) {
                        info.used_at.push(line_num);
                    }
                }
            }
        }

        // Check for discriminator pattern
        if let Some(caps) = discriminator_pattern.captures(line) {
            let instr_num = caps.get(1).map_or("?", |m| m.as_str());
            instruction_boundaries.push(InstructionBoundary {
                line: line_num,
                instruction_name: format!("Instruction {}", instr_num),
                depth_at_start: start_depth,
                depth_at_end: end_depth,
                expected_start_depth: base_depth,
            });

            // Check if depth is appropriate - all instructions should start at the SAME depth
            if start_depth != base_depth {
                issues.push(LintIssue {
                    line: line_num,
                    message: format!(
                        "Instruction {} starts at depth {} (expected {} based on first instruction)",
                        instr_num, start_depth, base_depth
                    ),
                    suggestion: Some(
                        "Check parenthesis balance in previous instruction block".to_string()
                    ),
                    severity: LintSeverity::Error,
                });
            }
        }

        // Check for potential structural issues
        if null_close_pattern.is_match(line) && end_depth > base_depth {
            // This might indicate the 'null' is inside a (do ...) instead of being the else branch
            if verbose {
                issues.push(LintIssue {
                    line: line_num,
                    message: format!(
                        "'null' appears at depth {} (should be {} for else branch)",
                        end_depth, base_depth
                    ),
                    suggestion: Some("Ensure (do ...) block is closed before 'null'".to_string()),
                    severity: LintSeverity::Info,
                });
            }
        }

        // Detect negative depth (too many closes)
        if depth < 0 {
            issues.push(LintIssue {
                line: line_num,
                message: "More closing parens than opening on this line".to_string(),
                suggestion: Some("Remove extra ')' characters".to_string()),
                severity: LintSeverity::Error,
            });
        }

        // Additional lint checks

        // Check for TODO/FIXME comments
        if is_comment && (trimmed.contains("TODO") || trimmed.contains("FIXME")) {
            issues.push(LintIssue {
                line: line_num,
                message: format!("Found {} marker", if trimmed.contains("TODO") { "TODO" } else { "FIXME" }),
                suggestion: None,
                severity: LintSeverity::Info,
            });
        }

        // Check for extremely long lines
        if line.len() > 120 && !is_comment {
            issues.push(LintIssue {
                line: line_num,
                message: format!("Line exceeds 120 characters ({} chars)", line.len()),
                suggestion: Some("Consider breaking into multiple lines for readability".to_string()),
                severity: LintSeverity::Info,
            });
        }
    }

    // Check final balance
    if depth != 0 {
        issues.push(LintIssue {
            line: lines.len(),
            message: format!("File ends with {} unclosed parentheses", depth),
            suggestion: Some(format!("Add {} closing parentheses at end", depth)),
            severity: LintSeverity::Error,
        });
    }

    // Check for unused top-level variables
    for (name, info) in &variables {
        if info.used_at.is_empty() && !name.starts_with('_') {
            issues.push(LintIssue {
                line: info.defined_at,
                message: format!("Unused variable '{}'", name),
                suggestion: Some("Remove or use this variable, or prefix with '_' to suppress".to_string()),
                severity: LintSeverity::Warning,
            });
        }
    }

    // Sort issues by line number, then by severity
    issues.sort_by(|a, b| {
        a.line.cmp(&b.line).then_with(|| {
            // Errors first, then warnings, then info
            match (&a.severity, &b.severity) {
                (LintSeverity::Error, LintSeverity::Error) => std::cmp::Ordering::Equal,
                (LintSeverity::Error, _) => std::cmp::Ordering::Less,
                (_, LintSeverity::Error) => std::cmp::Ordering::Greater,
                (LintSeverity::Warning, LintSeverity::Warning) => std::cmp::Ordering::Equal,
                (LintSeverity::Warning, _) => std::cmp::Ordering::Less,
                (_, LintSeverity::Warning) => std::cmp::Ordering::Greater,
                _ => std::cmp::Ordering::Equal,
            }
        })
    });

    // Count issues by severity
    let error_count = issues.iter().filter(|i| i.severity == LintSeverity::Error).count();
    let warning_count = issues.iter().filter(|i| i.severity == LintSeverity::Warning).count();
    let info_count = issues.iter().filter(|i| i.severity == LintSeverity::Info).count();

    // Generate summary
    let summary = if issues.is_empty() {
        format!(
            "✅ OVSM Lint: {} lines analyzed, {} instructions, no issues (base depth: {})",
            lines.len(),
            instruction_boundaries.len(),
            base_depth
        )
    } else {
        format!(
            "⚠️  OVSM Lint: {} lines, {} instructions — {} error(s), {} warning(s), {} info",
            lines.len(),
            instruction_boundaries.len(),
            error_count,
            warning_count,
            info_count
        )
    };

    // Verbose depth trace
    if verbose {
        println!("\n📈 Depth trace (line: start → end) [base depth: {}]:", base_depth);
        for (line_num, start, end) in &line_depths {
            if *start != *end || *start > 3 {
                let indicator = if *end > *start { "↗" } else if *end < *start { "↘" } else { "→" };
                println!("   Line {:4}: {:2} {} {:2}", line_num, start, indicator, end);
            }
        }
    }

    LintReport {
        summary,
        instruction_boundaries,
        issues,
        total_lines: lines.len(),
        max_depth,
        final_balance: depth,
    }
}

/// Count opening and closing parens, ignoring those in strings and comments
fn count_parens_smart(line: &str) -> (i32, i32) {
    let mut opens = 0i32;
    let mut closes = 0i32;
    let mut in_string = false;
    let mut chars = line.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            ';' if !in_string => break, // Comment - ignore rest of line
            '"' => in_string = !in_string,
            '\\' if in_string => { chars.next(); } // Escape sequence
            '(' if !in_string => opens += 1,
            ')' if !in_string => closes += 1,
            _ => {}
        }
    }

    (opens, closes)
}

/// Attempt to auto-fix simple parenthesis imbalances
fn attempt_auto_fix(source: &str, report: &LintReport) -> Option<String> {
    // Only auto-fix if it's a simple case: just missing closing parens at end
    if report.final_balance > 0 && report.issues.len() == 1 {
        let mut fixed = source.to_string();
        // Add the missing closing parens
        fixed.push_str(&")".repeat(report.final_balance as usize));
        Some(fixed)
    } else {
        None
    }
}

// ============================================================================
// OVSM Formatter
// ============================================================================

/// Format OVSM source code with consistent indentation
fn format_ovsm_script(source: &str, indent_size: usize) -> String {
    let mut result = String::new();
    let mut depth: i32 = 0;
    let indent = " ".repeat(indent_size);

    for line in source.lines() {
        let trimmed = line.trim();

        // Skip empty lines but preserve them
        if trimmed.is_empty() {
            result.push('\n');
            continue;
        }

        // Handle comments - preserve them at current depth
        if trimmed.starts_with(";;") {
            // Full-line comments get indented to current depth
            for _ in 0..depth {
                result.push_str(&indent);
            }
            result.push_str(trimmed);
            result.push('\n');
            continue;
        }

        // Count leading closes on this line (they should be dedented)
        let leading_closes = count_leading_closes(trimmed);

        // Adjust depth for leading closes BEFORE indenting
        let line_depth = (depth - leading_closes as i32).max(0);

        // Add indentation
        for _ in 0..line_depth {
            result.push_str(&indent);
        }

        // Add the trimmed line content
        result.push_str(trimmed);
        result.push('\n');

        // Update depth for next line based on ALL parens on this line
        let (opens, closes) = count_parens_smart(trimmed);
        depth = (depth + opens - closes).max(0);
    }

    // Remove trailing newline if original didn't have one
    if !source.ends_with('\n') && result.ends_with('\n') {
        result.pop();
    }

    result
}

/// Count leading closing parentheses on a line
fn count_leading_closes(line: &str) -> usize {
    let mut count = 0;
    for c in line.chars() {
        match c {
            ')' => count += 1,
            ' ' | '\t' => continue,
            _ => break,
        }
    }
    count
}

// ============================================================================
// IDL Type Decoding for validate-instruction
// ============================================================================

use ovsm::compiler::anchor_idl::IdlType;

/// Decode an IDL type from instruction data bytes
/// Returns (decoded_value_string, bytes_consumed)
fn decode_idl_type(ty: &IdlType, data: &[u8], offset: usize, _verbose: bool) -> (String, usize) {
    if offset >= data.len() {
        return ("<insufficient data>".to_string(), 0);
    }

    match ty {
        IdlType::Primitive(prim) => match prim.as_str() {
            "u8" => (format!("{}", data[offset]), 1),
            "i8" => (format!("{}", data[offset] as i8), 1),
            "u16" if offset + 2 <= data.len() => {
                let val = u16::from_le_bytes([data[offset], data[offset + 1]]);
                (format!("{}", val), 2)
            }
            "u32" if offset + 4 <= data.len() => {
                let val = u32::from_le_bytes([data[offset], data[offset + 1], data[offset + 2], data[offset + 3]]);
                (format!("{}", val), 4)
            }
            "u64" if offset + 8 <= data.len() => {
                let val = u64::from_le_bytes([
                    data[offset], data[offset + 1], data[offset + 2], data[offset + 3],
                    data[offset + 4], data[offset + 5], data[offset + 6], data[offset + 7],
                ]);
                (format!("{}", val), 8)
            }
            "i64" if offset + 8 <= data.len() => {
                let val = i64::from_le_bytes([
                    data[offset], data[offset + 1], data[offset + 2], data[offset + 3],
                    data[offset + 4], data[offset + 5], data[offset + 6], data[offset + 7],
                ]);
                (format!("{}", val), 8)
            }
            "bool" => ((if data[offset] != 0 { "true" } else { "false" }).to_string(), 1),
            "publicKey" | "pubkey" if offset + 32 <= data.len() => {
                let encoded = bs58::encode(&data[offset..offset + 32]).into_string();
                (encoded, 32)
            }
            "string" if offset + 4 <= data.len() => {
                let len = u32::from_le_bytes([data[offset], data[offset + 1], data[offset + 2], data[offset + 3]]) as usize;
                if offset + 4 + len <= data.len() {
                    match std::str::from_utf8(&data[offset + 4..offset + 4 + len]) {
                        Ok(s) => (format!("\"{}\"", s), 4 + len),
                        Err(_) => ("<invalid utf8>".to_string(), 4 + len),
                    }
                } else {
                    (format!("<string len {} exceeds data>", len), 4)
                }
            }
            other => (format!("<{}>", other), type_size(other)),
        },
        IdlType::Array { array: _ } => ("<array>".to_string(), 0),
        IdlType::Vec { vec: _ } if offset + 4 <= data.len() => {
            let len = u32::from_le_bytes([data[offset], data[offset + 1], data[offset + 2], data[offset + 3]]);
            (format!("<vec of {} elements>", len), 4)
        }
        IdlType::Option { option } if offset < data.len() => {
            if data[offset] == 0 {
                ("None".to_string(), 1)
            } else {
                let (inner_val, inner_size) = decode_idl_type(option, data, offset + 1, _verbose);
                (format!("Some({})", inner_val), 1 + inner_size)
            }
        }
        IdlType::Defined { defined } => (format!("<defined:{}>", defined), 0),
        _ => ("<?>".to_string(), 0),
    }
}

fn type_size(prim: &str) -> usize {
    match prim {
        "u8" | "i8" | "bool" => 1,
        "u16" | "i16" => 2,
        "u32" | "i32" => 4,
        "u64" | "i64" => 8,
        "u128" | "i128" => 16,
        "publicKey" | "pubkey" => 32,
        _ => 0,
    }
}

fn format_idl_type(ty: &IdlType) -> String {
    match ty {
        IdlType::Primitive(p) => p.clone(),
        IdlType::Array { array: _ } => "array".to_string(),
        IdlType::Vec { vec } => format!("Vec<{}>", format_idl_type(vec)),
        IdlType::Option { option } => format!("Option<{}>", format_idl_type(option)),
        IdlType::Defined { defined } => defined.clone(),
    }
}
