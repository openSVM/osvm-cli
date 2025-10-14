//! OVSM (Open Versatile Seeker Mind) language interpreter command definition

use clap::{Arg, ArgAction, Command};

pub fn build_ovsm_command() -> Command {
    Command::new("ovsm")
        .about("OVSM language interpreter for blockchain automation scripting")
        .long_about("Execute and manage OVSM (Open Versatile Seeker Mind) scripts for blockchain automation.\n\
                   \n\
                   OVSM is a production-ready scripting language (97.3% test coverage) designed for\n\
                   blockchain operations with full control flow, loops, and rich data types.\n\
                   \n\
                   Features:\n\
                   • Control Flow: IF/THEN/ELSE, FOR, WHILE, BREAK, CONTINUE\n\
                   • Data Types: Int, Float, String, Bool, Arrays, Objects, Ranges\n\
                   • OSVM Integration: Direct access to blockchain operations\n\
                   • MCP Tools: Call MCP servers from scripts\n\
                   • MicroVM Isolation: Run untrusted scripts safely\n\
                   \n\
                   Quick Examples:\n\
                   • osvm ovsm run script.ovsm              # Execute script file\n\
                   • osvm ovsm repl                         # Interactive REPL\n\
                   • osvm ovsm eval '$x = 10; RETURN $x'    # Inline evaluation\n\
                   • osvm ovsm examples                     # Show example scripts\n\
                   • osvm ovsm generate \"monitor validators\" # AI script generation")
        .arg_required_else_help(true)
        .subcommand(
            Command::new("run")
                .about("Execute an OVSM script file")
                .arg(
                    Arg::new("script")
                        .value_name("SCRIPT")
                        .help("Path to OVSM script file (.ovsm)")
                        .required(true)
                        .index(1)
                )
                .arg(
                    Arg::new("args")
                        .value_name("ARGS")
                        .help("Arguments to pass to the script (as JSON array)")
                        .long("args")
                )
                .arg(
                    Arg::new("isolated")
                        .long("isolated")
                        .action(ArgAction::SetTrue)
                        .help("Run script in isolated microVM for security")
                )
                .arg(
                    Arg::new("verbose")
                        .long("verbose")
                        .short('v')
                        .action(ArgAction::Count)
                        .help("Show detailed execution information")
                )
                .arg(
                    Arg::new("debug")
                        .long("debug")
                        .action(ArgAction::SetTrue)
                        .help("Enable debug mode with step-by-step execution")
                )
                .arg(
                    Arg::new("json")
                        .long("json")
                        .action(ArgAction::SetTrue)
                        .help("Output results in JSON format")
                )
        )
        .subcommand(
            Command::new("repl")
                .about("Launch interactive OVSM REPL (Read-Eval-Print Loop)")
                .long_about("Start an interactive OVSM shell for experimentation and learning.\n\
                           \n\
                           REPL Features:\n\
                           • Line editing with history\n\
                           • Multi-line input support\n\
                           • Access to all OSVM tools\n\
                           • Session recording\n\
                           • Help and documentation lookup")
                .arg(
                    Arg::new("history")
                        .long("history")
                        .value_name("FILE")
                        .help("History file location (default: ~/.osvm/ovsm_history)")
                )
        )
        .subcommand(
            Command::new("eval")
                .about("Evaluate inline OVSM code")
                .arg(
                    Arg::new("code")
                        .value_name("CODE")
                        .help("OVSM code to evaluate")
                        .required(true)
                        .index(1)
                )
                .arg(
                    Arg::new("json")
                        .long("json")
                        .action(ArgAction::SetTrue)
                        .help("Output result in JSON format")
                )
        )
        .subcommand(
            Command::new("check")
                .about("Check OVSM script syntax without executing")
                .arg(
                    Arg::new("script")
                        .value_name("SCRIPT")
                        .help("Path to OVSM script file")
                        .required(true)
                        .index(1)
                )
                .arg(
                    Arg::new("strict")
                        .long("strict")
                        .action(ArgAction::SetTrue)
                        .help("Enable strict syntax checking")
                )
        )
        .subcommand(
            Command::new("examples")
                .about("Show OVSM example scripts and tutorials")
                .arg(
                    Arg::new("category")
                        .long("category")
                        .short('c')
                        .value_name("CATEGORY")
                        .value_parser(clap::builder::PossibleValuesParser::new([
                            "basics",
                            "blockchain",
                            "automation",
                            "mcp",
                            "advanced"
                        ]))
                        .help("Filter examples by category")
                )
                .arg(
                    Arg::new("list")
                        .long("list")
                        .action(ArgAction::SetTrue)
                        .help("List all available examples")
                )
                .arg(
                    Arg::new("show")
                        .long("show")
                        .value_name("NAME")
                        .help("Show specific example script")
                )
        )
        .subcommand(
            Command::new("generate")
                .about("Generate OVSM script from natural language description (AI-powered)")
                .arg(
                    Arg::new("description")
                        .value_name("DESCRIPTION")
                        .help("Natural language description of desired script")
                        .required(true)
                        .index(1)
                )
                .arg(
                    Arg::new("output")
                        .long("output")
                        .short('o')
                        .value_name("FILE")
                        .help("Save generated script to file")
                )
                .arg(
                    Arg::new("interactive")
                        .long("interactive")
                        .short('i')
                        .action(ArgAction::SetTrue)
                        .help("Interactive mode with refinement")
                )
        )
        .subcommand(
            Command::new("library")
                .about("Manage OVSM script library")
                .arg_required_else_help(true)
                .subcommand(
                    Command::new("list")
                        .about("List installed scripts in library")
                )
                .subcommand(
                    Command::new("install")
                        .about("Install script from URL or GitHub")
                        .arg(
                            Arg::new("source")
                                .value_name("URL")
                                .help("GitHub URL or script URL")
                                .required(true)
                                .index(1)
                        )
                        .arg(
                            Arg::new("name")
                                .long("name")
                                .value_name("NAME")
                                .help("Custom name for the script")
                        )
                )
                .subcommand(
                    Command::new("remove")
                        .about("Remove script from library")
                        .arg(
                            Arg::new("name")
                                .value_name("NAME")
                                .help("Script name to remove")
                                .required(true)
                                .index(1)
                        )
                )
                .subcommand(
                    Command::new("run")
                        .about("Run installed library script")
                        .arg(
                            Arg::new("name")
                                .value_name("NAME")
                                .help("Script name to run")
                                .required(true)
                                .index(1)
                        )
                        .arg(
                            Arg::new("args")
                                .value_name("ARGS")
                                .help("Arguments to pass (JSON array)")
                                .long("args")
                        )
                )
                .subcommand(
                    Command::new("update")
                        .about("Update installed scripts")
                        .arg(
                            Arg::new("name")
                                .value_name("NAME")
                                .help("Script name to update (updates all if not specified)")
                                .index(1)
                        )
                )
        )
}
