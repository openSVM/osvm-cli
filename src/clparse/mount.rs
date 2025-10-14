use clap::{Arg, ArgAction, Command};

/// Build the mount command
pub fn build_mount_command() -> Command {
    Command::new("mount")
        .about("Manage folder mounts for OSVM microVMs and MCP tools")
        .arg_required_else_help(true)
        .subcommand(
            Command::new("add")
                .about("Mount a folder to OSVM microVM (auto-detected VM path)")
                .arg(
                    Arg::new("host_path")
                        .help("Host folder path to mount (e.g., ~/Documents)")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::new("readonly")
                        .long("readonly")
                        .short('r')
                        .action(ArgAction::SetTrue)
                        .help("Mount as read-only"),
                ),
        )
        .subcommand(
            Command::new("remove")
                .about("Unmount a folder from OSVM microVM")
                .arg(
                    Arg::new("host_path")
                        .help("Host folder path to unmount")
                        .required(true)
                        .index(1),
                ),
        )
        .subcommand(Command::new("list").about("List all OSVM microVM mounts"))
}
