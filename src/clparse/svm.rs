use clap::{Arg, Command};

/// Build the SVM management command
pub fn build_svm_command() -> Command {
    Command::new("svm")
        .about("Manage Solana Virtual Machines (SVMs)")
        .arg_required_else_help(true)
        .subcommand(Command::new("list").about("List all SVMs installed in the chain"))
        .subcommand(
            Command::new("dashboard").about("Launch interactive SVM monitoring dashboard"),
        )
        .subcommand(
            Command::new("get")
                .about("Get detailed information about a specific SVM")
                .arg(
                    Arg::new("name")
                        .value_name("NAME")
                        .index(1)
                        .required(true)
                        .help("Name of the SVM to get information about"),
                ),
        )
        .subcommand(
            Command::new("install")
                .about("🚧 COMING SOON: Install an SVM on a remote host")
                .arg(
                    Arg::new("name")
                        .value_name("NAME")
                        .index(1)
                        .required(true)
                        .help("Name of the SVM to install"),
                )
                .arg(
                    Arg::new("host")
                        .long("host")
                        .value_name("HOST")
                        .required(true)
                        .help("Remote host to install on (format: user@host[:port])"),
                ),
        )
}
