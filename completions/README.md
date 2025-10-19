# OSVM Shell Completions

Shell completions for the `osvm` CLI provide tab-completion for commands, subcommands, and file paths.

## Installation

### Bash

**System-wide (requires sudo):**
```bash
sudo cp osvm.bash /etc/bash_completion.d/osvm
```

**User-specific:**
```bash
mkdir -p ~/.bash_completion.d
cp osvm.bash ~/.bash_completion.d/osvm
echo 'source ~/.bash_completion.d/osvm' >> ~/.bashrc
source ~/.bashrc
```

**Test:**
```bash
osvm <TAB><TAB>
# Shows: balance rpc svm nodes deploy audit mcp chat doctor ovsm...
```

### Zsh

**System-wide (requires sudo):**
```bash
sudo cp osvm.zsh /usr/local/share/zsh/site-functions/_osvm
```

**User-specific:**
```bash
mkdir -p ~/.zsh/completions
cp osvm.zsh ~/.zsh/completions/_osvm
echo 'fpath=(~/.zsh/completions $fpath)' >> ~/.zshrc
echo 'autoload -Uz compinit && compinit' >> ~/.zshrc
source ~/.zshrc
```

**Test:**
```bash
osvm <TAB>
# Shows completions with descriptions
```

### Fish

**Coming soon!**

Contributions welcome at https://github.com/openSVM/osvm-cli

## Features

### Command Completion
```bash
osvm <TAB>
# Suggests: balance, rpc, svm, nodes, deploy, audit, mcp, chat, doctor, ovsm...
```

### Subcommand Completion
```bash
osvm rpc <TAB>
# Suggests: local, devnet, mainnet, testnet, status, stop

osvm ovsm <TAB>
# Suggests: run, eval, check, repl, examples
```

### File Completion
```bash
osvm ovsm run <TAB>
# Suggests: *.ovsm files in current directory

osvm --config <TAB>
# Suggests: *.yml and *.yaml files

osvm --keypair <TAB>
# Suggests: *.json files
```

### Flag Completion
```bash
osvm --<TAB>
# Suggests: --help, --version, --verbose, --debug, --config, --keypair, --url
```

## Supported Commands

### Main Commands
- `balance` - Show SOL balance
- `rpc` - RPC node management
- `svm` - SVM instance management
- `nodes` - Node management
- `deploy` - Deploy eBPF programs
- `audit` - Security audit
- `mcp` - MCP server management
- `chat` - AI chat interface
- `doctor` - System diagnostics
- `ovsm` - OVSM LISP interpreter
- `agent` - Agent execution
- `plan` - Plan execution
- `snapshot` - Snapshot management
- `db` - Database operations
- `realtime` - Real-time monitoring

### RPC Subcommands
- `local` - Start local RPC
- `devnet` - Start devnet validator
- `mainnet` - Connect to mainnet
- `testnet` - Connect to testnet
- `status` - Show status
- `stop` - Stop RPC

### OVSM Subcommands
- `run <file.ovsm>` - Execute LISP script
- `eval <expr>` - Evaluate LISP expression
- `check <file.ovsm>` - Check syntax
- `repl` - Interactive REPL
- `examples` - Show examples

### MCP Subcommands
- `add <name> <url>` - Add server
- `list` - List servers
- `remove <name>` - Remove server
- `setup` - Setup configuration
- `start <name>` - Start server
- `stop <name>` - Stop server
- `status` - Show status

## Global Flags

All commands support:
- `--help, -h` - Show help
- `--version, -V` - Show version
- `--verbose, -v` - Verbose output
- `--debug` - Debug mode
- `--config <file>` - Config file path
- `--keypair <file>` - Keypair file path
- `--url <url>` - RPC URL

## Examples

### Basic Usage
```bash
# Tab-complete main command
$ osvm r<TAB>
rpc realtime

# Tab-complete subcommand
$ osvm rpc <TAB>
local devnet mainnet testnet status stop

# Tab-complete file
$ osvm ovsm run examples/<TAB>
examples/factorial.ovsm examples/hello_world.ovsm
```

### Advanced Usage
```bash
# Complete with flags
$ osvm --<TAB>
--help --version --verbose --debug --config --keypair --url

# Complete config file
$ osvm --config ~/.config/osvm/<TAB>
~/.config/osvm/config.yml

# Complete OVSM script
$ osvm ovsm run <TAB>
script.ovsm test.ovsm examples/
```

## Troubleshooting

### Completions not working (Bash)
```bash
# Check if bash-completion is installed
dpkg -l | grep bash-completion

# Install if missing
sudo apt-get install bash-completion

# Reload bashrc
source ~/.bashrc
```

### Completions not working (Zsh)
```bash
# Check fpath
echo $fpath

# Ensure compinit is run
echo 'autoload -Uz compinit && compinit' >> ~/.zshrc
source ~/.zshrc

# Clear completion cache
rm -f ~/.zcompdump*
compinit
```

### Updating Completions

After updating OSVM, reinstall completions:

```bash
# Bash
sudo cp completions/osvm.bash /etc/bash_completion.d/osvm
source ~/.bashrc

# Zsh
sudo cp completions/osvm.zsh /usr/local/share/zsh/site-functions/_osvm
rm -f ~/.zcompdump*
source ~/.zshrc
```

## Contributing

To add new commands or improve completions:

1. Edit `osvm.bash` or `osvm.zsh`
2. Test locally
3. Submit PR to https://github.com/openSVM/osvm-cli

### Adding New Commands

**Bash (`osvm.bash`):**
```bash
# Add to main commands list
local commands="balance rpc svm ... newcommand"

# Add subcommands if applicable
local newcommand_subcmds="sub1 sub2 sub3"

# Add completion case
case "$main_cmd" in
    ...
    newcommand)
        COMPREPLY=($(compgen -W "$newcommand_subcmds $global_flags" -- "$cur"))
        ;;
esac
```

**Zsh (`osvm.zsh`):**
```zsh
# Add to commands array
commands=(
    ...
    'newcommand:Description of new command'
)

# Add subcommands array
local -a newcommand_commands
newcommand_commands=(
    'sub1:Description of sub1'
    'sub2:Description of sub2'
)

# Add completion case
case $words[1] in
    ...
    newcommand)
        _describe 'newcommand subcommands' newcommand_commands
        ;;
esac
```

## License

MIT License - Same as OSVM CLI

## Support

- GitHub Issues: https://github.com/openSVM/osvm-cli/issues
- Documentation: https://docs.osvm.dev
- Discord: https://discord.gg/osvm (coming soon)
