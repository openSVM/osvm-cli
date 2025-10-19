#compdef osvm
# Zsh completion for osvm CLI
# Install: Copy to ~/.zsh/completions/ or /usr/local/share/zsh/site-functions/_osvm

_osvm() {
    local -a commands
    commands=(
        'balance:Show SOL balance for an address'
        'rpc:RPC node management'
        'svm:SVM instance management'
        'nodes:Node management and monitoring'
        'deploy:Deploy eBPF programs'
        'audit:Security audit with AI analysis'
        'mcp:Model Context Protocol server management'
        'chat:AI-powered chat interface'
        'doctor:System diagnostics and repair'
        'ovsm:OVSM LISP interpreter'
        'agent:CLI-based agent execution'
        'plan:Plan execution interface'
        'snapshot:Snapshot management'
        'db:Database operations'
        'realtime:Real-time monitoring'
        'version:Show version information'
        'help:Show help information'
    )

    local -a rpc_commands
    rpc_commands=(
        'local:Start local RPC node'
        'devnet:Start devnet validator'
        'mainnet:Connect to mainnet'
        'testnet:Connect to testnet'
        'status:Show RPC status'
        'stop:Stop RPC node'
    )

    local -a svm_commands
    svm_commands=(
        'list:List available SVMs'
        'add:Add new SVM instance'
        'remove:Remove SVM instance'
        'status:Show SVM status'
        'dashboard:Launch SVM dashboard'
    )

    local -a mcp_commands
    mcp_commands=(
        'add:Add MCP server'
        'list:List MCP servers'
        'remove:Remove MCP server'
        'setup:Setup MCP configuration'
        'start:Start MCP server'
        'stop:Stop MCP server'
        'status:Show MCP server status'
    )

    local -a ovsm_commands
    ovsm_commands=(
        'run:Execute OVSM LISP script'
        'eval:Evaluate OVSM LISP expression'
        'check:Check OVSM script syntax'
        'repl:Start interactive REPL'
        'examples:Show OVSM examples'
    )

    local -a db_commands
    db_commands=(
        'init:Initialize database'
        'sync:Sync database'
        'status:Show database status'
        'query:Query database'
    )

    _arguments -C \
        '(- *)'{-h,--help}'[Show help information]' \
        '(- *)'{-V,--version}'[Show version information]' \
        '(-v --verbose)'{-v,--verbose}'[Verbose output]' \
        '(--debug)--debug[Debug mode]' \
        '(--config)--config[Config file path]:config file:_files -g "*.{yml,yaml}"' \
        '(--keypair)--keypair[Keypair file path]:keypair file:_files -g "*.json"' \
        '(--url)--url[RPC URL]:rpc url:' \
        '1: :->command' \
        '*:: :->args'

    case $state in
        command)
            _describe 'osvm commands' commands
            ;;
        args)
            case $words[1] in
                rpc)
                    _describe 'rpc subcommands' rpc_commands
                    ;;
                svm)
                    _describe 'svm subcommands' svm_commands
                    ;;
                nodes)
                    _describe 'nodes subcommands' svm_commands
                    ;;
                mcp)
                    _describe 'mcp subcommands' mcp_commands
                    ;;
                ovsm)
                    case $words[2] in
                        run|check)
                            _arguments \
                                '*:ovsm file:_files -g "*.ovsm"'
                            ;;
                        *)
                            _describe 'ovsm subcommands' ovsm_commands
                            ;;
                    esac
                    ;;
                chat)
                    _arguments \
                        '(--advanced)--advanced[Use advanced chat interface]' \
                        '(--microvm)--microvm[Use MicroVM isolated chat]' \
                        '(--test)--test[Run chat UI tests]'
                    ;;
                audit)
                    _arguments \
                        '(--gh)--gh[GitHub repository]:repository:' \
                        '(--ai-analysis)--ai-analysis[Enable AI analysis]' \
                        '(--format)--format[Output format]:format:(json html markdown)' \
                        '(--output)--output[Output directory]:directory:_files -/'
                    ;;
                db)
                    _describe 'db subcommands' db_commands
                    ;;
            esac
            ;;
    esac
}

_osvm "$@"
