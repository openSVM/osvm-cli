# Bash completion for osvm CLI
# Install: Copy to /etc/bash_completion.d/ or source in ~/.bashrc

_osvm_completion() {
    local cur prev words cword
    _init_completion || return

    # Main commands
    local commands="balance rpc svm nodes deploy audit mcp chat doctor ovsm agent plan snapshot db realtime version help"

    # Subcommands for each main command
    local rpc_subcmds="local devnet mainnet testnet status stop"
    local svm_subcmds="list add remove status dashboard"
    local nodes_subcmds="list add remove status dashboard"
    local mcp_subcmds="add list remove setup start stop status"
    local ovsm_subcmds="run eval check repl examples"
    local db_subcmds="init sync status query"

    # Flags
    local global_flags="--help --version --verbose --debug --config --keypair --url"
    local chat_flags="--advanced --microvm --test --test_mode"
    local audit_flags="--gh --ai-analysis --format --output"
    local ovsm_flags="--file --input --output"

    # Get the main command (first non-flag argument)
    local main_cmd=""
    for ((i=1; i < cword; i++)); do
        if [[ ${words[i]} != -* ]]; then
            main_cmd="${words[i]}"
            break
        fi
    done

    # If no main command yet, suggest main commands
    if [[ -z "$main_cmd" ]]; then
        COMPREPLY=($(compgen -W "$commands $global_flags" -- "$cur"))
        return 0
    fi

    # Complete based on main command
    case "$main_cmd" in
        rpc)
            COMPREPLY=($(compgen -W "$rpc_subcmds $global_flags" -- "$cur"))
            ;;
        svm)
            COMPREPLY=($(compgen -W "$svm_subcmds $global_flags" -- "$cur"))
            ;;
        nodes)
            COMPREPLY=($(compgen -W "$nodes_subcmds $global_flags" -- "$cur"))
            ;;
        mcp)
            COMPREPLY=($(compgen -W "$mcp_subcmds $global_flags" -- "$cur"))
            ;;
        ovsm)
            if [[ "$prev" == "run" ]] || [[ "$prev" == "check" ]] || [[ "$prev" == "--file" ]]; then
                # Complete .ovsm files
                COMPREPLY=($(compgen -f -X '!*.ovsm' -- "$cur"))
            else
                COMPREPLY=($(compgen -W "$ovsm_subcmds $ovsm_flags $global_flags" -- "$cur"))
            fi
            ;;
        chat)
            COMPREPLY=($(compgen -W "$chat_flags $global_flags" -- "$cur"))
            ;;
        audit)
            COMPREPLY=($(compgen -W "$audit_flags $global_flags" -- "$cur"))
            ;;
        db)
            COMPREPLY=($(compgen -W "$db_subcmds $global_flags" -- "$cur"))
            ;;
        --config)
            # Complete config files
            COMPREPLY=($(compgen -f -X '!*.yml' -X '!*.yaml' -- "$cur"))
            ;;
        --keypair)
            # Complete JSON files
            COMPREPLY=($(compgen -f -X '!*.json' -- "$cur"))
            ;;
        *)
            COMPREPLY=($(compgen -W "$global_flags" -- "$cur"))
            ;;
    esac

    return 0
}

complete -F _osvm_completion osvm
