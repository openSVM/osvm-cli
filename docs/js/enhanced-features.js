/**
 * Enhanced Features for OSVM Landing Page
 * - Command expandable examples with terminal output
 * - Improved copy to clipboard
 * - Interactive onboarding
 */

// ===== EXPANDABLE COMMAND EXAMPLES =====

/**
 * Command examples database with terminal output
 */
const commandExamples = {
    'osvm chat': {
        description: 'Start AI-powered chat with code execution',
        output: `$ osvm chat

┌─────────────────────────────────────────┐
│   OSVM Agent Chat (Enhanced)            │
│   Type /help for commands               │
└─────────────────────────────────────────┘

> Calculate the sum of 1 to 100

• Assistant: I'll help you calculate that using OVSM LISP:

\`\`\`lisp
(define total 0)
(for (i (range 1 101))
  (set! total (+ total i)))
total
\`\`\`

╭─ OVSM Code Block 1 ─
│ (define total 0)
│ (for (i (range 1 101))
│   (set! total (+ total i)))
│ ...
╰─

Execute? ([y]es/[n]o/[v]iew full): y

▶ Executing OVSM code (30s timeout)...
✓ Execution successful!
Result: Number(5050)`,
        category: 'ai'
    },
    'osvm ovsm run script.ovsm': {
        description: 'Execute an OVSM LISP script file',
        output: `$ osvm ovsm run examples/ovsm_scripts/factorial.ovsm

🔍 Parsing OVSM code...
✅ Tokenization successful (42 tokens)
✅ Parsing successful
▶  Executing...

Calculating factorial of 5...
Result: 120

✨ Execution completed successfully`,
        category: 'ovsm'
    },
    'osvm ovsm eval \'(+ 1 2 3 4 5)\'': {
        description: 'Execute inline OVSM LISP code',
        output: `$ osvm ovsm eval '(+ 1 2 3 4 5)'

✅ Tokenization successful (7 tokens)
✅ Parsing successful
▶  Executing...

Result: 15`,
        category: 'ovsm'
    },
    'osvm doctor --fix': {
        description: 'Run system diagnostics and auto-fix issues',
        output: `$ osvm doctor --fix

🔍 Running system diagnostics...

System Health:
  ✓ Rust toolchain: 1.70+ installed
  ✓ Cargo: Available
  ✓ OSVM binary: /usr/bin/osvm (v0.9.2)
  ✓ Configuration: ~/.config/osvm/config.yml
  ⚠ Solana CLI: Not found

Network Connectivity:
  ✓ RPC endpoint: api.mainnet-beta.solana.com (48ms)
  ✓ Internet: Connected

Permissions:
  ✓ Config directory: Writable
  ✓ Cache directory: Writable

Auto-fixing issues...
  → Installing Solana CLI...
  ✓ Solana CLI installed

✨ All issues resolved!
System health: 100%`,
        category: 'utils'
    },
    'osvm svm list': {
        description: 'List available Solana Virtual Machines',
        output: `$ osvm svm list

Available SVMs:

┌─────────┬──────────────┬──────────┬─────────┐
│ Name    │ Network      │ Version  │ Status  │
├─────────┼──────────────┼──────────┼─────────┤
│ sonic   │ Mainnet      │ 1.17.15  │ Active  │
│ solana  │ Mainnet/Test │ 1.16.28  │ Active  │
│ eclipse │ Mainnet      │ 0.8.2    │ Beta    │
│ soon    │ Testnet      │ 0.2.0    │ Alpha   │
└─────────┴──────────────┴──────────┴─────────┘

Total: 4 SVMs available

Use 'osvm svm get <name>' for details`,
        category: 'svm'
    },
    'osvm audit ./contracts': {
        description: 'Run AI-powered security audit on code',
        output: `$ osvm audit ./contracts

🔍 AI Security Audit

Scanning: ./contracts
Files found: 12 Rust files, 3 OVSM scripts

Running analysis...
  ✓ Static analysis complete
  ✓ Dependency check complete
  ✓ AI vulnerability detection complete

Results:

🔴 Critical (1):
  • Unchecked arithmetic in transfer.rs:142
    → Potential integer overflow
    → Recommendation: Use checked_add()

🟡 Medium (3):
  • Missing input validation in swap.rs:89
  • Deprecated dependency: solana-sdk 1.14.0
  • Unused variable in pool.rs:256

🟢 Low (5):
  • Code style inconsistency
  • Missing documentation
  • Unreachable code detected

Security Score: 7.5/10
Audit complete in 12.4s

Full report: ./audit-report.html`,
        category: 'security'
    },
    'osvm mcp list': {
        description: 'List configured MCP servers',
        output: `$ osvm mcp list

Configured MCP Servers:

┌────────────────┬─────────┬────────────────────────┐
│ Server ID      │ Status  │ URL                    │
├────────────────┼─────────┼────────────────────────┤
│ solana-server  │ Active  │ http://localhost:3000  │
│ github-tools   │ Active  │ stdio                  │
│ blockchain-api │ Stopped │ http://localhost:3001  │
└────────────────┴─────────┴────────────────────────┘

Total tools available: 24

Use 'osvm mcp tools <server>' to list tools`,
        category: 'mcp'
    },
    'osvm balance': {
        description: 'Check SOL balance for configured wallet',
        output: `$ osvm balance

Wallet: 7xKX...9dF2 (from ~/.config/solana/id.json)
Network: Mainnet Beta

Balance: 12.458 SOL ($2,847.42 USD)

Recent transactions: 3 in last 24h
Last updated: 2 seconds ago`,
        category: 'blockchain'
    },
    'osvm --version': {
        description: 'Show OSVM CLI version',
        output: `$ osvm --version

osvm 0.9.2

Components:
  • OSVM CLI: 0.9.2
  • OVSM LISP: 1.0.1 (100% test coverage)
  • Rust: 1.70.0
  • Solana SDK: 3.0.0

Features:
  ✓ AI-Powered Chat
  ✓ OVSM Language
  ✓ MCP Integration
  ✓ Hardware Isolation
  ✓ Zero-Downtime Updates`,
        category: 'info'
    }
};

/**
 * Initialize expandable command examples
 */
function initExpandableCommands() {
    // Find all code blocks with commands
    document.querySelectorAll('code, .command, .cmd').forEach(codeElement => {
        const command = codeElement.textContent.trim();

        // Check if this command has an example
        if (commandExamples[command]) {
            makeCommandExpandable(codeElement, command);
        }
    });

    // Add click handlers to copyable commands
    document.querySelectorAll('.copyable-command').forEach(element => {
        element.addEventListener('click', function(e) {
            // If clicking the expand button, don't copy
            if (e.target.closest('.expand-btn')) {
                return;
            }

            const command = this.dataset.command || this.textContent.trim();
            copyCommandToClipboard(command, this);
        });
    });

    console.log('✨ Expandable commands initialized');
}

/**
 * Make a command element expandable
 */
function makeCommandExpandable(element, command) {
    const example = commandExamples[command];

    // Wrap element if not already wrapped
    if (!element.closest('.command-wrapper')) {
        const wrapper = document.createElement('div');
        wrapper.className = 'command-wrapper';
        element.parentNode.insertBefore(wrapper, element);
        wrapper.appendChild(element);

        // Add expand button
        const expandBtn = document.createElement('button');
        expandBtn.className = 'expand-btn';
        expandBtn.innerHTML = `
            <span class="expand-icon">▶</span>
            <span class="expand-text">Try Example</span>
        `;
        expandBtn.onclick = () => toggleCommandExample(wrapper, command);

        // Add copy button
        const copyBtn = document.createElement('button');
        copyBtn.className = 'copy-cmd-btn';
        copyBtn.innerHTML = '📋 Copy';
        copyBtn.onclick = (e) => {
            e.stopPropagation();
            copyCommandToClipboard(command, copyBtn);
        };

        wrapper.appendChild(expandBtn);
        wrapper.appendChild(copyBtn);

        // Add description tooltip
        const tooltip = document.createElement('div');
        tooltip.className = 'cmd-tooltip';
        tooltip.textContent = example.description;
        wrapper.appendChild(tooltip);
    }
}

/**
 * Toggle command example visibility
 */
function toggleCommandExample(wrapper, command) {
    const example = commandExamples[command];
    const expandBtn = wrapper.querySelector('.expand-btn');
    const expandIcon = expandBtn.querySelector('.expand-icon');

    let exampleDiv = wrapper.querySelector('.command-example');

    if (exampleDiv) {
        // Collapse
        exampleDiv.style.maxHeight = exampleDiv.scrollHeight + 'px';
        requestAnimationFrame(() => {
            exampleDiv.style.maxHeight = '0';
        });

        setTimeout(() => {
            exampleDiv.remove();
            expandIcon.textContent = '▶';
            expandBtn.classList.remove('expanded');
        }, 300);
    } else {
        // Expand
        exampleDiv = document.createElement('div');
        exampleDiv.className = 'command-example';
        exampleDiv.innerHTML = `
            <div class="example-header">
                <span class="example-label">Terminal Output:</span>
                <span class="example-category">${example.category}</span>
            </div>
            <pre class="terminal-output">${escapeHtml(example.output)}</pre>
            <div class="example-footer">
                <button class="try-btn" onclick="copyCommandToClipboard('${command.replace(/'/g, "\\'")}')">
                    🚀 Copy & Try It
                </button>
            </div>
        `;

        wrapper.appendChild(exampleDiv);

        // Animate expansion
        requestAnimationFrame(() => {
            exampleDiv.style.maxHeight = exampleDiv.scrollHeight + 'px';
            expandIcon.textContent = '▼';
            expandBtn.classList.add('expanded');
        });

        // Remove max-height after animation
        setTimeout(() => {
            exampleDiv.style.maxHeight = 'none';
        }, 300);
    }
}

/**
 * Copy command to clipboard with enhanced feedback
 */
function copyCommandToClipboard(command, buttonElement) {
    // Clean command
    const cleanCommand = command
        .replace(/&quot;/g, '"')
        .replace(/&amp;/g, '&')
        .replace(/&lt;/g, '<')
        .replace(/&gt;/g, '>');

    navigator.clipboard.writeText(cleanCommand).then(() => {
        // Update button
        if (buttonElement) {
            const originalText = buttonElement.innerHTML;
            buttonElement.innerHTML = '✓ Copied!';
            buttonElement.classList.add('copied');

            setTimeout(() => {
                buttonElement.innerHTML = originalText;
                buttonElement.classList.remove('copied');
            }, 2000);
        }

        // Show toast notification
        showToast(`Copied: ${cleanCommand.substring(0, 40)}${cleanCommand.length > 40 ? '...' : ''}`, 'success');
    }).catch(err => {
        console.error('Copy failed:', err);
        showToast('Copy failed - please try again', 'error');
    });
}

/**
 * Show toast notification
 */
function showToast(message, type = 'info') {
    // Remove existing toasts
    document.querySelectorAll('.toast').forEach(t => t.remove());

    const toast = document.createElement('div');
    toast.className = `toast toast-${type}`;
    toast.innerHTML = `
        <span class="toast-icon">${type === 'success' ? '✓' : type === 'error' ? '✗' : 'ℹ'}</span>
        <span class="toast-message">${message}</span>
    `;

    document.body.appendChild(toast);

    // Animate in
    requestAnimationFrame(() => {
        toast.classList.add('show');
    });

    // Auto remove
    setTimeout(() => {
        toast.classList.remove('show');
        setTimeout(() => toast.remove(), 300);
    }, 3000);
}

/**
 * Escape HTML for safe display
 */
function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

// ===== ENHANCED COPY TO CLIPBOARD =====

/**
 * Fix copy to clipboard for all code blocks
 */
function initCopyToClipboard() {
    // Add copy buttons to all pre/code blocks
    document.querySelectorAll('pre code, pre, .code-block').forEach((block, index) => {
        if (block.classList.contains('no-copy')) return;
        if (block.closest('.command-wrapper')) return; // Skip if already in command wrapper

        const wrapper = document.createElement('div');
        wrapper.className = 'code-block-wrapper';
        block.parentNode.insertBefore(wrapper, block);
        wrapper.appendChild(block);

        const copyBtn = document.createElement('button');
        copyBtn.className = 'copy-code-btn';
        copyBtn.innerHTML = '📋';
        copyBtn.title = 'Copy to clipboard';
        copyBtn.onclick = () => {
            const code = block.textContent;
            copyCommandToClipboard(code, copyBtn);
        };

        wrapper.appendChild(copyBtn);
    });

    console.log('📋 Copy to clipboard initialized');
}

// ===== INTERACTIVE ONBOARDING =====

/**
 * Create onboarding tour for first-time visitors
 */
function initOnboarding() {
    // Check if user has seen onboarding
    if (localStorage.getItem('osvm_onboarding_seen')) {
        return;
    }

    // Show onboarding after brief delay
    setTimeout(() => {
        showOnboardingTip();
    }, 1000);
}

/**
 * Show onboarding tip
 */
function showOnboardingTip() {
    const tip = document.createElement('div');
    tip.className = 'onboarding-tip';
    tip.innerHTML = `
        <div class="tip-content">
            <div class="tip-icon">💡</div>
            <div class="tip-text">
                <strong>Pro Tip:</strong> Click any command to see example output and copy it!
            </div>
            <button class="tip-close" onclick="this.parentElement.parentElement.remove(); localStorage.setItem('osvm_onboarding_seen', 'true');">
                Got it!
            </button>
        </div>
    `;

    document.body.appendChild(tip);

    requestAnimationFrame(() => {
        tip.classList.add('show');
    });
}

// ===== INITIALIZATION =====

// Initialize when DOM is ready
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', () => {
        initExpandableCommands();
        initCopyToClipboard();
        initOnboarding();
    });
} else {
    initExpandableCommands();
    initCopyToClipboard();
    initOnboarding();
}

// Export functions globally
window.copyCommandToClipboard = copyCommandToClipboard;
window.toggleCommandExample = toggleCommandExample;
window.showToast = showToast;
