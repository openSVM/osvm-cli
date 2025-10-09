// OSVM Interactive DOS Terminal Teaching System
// Simulates command execution and teaches users through interactive examples

// Command scenarios with simulated outputs
const commandScenarios = {
  // Basic Commands
  'osvm --version': {
    category: 'Getting Started',
    description: 'Check OSVM version',
    output: `OSVM CLI v0.8.3
Copyright (c) 2024 OpenSVM
Built with Rust 1.80.0`,
    delay: 100
  },

  'osvm help': {
    category: 'Getting Started',
    description: 'Show available commands',
    output: `OSVM CLI - Solana Virtual Machine Management

USAGE:
    osvm [OPTIONS] <SUBCOMMAND>

OPTIONS:
    -C, --config <PATH>    Configuration file path
    -v, --verbose         Verbose output
    --no-color           Disable colored output
    -h, --help           Print help information

SUBCOMMANDS:
    balance      Show SOL balance
    svm          Manage Solana Virtual Machines
    nodes        Manage validator nodes
    deploy       Deploy programs to SVM
    audit        Security audit for programs
    mcp          Model Context Protocol servers
    chat         AI-powered chat interface
    doctor       System diagnostics`,
    delay: 200
  },

  // MicroVM Commands
  'osvm chat --microvm': {
    category: 'MicroVM Isolation',
    description: 'Launch chat in isolated microVM',
    output: `🚀 Starting MicroVM-enabled Agent Chat
📦 Main chat will run in a persistent microVM
🔒 All MCP tools will execute in ephemeral microVMs

⏳ Starting main chat microVM...
[████████████████████████] 100%
✅ Chat microVM started successfully

🔐 Ephemeral microVMs enabled for all tool executions
💬 Chat interface is now running in an isolated microVM

Type 'exit' or 'quit' to leave the chat
Type '/help' for available commands

> _`,
    delay: 1500,
    animated: true
  },

  'osvm mcp microvm launch my-server': {
    category: 'MicroVM Isolation',
    description: 'Launch MCP server in microVM',
    output: `Launching MCP server in microVM: my-server
Creating microVM configuration...
Allocating resources: 512MB RAM, 1 vCPU
Setting up vsock communication (CID: 42)
Launching Firecracker instance...

[====================] 100% Complete

✅ MicroVM launched successfully
   ID: vm-7f3a2b91
   Server: my-server
   Status: Running
   vsock: 42:5252`,
    delay: 800,
    animated: true
  },

  // SVM Management
  'osvm svm list': {
    category: 'SVM Management',
    description: 'List available SVMs',
    output: `┌─────────────┬────────────┬──────────┬─────────┐
│ NAME        │ TYPE       │ STATUS   │ VERSION │
├─────────────┼────────────┼──────────┼─────────┤
│ sonic       │ validator  │ ACTIVE   │ 1.18.22 │
│ eclipse     │ rollup     │ ACTIVE   │ 0.5.0   │
│ neon        │ evm        │ INACTIVE │ 0.13.0  │
│ nitro       │ optimistic │ ACTIVE   │ 2.1.0   │
└─────────────┴────────────┴──────────┴─────────┘

Total: 4 SVMs (3 active, 1 inactive)`,
    delay: 300
  },

  'osvm nodes deploy validator --network mainnet': {
    category: 'Node Deployment',
    description: 'Deploy validator node',
    output: `Deploying validator node to mainnet...

[Phase 1/5] Checking system requirements
  ✓ CPU: 32 cores detected
  ✓ RAM: 256GB available
  ✓ Storage: 2TB NVMe SSD
  ✓ Network: 10Gbps connection

[Phase 2/5] Installing dependencies
  ✓ Rust toolchain installed
  ✓ Solana CLI tools installed
  ✓ System libraries updated

[Phase 3/5] Configuring validator
  ✓ Identity keypair generated
  ✓ Vote account created
  ✓ Commission set to 10%

[Phase 4/5] Starting services
  ✓ Systemd service created
  ✓ Firewall rules configured
  ✓ Monitoring enabled

[Phase 5/5] Syncing with network
  ✓ Genesis block downloaded
  ✓ Snapshot restored
  ✓ Catching up to slot 250,000,000

✅ Validator successfully deployed!
   Identity: 7xKXt...9PfB
   Vote Account: Vote11...1111
   Status: Catching up (95%)`,
    delay: 2000,
    animated: true
  },

  // AI and Chat
  'osvm chat': {
    category: 'AI Assistant',
    description: 'Start AI chat interface',
    output: `┌─────────────────────────────────────────┐
│        OSVM AI Chat Assistant           │
│      Press F1 for help, ESC to exit     │
└─────────────────────────────────────────┘

Welcome to OSVM Chat! I can help you with:
• Solana development and deployment
• SVM configuration and management
• Smart contract auditing
• Performance optimization
• Troubleshooting issues

You: How do I deploy a program?

AI: To deploy a program, use:
    osvm deploy <BINARY> --program-id <ID>

Try these commands to see examples!`,
    delay: 500
  },

  // Security Audit
  'osvm audit github.com/example/program': {
    category: 'Security',
    description: 'Audit a Solana program',
    output: `🔍 Starting security audit...

Cloning repository...
[████████████████████] 100%

Analyzing Rust code...
Files scanned: 42
Lines of code: 8,531

Running security checks:
  ✓ No integer overflows detected
  ✓ No reentrancy vulnerabilities
  ✓ Proper access controls
  ⚠ Missing input validation in transfer()
  ✓ No hardcoded keys
  ✓ Secure random number generation

Severity Summary:
  🔴 Critical: 0
  🟠 High: 0
  🟡 Medium: 1
  🔵 Low: 2

Overall Score: B+ (Good)
Report saved to: audit_report.html`,
    delay: 1200,
    animated: true
  },

  // MCP Tools
  'osvm mcp list': {
    category: 'MCP Servers',
    description: 'List MCP servers',
    output: `┌──────────────┬──────────┬─────────┬──────────┐
│ SERVER       │ TYPE     │ STATUS  │ TOOLS    │
├──────────────┼──────────┼─────────┼──────────┤
│ solana-mcp   │ stdio    │ ENABLED │ 12       │
│ openai-mcp   │ http     │ ENABLED │ 8        │
│ local-tools  │ stdio    │ DISABLED│ 5        │
└──────────────┴──────────┴─────────┴──────────┘`,
    delay: 200
  },

  'osvm mcp call solana-mcp get_balance': {
    category: 'MCP Servers',
    description: 'Execute MCP tool',
    output: `🔄 Launching ephemeral microVM for tool: solana-mcp/get_balance
[====================] 100%

Executing tool in isolated environment...

Result:
{
  "address": "7xKXtg2...9PfB",
  "balance": 142.5,
  "unit": "SOL"
}

✅ Tool execution successful
🗑️ Ephemeral microVM destroyed`,
    delay: 800,
    animated: true
  }
};

// Initialize the interactive terminal
class InteractiveDOSTerminal {
  constructor() {
    this.currentOutput = null;
    this.typewriterSpeed = 10;
    this.isAnimating = false;
    this.commandHistory = [];
    this.historyIndex = -1;
  }

  init() {
    this.createUI();
    this.attachEventListeners();
    this.showWelcomeMessage();
    this.createCommandPalette();
  }

  createUI() {
    // Add interactive terminal container if it doesn't exist
    if (!document.getElementById('interactive-terminal')) {
      const container = document.createElement('div');
      container.id = 'interactive-terminal';
      container.className = 'dos-terminal-interactive';
      container.innerHTML = `
        <div class="terminal-output" id="terminal-output">
          <div class="terminal-line"></div>
        </div>
        <div class="terminal-input-area">
          <span class="terminal-prompt">C:\\OSVM> </span>
          <input type="text" id="terminal-input" class="terminal-input" autocomplete="off" spellcheck="false">
          <span class="terminal-cursor">_</span>
        </div>
        <div class="command-palette" id="command-palette"></div>
      `;

      // Find a suitable place to insert it
      const mainContent = document.querySelector('main') || document.querySelector('.content') || document.body;
      mainContent.insertBefore(container, mainContent.firstChild);
    }
  }

  createCommandPalette() {
    const palette = document.getElementById('command-palette');
    if (!palette) return;

    // Group commands by category
    const categories = {};
    Object.entries(commandScenarios).forEach(([cmd, info]) => {
      if (!categories[info.category]) {
        categories[info.category] = [];
      }
      categories[info.category].push({ command: cmd, ...info });
    });

    // Create category sections
    let paletteHTML = '<div class="palette-header">📚 Interactive Command Examples - Click to Try!</div>';

    Object.entries(categories).forEach(([category, commands]) => {
      paletteHTML += `
        <div class="command-category">
          <div class="category-header">▓▓ ${category}</div>
          <div class="command-list">
      `;

      commands.forEach(cmd => {
        paletteHTML += `
          <div class="command-item" data-command="${cmd.command}">
            <div class="command-text">
              <code>${cmd.command}</code>
              <span class="command-desc">${cmd.description}</span>
            </div>
            <div class="command-actions">
              <button class="btn-try" title="Try this command">▶</button>
              <button class="btn-copy" title="Copy to clipboard">📋</button>
            </div>
          </div>
        `;
      });

      paletteHTML += '</div></div>';
    });

    palette.innerHTML = paletteHTML;
  }

  showWelcomeMessage() {
    const output = document.getElementById('terminal-output');
    if (!output) return;

    const welcome = `╔═══════════════════════════════════════════════════════════════╗
║                  OSVM CLI - Interactive Mode                  ║
║                     DOS Terminal v1.0                         ║
╚═══════════════════════════════════════════════════════════════╝

Welcome to the OSVM Interactive Terminal!

This terminal teaches you OSVM commands through interactive examples:
  • Click any command example to see it in action
  • Copy commands to use in your real terminal
  • Watch simulated outputs to understand behavior
  • Learn by doing with zero risk

Type 'help' for available commands or click examples below.

C:\\OSVM> _`;

    output.innerHTML = `<pre>${welcome}</pre>`;
  }

  attachEventListeners() {
    // Input handling
    const input = document.getElementById('terminal-input');
    if (input) {
      input.addEventListener('keydown', (e) => this.handleInput(e));
    }

    // Command palette clicks
    document.addEventListener('click', (e) => {
      if (e.target.classList.contains('btn-try')) {
        const command = e.target.closest('.command-item').dataset.command;
        this.executeCommand(command);
      } else if (e.target.classList.contains('btn-copy')) {
        const command = e.target.closest('.command-item').dataset.command;
        this.copyToClipboard(command);
      } else if (e.target.closest('.command-item') && !e.target.closest('.command-actions')) {
        const command = e.target.closest('.command-item').dataset.command;
        this.executeCommand(command);
      }
    });
  }

  handleInput(e) {
    const input = e.target;

    if (e.key === 'Enter') {
      const command = input.value.trim();
      if (command) {
        this.executeCommand(command);
        this.commandHistory.push(command);
        this.historyIndex = this.commandHistory.length;
        input.value = '';
      }
    } else if (e.key === 'ArrowUp') {
      e.preventDefault();
      if (this.historyIndex > 0) {
        this.historyIndex--;
        input.value = this.commandHistory[this.historyIndex];
      }
    } else if (e.key === 'ArrowDown') {
      e.preventDefault();
      if (this.historyIndex < this.commandHistory.length - 1) {
        this.historyIndex++;
        input.value = this.commandHistory[this.historyIndex];
      } else {
        this.historyIndex = this.commandHistory.length;
        input.value = '';
      }
    }
  }

  executeCommand(command) {
    if (this.isAnimating) return;

    const output = document.getElementById('terminal-output');
    if (!output) return;

    // Add command to output
    const commandLine = document.createElement('div');
    commandLine.className = 'terminal-line command-input';
    commandLine.textContent = `C:\\OSVM> ${command}`;
    output.appendChild(commandLine);

    // Find matching scenario or show error
    const scenario = commandScenarios[command] || this.findPartialMatch(command);

    if (scenario) {
      this.animateOutput(scenario.output, scenario.delay || 100, scenario.animated);
    } else if (command.toLowerCase() === 'help') {
      this.showHelp();
    } else if (command.toLowerCase() === 'clear' || command.toLowerCase() === 'cls') {
      this.clearTerminal();
    } else {
      this.showError(command);
    }

    // Scroll to bottom
    output.scrollTop = output.scrollHeight;
  }

  findPartialMatch(command) {
    // Try to find a partial match for the command
    const cmd = command.toLowerCase();
    for (const [key, value] of Object.entries(commandScenarios)) {
      if (key.toLowerCase().startsWith(cmd) || cmd.startsWith(key.toLowerCase())) {
        return value;
      }
    }
    return null;
  }

  animateOutput(text, delay, animated = false) {
    const output = document.getElementById('terminal-output');
    if (!output) return;

    const outputDiv = document.createElement('div');
    outputDiv.className = 'terminal-line output';
    output.appendChild(outputDiv);

    if (animated) {
      this.isAnimating = true;
      let index = 0;
      const chars = text.split('');

      const typeInterval = setInterval(() => {
        if (index < chars.length) {
          outputDiv.textContent += chars[index];
          index++;
        } else {
          clearInterval(typeInterval);
          this.isAnimating = false;
        }
        output.scrollTop = output.scrollHeight;
      }, this.typewriterSpeed);
    } else {
      setTimeout(() => {
        outputDiv.textContent = text;
        output.scrollTop = output.scrollHeight;
      }, delay);
    }
  }

  showHelp() {
    const helpText = `
Available Commands:
═══════════════════
  help              Show this help message
  clear, cls        Clear the terminal

Click any example command in the palette below to try it!
You can also type commands directly.

Keyboard Shortcuts:
  ↑/↓              Navigate command history
  Enter            Execute command
  Ctrl+C           Copy selected text
`;
    this.animateOutput(helpText, 50);
  }

  showError(command) {
    const errorText = `'${command}' is not recognized as an internal or external command.
Type 'help' for available commands or try the examples below.`;

    const output = document.getElementById('terminal-output');
    const errorDiv = document.createElement('div');
    errorDiv.className = 'terminal-line error';
    errorDiv.textContent = errorText;
    output.appendChild(errorDiv);
  }

  clearTerminal() {
    const output = document.getElementById('terminal-output');
    if (output) {
      output.innerHTML = '<pre>C:\\OSVM> _</pre>';
    }
  }

  copyToClipboard(text) {
    navigator.clipboard.writeText(text).then(() => {
      // Show feedback
      const feedback = document.createElement('div');
      feedback.className = 'copy-feedback';
      feedback.textContent = '✓ Copied to clipboard!';
      document.body.appendChild(feedback);

      setTimeout(() => {
        feedback.classList.add('fade-out');
        setTimeout(() => feedback.remove(), 300);
      }, 1500);
    }).catch(() => {
      // Fallback for older browsers
      const textarea = document.createElement('textarea');
      textarea.value = text;
      textarea.style.position = 'fixed';
      textarea.style.opacity = '0';
      document.body.appendChild(textarea);
      textarea.select();
      document.execCommand('copy');
      document.body.removeChild(textarea);
    });
  }
}

// Initialize when DOM is ready
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', () => {
    const terminal = new InteractiveDOSTerminal();
    terminal.init();
  });
} else {
  const terminal = new InteractiveDOSTerminal();
  terminal.init();
}

// Export for use in other scripts
window.InteractiveDOSTerminal = InteractiveDOSTerminal;