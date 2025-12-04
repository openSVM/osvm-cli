# Intelligent Multi-Stage Audit System

## 🎯 Problem Statement

The old audit system was **intellectually lazy** and **context-blind**:

- Applied Solana-specific security checks to **all Rust code**, even non-blockchain projects
- Spammed warnings like "⚠️ Unknown regex pattern requested: lamports" on generic code like `src/clparse/settings.rs`
- No understanding of project type, structure, or legitimacy
- No distinction between serious development and AI-generated template spam
- No user flow or data flow analysis

**Result:** False positives, noise, and zero insight into whether a codebase is real work or trash.

## 💡 Solution: Context-Aware Multi-Stage Analysis

We built an **intelligent audit coordinator** that thinks before it acts:

### Stage 1: Project Structure Analysis
**File:** `src/utils/audit_intelligent.rs:100-400`

```rust
let tree_builder = ProjectTreeBuilder::new(project_root);
let tree_map = tree_builder.build()?;
```

**What it does:**
- Recursively walks project directory
- Maps language distribution, file sizes, directory depth
- Detects dependencies (Cargo.toml, package.json, go.mod, requirements.txt)
- Identifies test directories and documentation
- Generates comprehensive project statistics

**Output:**
```rust
ProjectTreeMap {
    total_files: 453,
    total_loc: 47,283,
    language_distribution: {"rs": 312, "md": 15, "toml": 8, ...},
    dependencies: [solana-program, anchor-lang, ...],
    ...
}
```

### Stage 2: Legitimacy Scoring
**File:** `src/utils/audit_intelligent.rs:415-820`

```rust
let analyzer = LegitimacyAnalyzer::new(tree_map).with_ai(ai_service);
let legitimacy = analyzer.analyze().await?;
```

**Scoring Components:**
1. **Structure Score (20%)** - Organization, tests, docs, directory depth
2. **Diversity Score (15%)** - Multi-language vs single-language monorepo
3. **Depth Score (25%)** - LOC, file size variance, actual implementation
4. **Testing Score (15%)** - Presence and quantity of tests
5. **Documentation Score (10%)** - README, docs directory, markdown files
6. **Commit History Score (15%)** - Git commit frequency and maturity

**AI Enhancement:**
```rust
ai_service.query(&format!(r#"
You are a senior code auditor analyzing repository quality.

Project Statistics:
- Total files: {}
- Total LOC: {}
- Languages: {:?}
- Dependencies: {}

Classify: Serious project / Template / AI-generated spam

Provide brief analysis (2-3 sentences).
"#)).await?
```

**Output:**
```rust
LegitimacyScore {
    overall_score: 0.82,  // 0.0 = spam, 1.0 = serious
    red_flags: ["Very few files (8)", "No tests found"],
    green_flags: ["Substantial codebase (15K LOC)", "Tests present"],
    ai_analysis: Some("This appears to be a serious Solana program..."),
}
```

### Stage 3: Project Type Detection
**File:** `src/utils/audit_intelligent.rs:825-950`

```rust
let detector = ProjectTypeDetector::new(tree_map, root);
let project_type = detector.detect();
```

**Detection Logic:**
```rust
pub enum ProjectType {
    SolanaProgram,   // Has solana-program, anchor-lang, or Anchor.toml
    RustCLI,         // Has src/main.rs or [[bin]]
    RustLibrary,     // Rust without binary target
    WebBackend,      // Node.js/TS without frontend files
    WebFrontend,     // React/Vue/HTML files
    Python,
    Go,
    Mixed,
    Unknown,
}
```

**Solana Detection:**
- Checks `Cargo.toml` for `solana-program`, `anchor-lang`, `anchor-spl`, `borsh`
- Looks for `Anchor.toml`
- Checks for `programs/` or `program/` directories

### Stage 4: Intelligent Audit Execution
**File:** `src/utils/audit_intelligent.rs:955-1050`

```rust
let coordinator = IntelligentAuditCoordinator::new(tree_map, legitimacy, project_type);
let report = coordinator.run_audit().await?;
```

**Adaptive Analysis:**
```rust
match project_type {
    ProjectType::SolanaProgram => {
        // ONLY NOW apply Solana-specific checks
        findings.extend(self.run_solana_audit().await?);
    }
    ProjectType::RustCLI | ProjectType::RustLibrary => {
        findings.extend(self.run_rust_audit().await?);
    }
    ProjectType::Python => {
        findings.extend(self.run_python_audit().await?);
    }
    ...
}
```

## 🔍 Stage 5: User Flow & Call Graph Analysis
**File:** `src/utils/audit_flow_analysis.rs`

### Call Graph Building
```rust
let builder = RustCallGraphBuilder::new(project_root);
let call_graph = builder.build()?;
```

**What it extracts:**
- All function definitions (public/private, async, unsafe)
- Function parameters and return types
- Call relationships between functions
- Entry points (main, public handlers, etc.)

**Example Output:**
```rust
CallGraph {
    nodes: {
        "main": FunctionNode { visibility: Public, is_async: true, ... },
        "handle_request": FunctionNode { ... },
        "unsafe_operation": FunctionNode { is_unsafe: true, ... },
    },
    edges: [
        CallEdge { caller: "main", callee: "handle_request", line: 42 },
        CallEdge { caller: "handle_request", callee: "unsafe_operation", line: 105 },
    ],
    entry_points: ["main", "handle_api_request", ...],
}
```

### Critical Path Detection
```rust
let critical_paths = call_graph.find_critical_paths();
```

**Finds:**
- Paths from entry points to `unsafe` functions
- Paths to network/file I/O operations
- Security-critical code flows

**Example:**
```
Entry: main → handle_request → read_file → unsafe { ptr::write() }
Risk: HIGH - User input reaches unsafe pointer operation
```

### User Flow Tracing
```rust
let tracer = UserFlowTracer::new(call_graph);
let flows = tracer.trace_flows();
```

**Automatically detects:**
1. **Authentication flows** - Functions with `auth`, `login`, `verify` in name
2. **Data input flows** - Input → validation → processing → storage
3. **File operation flows** - Paths to file read/write operations

**Example Output:**
```rust
UserFlow {
    name: "Authentication Flow",
    entry_point: "handle_login",
    steps: [
        UserFlowStep { function: "handle_login", security_relevant: true },
        UserFlowStep { function: "verify_credentials", security_relevant: true },
        UserFlowStep { function: "generate_token", security_relevant: true },
    ],
    touches_sensitive_data: true,
    requires_authentication: true,
}
```

### Data Flow Analysis
```rust
let analyzer = DataFlowAnalyzer::new(call_graph);
let data_flows = analyzer.analyze_flows();
```

**Traces:**
- Sources: User input, file reads, network receives, env vars, DB queries
- Sinks: File writes, network sends, command execution, DB queries, logs
- Transformations: Sanitization, validation, escaping

**Security Implications:**
```
⚠️  Unsanitized Data Flow Detected:
Source: HTTP request parameter
Path: handle_post → parse_input → execute_query
Sink: SQL query
Risk: SQL INJECTION - No sanitization detected
```

## 📊 Complete Flow Example

```rust
// 1. Build project structure map
let tree_map = ProjectTreeBuilder::new("/tmp/my-repo").build()?;

// 2. Score legitimacy
let legitimacy = LegitimacyAnalyzer::new(tree_map.clone())
    .with_ai(ai_service)
    .analyze()
    .await?;

// 3. Detect project type
let project_type = ProjectTypeDetector::new(tree_map.clone(), "/tmp/my-repo").detect();

// 4. Run intelligent audit
let coordinator = IntelligentAuditCoordinator::new(tree_map, legitimacy, project_type);
let report = coordinator.run_audit().await?;

// 5. Build call graph for flow analysis
let call_graph = RustCallGraphBuilder::new("/tmp/my-repo").build()?;
let critical_paths = call_graph.find_critical_paths();
let user_flows = UserFlowTracer::new(call_graph.clone()).trace_flows();

// 6. Print intelligent report
report.print_summary();
```

**Output:**
```
╔══════════════════════════════════════════════════════════════╗
║           INTELLIGENT AUDIT REPORT                          ║
╚══════════════════════════════════════════════════════════════╝

📊 PROJECT ANALYSIS
├─ Type: SolanaProgram
├─ Files: 87
├─ LOC: 12,453
└─ Languages: ["rs", "toml", "md"]

🎯 LEGITIMACY SCORE: 0.85/1.00
├─ Structure: 0.90
├─ Diversity: 0.70
├─ Depth: 0.95
├─ Testing: 1.00
└─ Documentation: 0.80

✅ GREEN FLAGS:
  • Substantial codebase (12K LOC)
  • Tests present - quality focus
  • Documentation present - professional practice
  • 23 dependencies - real integration work

🔍 SECURITY FINDINGS: 12
├─ Critical: 0
├─ High: 2
├─ Medium: 5
└─ Low: 5

🔴 CRITICAL PATHS: 3
  • main → process_instruction → unsafe_deserialize (HIGH)
    Path: main → handle_transaction → process_instruction → unsafe_deserialize
  • handle_api → execute_command (MEDIUM)
    Path: handle_api → parse_input → execute_command

👤 USER FLOWS: 5
  • Authentication Flow (3 steps)
    ⚠️  Touches sensitive data
  • Token Transfer Flow (7 steps)
  • Account Creation Flow (4 steps)
```

## 🚀 Key Advantages

### 1. Context-Aware
**Before:** "⚠️ Unknown regex pattern: lamports" on `src/clparse/settings.rs`
**After:** "This is a Rust CLI tool, skipping Solana-specific checks"

### 2. Legitimacy Detection
**Before:** Audits AI template spam the same as serious projects
**After:** "Legitimacy score: 0.12/1.00 - AI-generated template with no real implementation"

### 3. Flow Tracing
**Before:** Pattern matching only
**After:** "User input from `handle_post` reaches `execute_sql` without sanitization (5-hop path traced)"

### 4. Adaptive Analysis
**Before:** One-size-fits-all security checks
**After:** Solana checks for Solana, Python checks for Python, generic checks for unknown

## 📁 File Structure

```
src/utils/
├── audit_intelligent.rs     # Stage 1-4: Project analysis & intelligent coordination
├── audit_flow_analysis.rs   # Stage 5: Call graphs, user flows, data flows
├── audit_modular.rs          # Legacy modular checks (still used for specialized scans)
└── audit.rs                  # Original audit system (deprecated)
```

## 🔧 Integration Points

### With Existing Audit System
```rust
// Old way (blind Solana checks on everything)
let coordinator = ModularAuditCoordinator::new();  // Always includes SolanaSecurityCheck
coordinator.analyze_file("settings.rs")?;  // ❌ Applies Solana checks to CLI code

// New way (intelligent analysis first)
let intelligent = IntelligentAuditCoordinator::new(tree_map, legitimacy, project_type);
intelligent.run_audit().await?;  // ✅ Only Solana checks if detected as Solana project
```

### With AI Service
```rust
// Legitimacy analysis with AI
let ai_service = AiService::new();
let legitimacy = LegitimacyAnalyzer::new(tree_map)
    .with_ai(ai_service)  // Optional AI enhancement
    .analyze()
    .await?;
```

## 🎓 Technical Insights

### Why This Architecture?

**Separation of Concerns:**
- **Analysis** (understand) → **Judgment** (score) → **Detection** (classify) → **Execution** (audit)
- Each stage can be tested and improved independently

**Composability:**
- Can use legitimacy scoring without full audit
- Can use call graph analysis standalone
- Can mix and match analyzers based on project type

**Extensibility:**
- Add new `ProjectType` variants easily
- Plugin new language-specific analyzers
- Extend flow analysis to other languages (Python AST, Go parser, etc.)

### Performance Considerations

**Tree Building:** O(n) where n = number of files
**Legitimacy Scoring:** O(1) with cached regex patterns
**Call Graph:** O(n*m) where n = functions, m = avg calls per function
**Flow Tracing:** BFS O(V+E) where V = functions, E = call edges

**Optimization:** Use `--skip-flow-analysis` flag to skip call graph for very large codebases.

## 🔮 Future Enhancements

### 1. Language Support
- Python AST parsing for accurate call graphs
- JavaScript/TypeScript with tree-sitter
- Go call graph with `go/analysis`

### 2. Advanced Flow Analysis
- Taint analysis for tracking user input through transformations
- Symbolic execution for path feasibility
- Inter-procedural data flow with points-to analysis

### 3. ML-Based Legitimacy
- Train classifier on real vs template repos
- Code quality metrics (Halstead, cyclomatic complexity)
- Commit pattern analysis (time of day, message quality)

### 4. Integration
- GitHub Actions integration for PR audits
- VS Code extension for real-time legitimacy feedback
- CI/CD pipeline with legitimacy gates

## 📚 References

- Call Graph Construction: [https://en.wikipedia.org/wiki/Call_graph](https://en.wikipedia.org/wiki/Call_graph)
- Data Flow Analysis: [https://en.wikipedia.org/wiki/Data-flow_analysis](https://en.wikipedia.org/wiki/Data-flow_analysis)
- Taint Analysis: [https://en.wikipedia.org/wiki/Taint_checking](https://en.wikipedia.org/wiki/Taint_checking)
- Solana Security Best Practices: [https://book.anchor-lang.com/anchor_bts/security.html](https://book.anchor-lang.com/anchor_bts/security.html)

---

**Bottom Line:** This system is the difference between a **script** (blindly applying rules) and an **intelligent agent** (understanding context, adapting behavior, providing insight).
