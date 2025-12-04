//! User Flow and Call Graph Analysis
//!
//! This module traces user interactions and data flows through the codebase
//! to understand how the application works and identify security-critical paths.

use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet, VecDeque};
use std::path::{Path, PathBuf};

use crate::utils::debug_logger::VerbosityLevel;
use crate::{debug_print, debug_warn};

// ============================================================================
// Call Graph Analysis
// ============================================================================

/// Represents a function/method in the codebase
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct FunctionNode {
    pub name: String,
    pub file_path: PathBuf,
    pub line_number: usize,
    pub visibility: Visibility,
    pub is_async: bool,
    pub is_unsafe: bool,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Visibility {
    Public,
    PublicCrate,
    Private,
    Unknown,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Parameter {
    pub name: String,
    pub param_type: String,
    pub is_mutable: bool,
}

/// Represents a call edge in the call graph
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CallEdge {
    pub caller: String, // Function name
    pub callee: String, // Function name
    pub call_site_line: usize,
    pub is_conditional: bool, // Inside if/match/loop
}

/// Call graph representing function relationships
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CallGraph {
    pub nodes: HashMap<String, FunctionNode>,
    pub edges: Vec<CallEdge>,
    pub entry_points: Vec<String>, // main, handlers, etc.
}

impl CallGraph {
    pub fn new() -> Self {
        Self {
            nodes: HashMap::new(),
            edges: Vec::new(),
            entry_points: Vec::new(),
        }
    }

    /// Add a function node
    pub fn add_function(&mut self, func: FunctionNode) {
        self.nodes.insert(func.name.clone(), func);
    }

    /// Add a call edge
    pub fn add_call(&mut self, edge: CallEdge) {
        self.edges.push(edge);
    }

    /// Find all entry points (public functions, main, handlers)
    pub fn find_entry_points(&mut self) {
        self.entry_points = self
            .nodes
            .iter()
            .filter(|(name, node)| {
                // Entry points are:
                // 1. main function
                // 2. Public functions
                // 3. Functions with certain patterns (handle_, process_, on_)
                name == &"main"
                    || matches!(node.visibility, Visibility::Public)
                    || name.starts_with("handle_")
                    || name.starts_with("process_")
                    || name.starts_with("on_")
            })
            .map(|(name, _)| name.clone())
            .collect();

        debug_print!("🎯 Found {} entry points", self.entry_points.len());
    }

    /// Find all functions reachable from entry points
    pub fn find_reachable_functions(&self) -> HashSet<String> {
        let mut reachable = HashSet::new();
        let mut queue: VecDeque<String> = self.entry_points.iter().cloned().collect();

        while let Some(current) = queue.pop_front() {
            if reachable.contains(&current) {
                continue;
            }

            reachable.insert(current.clone());

            // Find all callees
            for edge in &self.edges {
                if edge.caller == current && !reachable.contains(&edge.callee) {
                    queue.push_back(edge.callee.clone());
                }
            }
        }

        reachable
    }

    /// Find dead code (unreachable functions)
    pub fn find_dead_code(&self) -> Vec<String> {
        let reachable = self.find_reachable_functions();
        self.nodes
            .keys()
            .filter(|name| !reachable.contains(*name))
            .cloned()
            .collect()
    }

    /// Find security-critical paths (paths that involve unsafe, network, or file I/O)
    pub fn find_critical_paths(&self) -> Vec<CriticalPath> {
        let mut paths = Vec::new();

        // Find all critical functions (unsafe, network, file I/O)
        let critical_functions: Vec<String> = self
            .nodes
            .iter()
            .filter(|(_, node)| {
                node.is_unsafe
                    || node.name.contains("network")
                    || node.name.contains("file")
                    || node.name.contains("socket")
                    || node.name.contains("http")
                    || node.name.contains("read")
                    || node.name.contains("write")
            })
            .map(|(name, _)| name.clone())
            .collect();

        // For each critical function, trace back to entry points
        for critical_func in critical_functions {
            let path = self.trace_path_to_entry(&critical_func);
            if !path.is_empty() {
                paths.push(CriticalPath {
                    entry_point: path[0].clone(),
                    critical_function: critical_func.clone(),
                    path,
                    risk_level: self.calculate_path_risk(&critical_func),
                });
            }
        }

        paths
    }

    /// Trace path from a function back to its entry points
    fn trace_path_to_entry(&self, target: &str) -> Vec<String> {
        // BFS to find shortest path to any entry point
        let mut queue: VecDeque<(String, Vec<String>)> = VecDeque::new();
        let mut visited: HashSet<String> = HashSet::new();

        queue.push_back((target.to_string(), vec![target.to_string()]));

        while let Some((current, path)) = queue.pop_front() {
            if visited.contains(&current) {
                continue;
            }
            visited.insert(current.clone());

            // Check if we reached an entry point
            if self.entry_points.contains(&current) {
                return path;
            }

            // Find all callers
            for edge in &self.edges {
                if edge.callee == current {
                    let mut new_path = path.clone();
                    new_path.insert(0, edge.caller.clone());
                    queue.push_back((edge.caller.clone(), new_path));
                }
            }
        }

        vec![] // No path found
    }

    /// Calculate risk level for a path
    fn calculate_path_risk(&self, critical_func: &str) -> RiskLevel {
        if let Some(node) = self.nodes.get(critical_func) {
            if node.is_unsafe {
                return RiskLevel::High;
            }
            if node.name.contains("network") || node.name.contains("http") {
                return RiskLevel::Medium;
            }
        }
        RiskLevel::Low
    }
}

/// A critical path from entry point to security-sensitive function
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CriticalPath {
    pub entry_point: String,
    pub critical_function: String,
    pub path: Vec<String>, // Sequence of function calls
    pub risk_level: RiskLevel,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum RiskLevel {
    Low,
    Medium,
    High,
    Critical,
}

// ============================================================================
// Data Flow Analysis
// ============================================================================

/// Represents a data flow from source to sink
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataFlow {
    pub source: DataSource,
    pub sink: DataSink,
    pub path: Vec<DataFlowNode>,
    pub is_sanitized: bool,
    pub risk_level: RiskLevel,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DataSource {
    UserInput(String),      // Function parameter, stdin, HTTP request
    FileRead(String),       // File path
    NetworkReceive(String), // Socket, HTTP
    Environment(String),    // Env var
    Database(String),       // DB query result
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DataSink {
    FileWrite(String),
    NetworkSend(String),
    CommandExecution(String),
    DatabaseQuery(String),
    LogOutput(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataFlowNode {
    pub function: String,
    pub variable: String,
    pub line: usize,
    pub transformation: Option<String>, // e.g., "sanitized", "escaped", "validated"
}

/// Data flow analyzer
pub struct DataFlowAnalyzer {
    call_graph: CallGraph,
}

impl DataFlowAnalyzer {
    pub fn new(call_graph: CallGraph) -> Self {
        Self { call_graph }
    }

    /// Find all data flows from sources to sinks
    pub fn analyze_flows(&self) -> Vec<DataFlow> {
        let mut flows = Vec::new();

        // TODO: Implement actual data flow analysis
        // This is a complex analysis that requires:
        // 1. Identifying data sources (user input, file reads, etc.)
        // 2. Tracking data transformations through function calls
        // 3. Identifying data sinks (file writes, command exec, etc.)
        // 4. Checking for sanitization/validation along the path

        flows
    }

    /// Check if a data flow path includes sanitization
    fn is_sanitized(&self, path: &[DataFlowNode]) -> bool {
        path.iter().any(|node| {
            node.transformation
                .as_ref()
                .map(|t| {
                    t.contains("sanitize")
                        || t.contains("escape")
                        || t.contains("validate")
                        || t.contains("filter")
                })
                .unwrap_or(false)
        })
    }
}

// ============================================================================
// User Flow Tracer
// ============================================================================

/// Represents a user interaction flow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UserFlow {
    pub name: String,
    pub entry_point: String,
    pub steps: Vec<UserFlowStep>,
    pub touches_sensitive_data: bool,
    pub requires_authentication: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UserFlowStep {
    pub function: String,
    pub description: String,
    pub security_relevant: bool,
}

/// User flow tracer
pub struct UserFlowTracer {
    call_graph: CallGraph,
}

impl UserFlowTracer {
    pub fn new(call_graph: CallGraph) -> Self {
        Self { call_graph }
    }

    /// Trace common user flows (e.g., login, data submission, etc.)
    pub fn trace_flows(&self) -> Vec<UserFlow> {
        let mut flows = Vec::new();

        // Find authentication flows
        if let Some(auth_flow) = self.trace_authentication_flow() {
            flows.push(auth_flow);
        }

        // Find data input flows
        flows.extend(self.trace_data_input_flows());

        // Find file operation flows
        flows.extend(self.trace_file_operation_flows());

        flows
    }

    /// Trace authentication/authorization flow
    fn trace_authentication_flow(&self) -> Option<UserFlow> {
        // Look for functions related to auth
        let auth_functions: Vec<String> = self
            .call_graph
            .nodes
            .keys()
            .filter(|name| {
                name.contains("auth")
                    || name.contains("login")
                    || name.contains("verify")
                    || name.contains("check_permission")
            })
            .cloned()
            .collect();

        if auth_functions.is_empty() {
            return None;
        }

        let steps = auth_functions
            .iter()
            .map(|func| UserFlowStep {
                function: func.clone(),
                description: format!("Authentication/authorization step: {}", func),
                security_relevant: true,
            })
            .collect();

        Some(UserFlow {
            name: "Authentication Flow".to_string(),
            entry_point: auth_functions[0].clone(),
            steps,
            touches_sensitive_data: true,
            requires_authentication: true,
        })
    }

    /// Trace data input flows (user input handling)
    fn trace_data_input_flows(&self) -> Vec<UserFlow> {
        let mut flows = Vec::new();

        // Find input handling functions
        let input_functions: Vec<String> = self
            .call_graph
            .nodes
            .keys()
            .filter(|name| {
                name.contains("input")
                    || name.contains("parse")
                    || name.contains("handle")
                    || name.contains("process")
            })
            .cloned()
            .collect();

        for func in input_functions {
            let path = self.call_graph.trace_path_to_entry(&func);
            if !path.is_empty() {
                let steps = path
                    .iter()
                    .map(|f| UserFlowStep {
                        function: f.clone(),
                        description: format!("Processing step: {}", f),
                        security_relevant: f.contains("validate") || f.contains("sanitize"),
                    })
                    .collect();

                flows.push(UserFlow {
                    name: format!("Input Flow: {}", func),
                    entry_point: path[0].clone(),
                    steps,
                    touches_sensitive_data: false,
                    requires_authentication: false,
                });
            }
        }

        flows
    }

    /// Trace file operation flows
    fn trace_file_operation_flows(&self) -> Vec<UserFlow> {
        let mut flows = Vec::new();

        // Find file operation functions
        let file_functions: Vec<String> = self
            .call_graph
            .nodes
            .keys()
            .filter(|name| {
                name.contains("file")
                    || name.contains("read")
                    || name.contains("write")
                    || name.contains("open")
            })
            .cloned()
            .collect();

        for func in file_functions {
            let path = self.call_graph.trace_path_to_entry(&func);
            if !path.is_empty() {
                let steps = path
                    .iter()
                    .map(|f| UserFlowStep {
                        function: f.clone(),
                        description: format!("File operation step: {}", f),
                        security_relevant: true,
                    })
                    .collect();

                flows.push(UserFlow {
                    name: format!("File Flow: {}", func),
                    entry_point: path[0].clone(),
                    steps,
                    touches_sensitive_data: true,
                    requires_authentication: false,
                });
            }
        }

        flows
    }
}

// ============================================================================
// Call Graph Builder (Rust-specific for now)
// ============================================================================

pub struct RustCallGraphBuilder {
    root: PathBuf,
}

impl RustCallGraphBuilder {
    pub fn new(root: PathBuf) -> Self {
        Self { root }
    }

    /// Build call graph from Rust source files
    pub fn build(&self) -> Result<CallGraph> {
        let mut graph = CallGraph::new();

        // Walk through all .rs files recursively
        self.walk_rust_files(&self.root, &mut graph)?;

        graph.find_entry_points();

        debug_print!(
            "🔍 Built call graph: {} functions, {} calls",
            graph.nodes.len(),
            graph.edges.len()
        );

        Ok(graph)
    }

    /// Walk directory recursively to find Rust files
    fn walk_rust_files(&self, dir: &Path, graph: &mut CallGraph) -> Result<()> {
        if let Ok(entries) = std::fs::read_dir(dir) {
            for entry in entries.filter_map(|e| e.ok()) {
                let path = entry.path();

                // Skip hidden and build directories
                if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                    if name.starts_with('.') || name == "target" || name == "node_modules" {
                        continue;
                    }
                }

                if path.is_file() {
                    if let Some(ext) = path.extension() {
                        if ext == "rs" {
                            self.analyze_rust_file(&path, graph)?;
                        }
                    }
                } else if path.is_dir() {
                    self.walk_rust_files(&path, graph)?;
                }
            }
        }
        Ok(())
    }

    /// Analyze a single Rust file
    fn analyze_rust_file(&self, path: &Path, graph: &mut CallGraph) -> Result<()> {
        let content = std::fs::read_to_string(path)?;

        // Simple regex-based extraction (for MVP, would use syn for proper parsing)
        let fn_pattern = regex::Regex::new(
            r"(?m)^\s*(pub(?:\([^)]+\))?\s+)?(async\s+)?(unsafe\s+)?fn\s+(\w+)\s*\(",
        )
        .unwrap();

        for cap in fn_pattern.captures_iter(&content) {
            let visibility = if cap.get(1).is_some() {
                Visibility::Public
            } else {
                Visibility::Private
            };

            let is_async = cap.get(2).is_some();
            let is_unsafe = cap.get(3).is_some();
            let name = cap.get(4).unwrap().as_str().to_string();

            // Get line number
            let line_number = content[..cap.get(0).unwrap().start()]
                .chars()
                .filter(|&c| c == '\n')
                .count()
                + 1;

            let func = FunctionNode {
                name: name.clone(),
                file_path: path.to_path_buf(),
                line_number,
                visibility,
                is_async,
                is_unsafe,
                parameters: vec![], // TODO: Parse parameters
                return_type: None,  // TODO: Parse return type
            };

            graph.add_function(func);
        }

        // TODO: Extract function calls to build edges

        Ok(())
    }
}

/// Flow analysis report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlowAnalysisReport {
    pub call_graph: CallGraph,
    pub critical_paths: Vec<CriticalPath>,
    pub user_flows: Vec<UserFlow>,
    pub data_flows: Vec<DataFlow>,
    pub dead_code: Vec<String>,
}

impl FlowAnalysisReport {
    pub fn print_summary(&self) {
        println!("\n╔══════════════════════════════════════════════════════════════╗");
        println!("║           FLOW ANALYSIS REPORT                               ║");
        println!("╚══════════════════════════════════════════════════════════════╝");
        println!();
        println!("📊 CALL GRAPH STATISTICS");
        println!("├─ Functions: {}", self.call_graph.nodes.len());
        println!("├─ Call edges: {}", self.call_graph.edges.len());
        println!("├─ Entry points: {}", self.call_graph.entry_points.len());
        println!("└─ Dead code: {} functions", self.dead_code.len());
        println!();

        if !self.critical_paths.is_empty() {
            println!("🔴 CRITICAL PATHS: {}", self.critical_paths.len());
            for path in self.critical_paths.iter().take(5) {
                println!(
                    "  • {} → {} ({:?})",
                    path.entry_point, path.critical_function, path.risk_level
                );
                println!("    Path: {}", path.path.join(" → "));
            }
            println!();
        }

        if !self.user_flows.is_empty() {
            println!("👤 USER FLOWS: {}", self.user_flows.len());
            for flow in self.user_flows.iter().take(5) {
                println!("  • {} ({} steps)", flow.name, flow.steps.len());
                if flow.touches_sensitive_data {
                    println!("    ⚠️  Touches sensitive data");
                }
            }
            println!();
        }

        if !self.dead_code.is_empty() {
            println!("💀 DEAD CODE: {} functions", self.dead_code.len());
            for func in self.dead_code.iter().take(10) {
                println!("  • {}", func);
            }
            if self.dead_code.len() > 10 {
                println!("  ... and {} more", self.dead_code.len() - 10);
            }
            println!();
        }
    }
}
