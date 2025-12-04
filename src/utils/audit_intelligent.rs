//! Intelligent Multi-Stage Audit System
//!
//! This module implements a context-aware audit system that:
//! 1. Builds a project structure treemap
//! 2. Analyzes project legitimacy (real development vs AI slop)
//! 3. Detects project type and selects appropriate analyzers
//! 4. Traces user flows and data flows
//! 5. Applies specialized security checks only when relevant
//!
//! Unlike the old system that blindly applies Solana checks to all Rust code,
//! this system is intelligent and adaptive.

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::services::ai_service::AiService;
use crate::utils::audit::{AuditFinding, AuditSeverity};
use crate::utils::debug_logger::VerbosityLevel;
use crate::{debug_print, debug_warn};

/// Maximum file size to analyze (10MB)
const MAX_FILE_SIZE: u64 = 10 * 1024 * 1024;

/// Minimum file count for a serious project
const MIN_FILES_SERIOUS_PROJECT: usize = 10;

/// Maximum percentage of boilerplate before flagging as template spam
const MAX_BOILERPLATE_PERCENTAGE: f32 = 0.7;

// ============================================================================
// STAGE 1: Project Structure Analysis
// ============================================================================

/// Represents the project structure as a tree
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectTreeMap {
    /// Root directory of the project
    pub root: PathBuf,
    /// Language distribution (extension -> count)
    pub language_distribution: HashMap<String, usize>,
    /// Directory structure (directory -> file count)
    pub directory_structure: HashMap<PathBuf, DirectoryInfo>,
    /// Total file count
    pub total_files: usize,
    /// Total lines of code (estimated)
    pub total_loc: usize,
    /// File size distribution (for detecting bloat)
    pub file_size_stats: FileSizeStats,
    /// Dependencies detected (Cargo.toml, package.json, etc.)
    pub dependencies: Vec<Dependency>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirectoryInfo {
    pub file_count: usize,
    pub subdirs: usize,
    pub depth: usize,
    pub contains_tests: bool,
    pub contains_docs: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileSizeStats {
    pub min: u64,
    pub max: u64,
    pub mean: f64,
    pub median: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dependency {
    pub name: String,
    pub version: Option<String>,
    pub source: DependencySource,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DependencySource {
    CargoToml,
    PackageJson,
    RequirementsTxt,
    GoMod,
    Unknown,
}

/// Builds a comprehensive project structure map
pub struct ProjectTreeBuilder {
    root: PathBuf,
    verbosity: VerbosityLevel,
}

impl ProjectTreeBuilder {
    pub fn new(root: PathBuf) -> Self {
        Self {
            root,
            verbosity: VerbosityLevel::Normal,
        }
    }

    pub fn with_verbosity(mut self, verbosity: VerbosityLevel) -> Self {
        self.verbosity = verbosity;
        self
    }

    /// Build the complete project tree map
    pub fn build(&self) -> Result<ProjectTreeMap> {
        debug_print!(
            "📊 Building project structure treemap from: {:?}",
            self.root
        );

        let mut language_distribution: HashMap<String, usize> = HashMap::new();
        let mut directory_structure: HashMap<PathBuf, DirectoryInfo> = HashMap::new();
        let mut file_sizes: Vec<u64> = Vec::new();
        let mut total_files = 0;
        let mut total_loc = 0;

        // Walk the directory tree recursively
        self.walk_directory(
            &self.root,
            &mut language_distribution,
            &mut directory_structure,
            &mut file_sizes,
            &mut total_files,
            &mut total_loc,
        )?;

        // Calculate file size statistics
        file_sizes.sort_unstable();
        let file_size_stats = FileSizeStats {
            min: *file_sizes.first().unwrap_or(&0),
            max: *file_sizes.last().unwrap_or(&0),
            mean: if !file_sizes.is_empty() {
                file_sizes.iter().sum::<u64>() as f64 / file_sizes.len() as f64
            } else {
                0.0
            },
            median: if !file_sizes.is_empty() {
                file_sizes[file_sizes.len() / 2]
            } else {
                0
            },
        };

        // Detect dependencies
        let dependencies = self.detect_dependencies()?;

        debug_print!(
            "✅ Project structure analyzed: {} files, {} languages, {} LOC",
            total_files,
            language_distribution.len(),
            total_loc
        );

        Ok(ProjectTreeMap {
            root: self.root.clone(),
            language_distribution,
            directory_structure,
            total_files,
            total_loc,
            file_size_stats,
            dependencies,
        })
    }

    /// Recursively walk a directory
    fn walk_directory(
        &self,
        dir: &Path,
        language_distribution: &mut HashMap<String, usize>,
        directory_structure: &mut HashMap<PathBuf, DirectoryInfo>,
        file_sizes: &mut Vec<u64>,
        total_files: &mut usize,
        total_loc: &mut usize,
    ) -> Result<()> {
        if self.should_skip(dir) {
            return Ok(());
        }

        if let Ok(entries) = std::fs::read_dir(dir) {
            for entry in entries.filter_map(|e| e.ok()) {
                let path = entry.path();

                if self.should_skip(&path) {
                    continue;
                }

                if path.is_file() {
                    *total_files += 1;

                    // Track language distribution by extension
                    if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
                        *language_distribution.entry(ext.to_string()).or_insert(0) += 1;
                    }

                    // Track file sizes
                    if let Ok(metadata) = std::fs::metadata(&path) {
                        let size = metadata.len();
                        file_sizes.push(size);

                        // Estimate LOC (rough approximation: 40 bytes per line avg)
                        if size < MAX_FILE_SIZE {
                            *total_loc += (size / 40) as usize;
                        }
                    }
                } else if path.is_dir() {
                    // Analyze directory structure
                    if let Ok(dir_info) = self.analyze_directory(&path) {
                        directory_structure.insert(path.clone(), dir_info);
                    }

                    // Recurse into subdirectory
                    self.walk_directory(
                        &path,
                        language_distribution,
                        directory_structure,
                        file_sizes,
                        total_files,
                        total_loc,
                    )?;
                }
            }
        }

        Ok(())
    }

    /// Check if a path should be skipped
    fn should_skip(&self, path: &Path) -> bool {
        let skip_patterns = [
            ".git",
            "node_modules",
            "target",
            "build",
            "dist",
            ".cache",
            "__pycache__",
            ".pytest_cache",
            "venv",
            ".venv",
            "vendor",
        ];

        path.components().any(|c| {
            c.as_os_str()
                .to_str()
                .map(|s| {
                    s.starts_with('.') && s != "."
                        || skip_patterns.iter().any(|&pattern| s == pattern)
                })
                .unwrap_or(false)
        })
    }

    /// Analyze a single directory
    fn analyze_directory(&self, dir: &Path) -> Result<DirectoryInfo> {
        let mut file_count = 0;
        let mut subdirs = 0;
        let mut contains_tests = false;
        let mut contains_docs = false;

        if let Ok(entries) = std::fs::read_dir(dir) {
            for entry in entries.filter_map(|e| e.ok()) {
                if entry
                    .file_type()
                    .ok()
                    .map(|ft| ft.is_file())
                    .unwrap_or(false)
                {
                    file_count += 1;

                    if let Some(name) = entry.file_name().to_str() {
                        if name.contains("test") || name.contains("spec") {
                            contains_tests = true;
                        }
                        if name.ends_with(".md") || name.contains("doc") {
                            contains_docs = true;
                        }
                    }
                } else {
                    subdirs += 1;
                }
            }
        }

        let depth = dir.components().count() - self.root.components().count();

        Ok(DirectoryInfo {
            file_count,
            subdirs,
            depth,
            contains_tests,
            contains_docs,
        })
    }

    /// Detect project dependencies
    fn detect_dependencies(&self) -> Result<Vec<Dependency>> {
        let mut dependencies = Vec::new();

        // Check for Cargo.toml (Rust)
        let cargo_toml = self.root.join("Cargo.toml");
        if cargo_toml.exists() {
            if let Ok(content) = std::fs::read_to_string(&cargo_toml) {
                dependencies.extend(self.parse_cargo_dependencies(&content));
            }
        }

        // Check for package.json (Node.js)
        let package_json = self.root.join("package.json");
        if package_json.exists() {
            if let Ok(content) = std::fs::read_to_string(&package_json) {
                dependencies.extend(self.parse_package_json_dependencies(&content));
            }
        }

        // Check for requirements.txt (Python)
        let requirements_txt = self.root.join("requirements.txt");
        if requirements_txt.exists() {
            if let Ok(content) = std::fs::read_to_string(&requirements_txt) {
                dependencies.extend(self.parse_requirements_txt(&content));
            }
        }

        // Check for go.mod (Go)
        let go_mod = self.root.join("go.mod");
        if go_mod.exists() {
            if let Ok(content) = std::fs::read_to_string(&go_mod) {
                dependencies.extend(self.parse_go_mod(&content));
            }
        }

        Ok(dependencies)
    }

    fn parse_cargo_dependencies(&self, content: &str) -> Vec<Dependency> {
        let mut deps = Vec::new();
        let mut in_dependencies = false;

        for line in content.lines() {
            let trimmed = line.trim();

            if trimmed.starts_with("[dependencies]") || trimmed.starts_with("[dev-dependencies]") {
                in_dependencies = true;
                continue;
            }

            if trimmed.starts_with('[') {
                in_dependencies = false;
            }

            if in_dependencies && !trimmed.is_empty() && !trimmed.starts_with('#') {
                if let Some((name, version_part)) = trimmed.split_once('=') {
                    let name = name.trim().to_string();
                    let version = version_part
                        .trim()
                        .trim_matches('"')
                        .trim_matches('\'')
                        .to_string();

                    deps.push(Dependency {
                        name,
                        version: Some(version),
                        source: DependencySource::CargoToml,
                    });
                }
            }
        }

        deps
    }

    fn parse_package_json_dependencies(&self, content: &str) -> Vec<Dependency> {
        // Simple JSON parsing for dependencies
        let mut deps = Vec::new();

        if let Ok(json) = serde_json::from_str::<serde_json::Value>(content) {
            if let Some(dependencies) = json.get("dependencies").and_then(|v| v.as_object()) {
                for (name, version) in dependencies {
                    deps.push(Dependency {
                        name: name.clone(),
                        version: version.as_str().map(|s| s.to_string()),
                        source: DependencySource::PackageJson,
                    });
                }
            }
        }

        deps
    }

    fn parse_requirements_txt(&self, content: &str) -> Vec<Dependency> {
        content
            .lines()
            .filter(|line| !line.trim().is_empty() && !line.trim().starts_with('#'))
            .map(|line| {
                let parts: Vec<&str> = line.split("==").collect();
                Dependency {
                    name: parts[0].trim().to_string(),
                    version: parts.get(1).map(|v| v.trim().to_string()),
                    source: DependencySource::RequirementsTxt,
                }
            })
            .collect()
    }

    fn parse_go_mod(&self, content: &str) -> Vec<Dependency> {
        content
            .lines()
            .filter(|line| line.trim().starts_with("require"))
            .filter_map(|line| {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 3 {
                    Some(Dependency {
                        name: parts[1].to_string(),
                        version: Some(parts[2].to_string()),
                        source: DependencySource::GoMod,
                    })
                } else {
                    None
                }
            })
            .collect()
    }
}

// ============================================================================
// STAGE 2: Project Legitimacy Scoring
// ============================================================================

/// Scores a project's legitimacy (real development vs AI-generated template spam)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LegitimacyScore {
    /// Overall score (0.0 = obvious spam, 1.0 = serious project)
    pub overall_score: f32,
    /// Individual component scores
    pub structure_score: f32,
    pub diversity_score: f32,
    pub depth_score: f32,
    pub testing_score: f32,
    pub documentation_score: f32,
    pub commit_history_score: f32,
    /// Red flags detected
    pub red_flags: Vec<String>,
    /// Green flags detected
    pub green_flags: Vec<String>,
    /// AI analysis (if available)
    pub ai_analysis: Option<String>,
}

/// Analyzes project legitimacy
pub struct LegitimacyAnalyzer {
    tree_map: ProjectTreeMap,
    ai_service: Option<AiService>,
}

impl LegitimacyAnalyzer {
    pub fn new(tree_map: ProjectTreeMap) -> Self {
        Self {
            tree_map,
            ai_service: None,
        }
    }

    pub fn with_ai(mut self, ai_service: AiService) -> Self {
        self.ai_service = Some(ai_service);
        self
    }

    /// Perform comprehensive legitimacy analysis
    pub async fn analyze(&self) -> Result<LegitimacyScore> {
        debug_print!("🔍 Analyzing project legitimacy...");

        let structure_score = self.score_project_structure();
        let diversity_score = self.score_code_diversity();
        let depth_score = self.score_implementation_depth();
        let testing_score = self.score_testing_practices();
        let documentation_score = self.score_documentation_quality();
        let commit_history_score = self.score_commit_history();

        let overall_score = (structure_score * 0.2
            + diversity_score * 0.15
            + depth_score * 0.25
            + testing_score * 0.15
            + documentation_score * 0.10
            + commit_history_score * 0.15)
            .clamp(0.0, 1.0);

        let red_flags = self.detect_red_flags();
        let green_flags = self.detect_green_flags();

        // Get AI analysis if available
        let ai_analysis = if let Some(ref ai_service) = self.ai_service {
            self.get_ai_legitimacy_analysis(ai_service).await.ok()
        } else {
            None
        };

        debug_print!(
            "✅ Legitimacy analysis complete. Overall score: {:.2}/1.00",
            overall_score
        );

        Ok(LegitimacyScore {
            overall_score,
            structure_score,
            diversity_score,
            depth_score,
            testing_score,
            documentation_score,
            commit_history_score,
            red_flags,
            green_flags,
            ai_analysis,
        })
    }

    /// Score project structure organization
    fn score_project_structure(&self) -> f32 {
        let mut score: f32 = 0.0;

        // Penalty for too few files (template spam) - STRICTER
        if self.tree_map.total_files < 5 {
            score -= 0.5; // Definitely template
        } else if self.tree_map.total_files < MIN_FILES_SERIOUS_PROJECT {
            score -= 0.2; // Suspicious
        } else if self.tree_map.total_files < 50 {
            score += 0.1; // Minimal
        } else if self.tree_map.total_files < 200 {
            score += 0.3; // Real project
        } else {
            score += 0.5; // Substantial
        }

        // Reward for organized directory structure
        let has_src = self
            .tree_map
            .directory_structure
            .keys()
            .any(|p| p.ends_with("src"));
        let has_tests = self
            .tree_map
            .directory_structure
            .values()
            .any(|d| d.contains_tests);
        let has_docs = self
            .tree_map
            .directory_structure
            .values()
            .any(|d| d.contains_docs);

        if has_src {
            score += 0.2;
        }
        if has_tests {
            score += 0.3;
        }
        if has_docs {
            score += 0.2;
        }

        // Reward for reasonable depth (not flat, not overly nested)
        let avg_depth = if !self.tree_map.directory_structure.is_empty() {
            self.tree_map
                .directory_structure
                .values()
                .map(|d| d.depth)
                .sum::<usize>() as f32
                / self.tree_map.directory_structure.len() as f32
        } else {
            0.0
        };

        if (2.0..=5.0).contains(&avg_depth) {
            score += 0.2;
        }

        score.clamp(0.0, 1.0)
    }

    /// Score code diversity (not just copy-paste boilerplate)
    fn score_code_diversity(&self) -> f32 {
        let mut score: f32 = 0.5; // Start neutral

        // Reward for multiple languages
        let lang_count = self.tree_map.language_distribution.len();
        if lang_count >= 3 {
            score += 0.3;
        } else if lang_count == 2 {
            score += 0.1;
        } else if lang_count == 1 {
            score -= 0.2; // Single language can be okay, but less diverse
        }

        // Penalty for too many generated files (Cargo.lock, package-lock.json, etc.)
        let generated_files = self
            .tree_map
            .language_distribution
            .get("lock")
            .unwrap_or(&0);
        let json_files = self
            .tree_map
            .language_distribution
            .get("json")
            .unwrap_or(&0);

        if *generated_files > self.tree_map.total_files / 10 {
            score -= 0.2;
        }
        if *json_files > self.tree_map.total_files / 5 {
            score -= 0.1;
        }

        score.clamp(0.0, 1.0)
    }

    /// Score implementation depth (actual code vs empty templates)
    fn score_implementation_depth(&self) -> f32 {
        let mut score: f32 = 0.0;

        // Check lines of code (STRICTER thresholds)
        if self.tree_map.total_loc < 100 {
            score -= 0.5; // Definitely empty template
        } else if self.tree_map.total_loc < 1000 {
            score += 0.1; // Minimal implementation
        } else if self.tree_map.total_loc < 5000 {
            score += 0.4; // Small but real
        } else if self.tree_map.total_loc < 50000 {
            score += 0.7; // Medium project
        } else {
            score += 0.9; // Large, serious project
        }

        // Check file size distribution (varied sizes = real work)
        let size_variance =
            self.tree_map.file_size_stats.max as f64 - self.tree_map.file_size_stats.min as f64;
        if size_variance > 10000.0 {
            score += 0.2; // Good variance
        }

        score.clamp(0.0, 1.0)
    }

    /// Score testing practices
    fn score_testing_practices(&self) -> f32 {
        let has_tests = self
            .tree_map
            .directory_structure
            .values()
            .any(|d| d.contains_tests);

        let test_files = *self
            .tree_map
            .language_distribution
            .get("test")
            .unwrap_or(&0)
            + *self
                .tree_map
                .language_distribution
                .get("spec")
                .unwrap_or(&0);

        if has_tests || test_files > 0 {
            1.0
        } else {
            0.0
        }
    }

    /// Score documentation quality
    fn score_documentation_quality(&self) -> f32 {
        let readme_exists = self.tree_map.root.join("README.md").exists()
            || self.tree_map.root.join("README").exists();

        let has_docs = self
            .tree_map
            .directory_structure
            .values()
            .any(|d| d.contains_docs);

        let md_count = self.tree_map.language_distribution.get("md").unwrap_or(&0);

        let mut score: f32 = 0.0;
        if readme_exists {
            score += 0.5;
        }
        if has_docs {
            score += 0.3;
        }
        if *md_count >= 3 {
            score += 0.2;
        }

        score.clamp(0.0, 1.0)
    }

    /// Score commit history (if git repo)
    fn score_commit_history(&self) -> f32 {
        // Check if .git exists
        if !self.tree_map.root.join(".git").exists() {
            return 0.5; // Neutral if not a git repo
        }

        // Try to count commits
        if let Ok(output) = std::process::Command::new("git")
            .arg("rev-list")
            .arg("--count")
            .arg("HEAD")
            .current_dir(&self.tree_map.root)
            .output()
        {
            if let Ok(count_str) = String::from_utf8(output.stdout) {
                if let Ok(commit_count) = count_str.trim().parse::<usize>() {
                    return match commit_count {
                        0..=2 => 0.1,   // Brand new or template
                        3..=10 => 0.4,  // Getting started
                        11..=50 => 0.7, // Active development
                        _ => 1.0,       // Mature project
                    };
                }
            }
        }

        0.5 // Default neutral
    }

    /// Detect red flags (signs of AI-generated spam)
    fn detect_red_flags(&self) -> Vec<String> {
        let mut flags = Vec::new();

        if self.tree_map.total_files < MIN_FILES_SERIOUS_PROJECT {
            flags.push(format!(
                "Very few files ({}) - possible template spam",
                self.tree_map.total_files
            ));
        }

        if self.tree_map.total_loc < 100 {
            flags.push(format!(
                "Very few lines of code ({}) - likely empty template",
                self.tree_map.total_loc
            ));
        }

        if self.tree_map.language_distribution.len() == 1 {
            flags.push("Single language - limited diversity".to_string());
        }

        if !self
            .tree_map
            .directory_structure
            .values()
            .any(|d| d.contains_tests)
        {
            flags.push("No tests found - quality concern".to_string());
        }

        flags
    }

    /// Detect green flags (signs of serious development)
    fn detect_green_flags(&self) -> Vec<String> {
        let mut flags = Vec::new();

        if self.tree_map.total_loc > 1000 {
            flags.push(format!(
                "Substantial codebase ({} LOC)",
                self.tree_map.total_loc
            ));
        }

        if self.tree_map.language_distribution.len() >= 3 {
            flags.push("Multi-language project - good diversity".to_string());
        }

        if self
            .tree_map
            .directory_structure
            .values()
            .any(|d| d.contains_tests)
        {
            flags.push("Tests present - quality focus".to_string());
        }

        if self
            .tree_map
            .directory_structure
            .values()
            .any(|d| d.contains_docs)
        {
            flags.push("Documentation present - professional practice".to_string());
        }

        if !self.tree_map.dependencies.is_empty() {
            flags.push(format!(
                "{} dependencies - real integration work",
                self.tree_map.dependencies.len()
            ));
        }

        flags
    }

    /// Get AI analysis of project legitimacy
    async fn get_ai_legitimacy_analysis(&self, ai_service: &AiService) -> Result<String> {
        let prompt = format!(
            r#"You are a senior code auditor and security expert analyzing repository quality.

Analyze this project structure and determine if it's a legitimate development project or AI-generated template spam:

Project Statistics:
- Total files: {}
- Total lines of code: {}
- Languages: {:?}
- Dependencies: {} detected
- Has tests: {}
- Has documentation: {}

Based on these metrics, classify this project:
1. **Serious development project** - substantial implementation, good practices
2. **Template/boilerplate** - minimal customization from starter code
3. **AI-generated spam** - no real implementation, just scaffolding

Provide a brief, direct analysis (2-3 sentences max). Focus on what the metrics reveal about actual development work vs generated code."#,
            self.tree_map.total_files,
            self.tree_map.total_loc,
            self.tree_map
                .language_distribution
                .keys()
                .collect::<Vec<_>>(),
            self.tree_map.dependencies.len(),
            self.tree_map
                .directory_structure
                .values()
                .any(|d| d.contains_tests),
            self.tree_map
                .directory_structure
                .values()
                .any(|d| d.contains_docs),
        );

        ai_service
            .query(&prompt)
            .await
            .context("AI legitimacy analysis failed")
    }
}

// ============================================================================
// STAGE 3: Project Type Detection & Analyzer Selection
// ============================================================================

/// Detected project type
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ProjectType {
    SolanaProgram,
    RustCLI,
    RustLibrary,
    WebBackend,
    WebFrontend,
    Python,
    Go,
    Mixed,
    Unknown,
}

/// Project type detector
pub struct ProjectTypeDetector {
    tree_map: ProjectTreeMap,
    root: PathBuf,
}

impl ProjectTypeDetector {
    pub fn new(tree_map: ProjectTreeMap, root: PathBuf) -> Self {
        Self { tree_map, root }
    }

    /// Detect the primary project type
    pub fn detect(&self) -> ProjectType {
        debug_print!("🔍 Detecting project type...");

        // Check for Solana program
        if self.is_solana_program() {
            debug_print!("✅ Detected: Solana Program");
            return ProjectType::SolanaProgram;
        }

        // Check language distribution
        let primary_lang = self
            .tree_map
            .language_distribution
            .iter()
            .max_by_key(|(_, count)| *count)
            .map(|(lang, _)| lang.as_str());

        match primary_lang {
            Some("rs") => {
                if self.has_binary_target() {
                    debug_print!("✅ Detected: Rust CLI");
                    ProjectType::RustCLI
                } else {
                    debug_print!("✅ Detected: Rust Library");
                    ProjectType::RustLibrary
                }
            }
            Some("py") => {
                debug_print!("✅ Detected: Python");
                ProjectType::Python
            }
            Some("go") => {
                debug_print!("✅ Detected: Go");
                ProjectType::Go
            }
            Some("js") | Some("ts") | Some("jsx") | Some("tsx") => {
                if self.is_frontend() {
                    debug_print!("✅ Detected: Web Frontend");
                    ProjectType::WebFrontend
                } else {
                    debug_print!("✅ Detected: Web Backend");
                    ProjectType::WebBackend
                }
            }
            _ => {
                if self.tree_map.language_distribution.len() > 2 {
                    debug_print!("✅ Detected: Mixed Language Project");
                    ProjectType::Mixed
                } else {
                    debug_print!("⚠️  Detected: Unknown Project Type");
                    ProjectType::Unknown
                }
            }
        }
    }

    /// Check if this is a Solana program
    fn is_solana_program(&self) -> bool {
        // Check Cargo.toml for Solana dependencies
        let cargo_toml_path = self.root.join("Cargo.toml");
        if cargo_toml_path.exists() {
            if let Ok(content) = std::fs::read_to_string(&cargo_toml_path) {
                let solana_indicators = [
                    "solana-program",
                    "anchor-lang",
                    "anchor-spl",
                    "solana-sdk",
                    "borsh",
                ];

                if solana_indicators
                    .iter()
                    .any(|&indicator| content.contains(indicator))
                {
                    return true;
                }
            }
        }

        // Check for Anchor.toml
        if self.root.join("Anchor.toml").exists() {
            return true;
        }

        // Check for program/ directory with lib.rs
        if self.root.join("programs").exists() || self.root.join("program").exists() {
            return true;
        }

        false
    }

    /// Check if this is a binary (CLI) target
    fn has_binary_target(&self) -> bool {
        // Check for src/main.rs
        if self.root.join("src").join("main.rs").exists() {
            return true;
        }

        // Check Cargo.toml for [[bin]] sections
        let cargo_toml_path = self.root.join("Cargo.toml");
        if cargo_toml_path.exists() {
            if let Ok(content) = std::fs::read_to_string(&cargo_toml_path) {
                if content.contains("[[bin]]") {
                    return true;
                }
            }
        }

        false
    }

    /// Check if this is a frontend project
    fn is_frontend(&self) -> bool {
        // Check for frontend indicators
        let frontend_files = ["index.html", "App.tsx", "App.jsx", "package.json"];

        frontend_files
            .iter()
            .any(|&file| self.root.join(file).exists())
    }
}

// ============================================================================
// STAGE 4: Intelligent Audit Coordinator
// ============================================================================

/// Intelligent audit coordinator that selects appropriate analyzers
pub struct IntelligentAuditCoordinator {
    tree_map: ProjectTreeMap,
    legitimacy_score: LegitimacyScore,
    project_type: ProjectType,
    ai_service: Option<AiService>,
}

impl IntelligentAuditCoordinator {
    pub fn new(
        tree_map: ProjectTreeMap,
        legitimacy_score: LegitimacyScore,
        project_type: ProjectType,
    ) -> Self {
        Self {
            tree_map,
            legitimacy_score,
            project_type,
            ai_service: None,
        }
    }

    pub fn with_ai(mut self, ai_service: AiService) -> Self {
        self.ai_service = Some(ai_service);
        self
    }

    /// Run comprehensive intelligent audit
    pub async fn run_audit(&self) -> Result<IntelligentAuditReport> {
        debug_print!("🔍 Starting intelligent audit...");

        let mut findings = Vec::new();

        // Add legitimacy findings
        if self.legitimacy_score.overall_score < 0.5 {
            findings.push(AuditFinding {
                id: uuid::Uuid::new_v4().to_string(),
                title: "Low Project Legitimacy Score".to_string(),
                description: format!(
                    "This project has a legitimacy score of {:.2}/1.00, indicating it may be an AI-generated template or incomplete implementation. Red flags: {}",
                    self.legitimacy_score.overall_score,
                    self.legitimacy_score.red_flags.join(", ")
                ),
                severity: if self.legitimacy_score.overall_score < 0.3 {
                    AuditSeverity::High
                } else {
                    AuditSeverity::Medium
                },
                category: "Project Quality".to_string(),
                cwe_id: None,
                cvss_score: None,
                impact: "May not be a serious development project".to_string(),
                recommendation: "Review project structure and implementation depth".to_string(),
                code_location: None,
                references: vec![],
            });
        }

        // Select appropriate analyzers based on project type
        match self.project_type {
            ProjectType::SolanaProgram => {
                debug_print!("🔍 Running Solana-specific security checks...");
                // Only NOW apply Solana checks
                findings.extend(self.run_solana_audit().await?);
            }
            ProjectType::RustCLI | ProjectType::RustLibrary => {
                debug_print!("🔍 Running Rust-specific security checks...");
                findings.extend(self.run_rust_audit().await?);
            }
            ProjectType::Python => {
                debug_print!("🔍 Running Python-specific security checks...");
                findings.extend(self.run_python_audit().await?);
            }
            ProjectType::WebBackend | ProjectType::WebFrontend => {
                debug_print!("🔍 Running web security checks...");
                findings.extend(self.run_web_audit().await?);
            }
            _ => {
                debug_print!("🔍 Running generic security checks...");
                findings.extend(self.run_generic_audit().await?);
            }
        }

        debug_print!("✅ Intelligent audit complete. {} findings", findings.len());

        Ok(IntelligentAuditReport {
            tree_map: self.tree_map.clone(),
            legitimacy_score: self.legitimacy_score.clone(),
            project_type: self.project_type.clone(),
            findings,
        })
    }

    async fn run_solana_audit(&self) -> Result<Vec<AuditFinding>> {
        // TODO: Integrate with existing SolanaSecurityCheck
        // but ONLY when we know it's a Solana project
        Ok(vec![])
    }

    async fn run_rust_audit(&self) -> Result<Vec<AuditFinding>> {
        // TODO: Rust-specific checks (unsafe usage, panic handling, etc.)
        Ok(vec![])
    }

    async fn run_python_audit(&self) -> Result<Vec<AuditFinding>> {
        // TODO: Python-specific checks (eval, exec, SQL injection, etc.)
        Ok(vec![])
    }

    async fn run_web_audit(&self) -> Result<Vec<AuditFinding>> {
        // TODO: Web security checks (XSS, CSRF, etc.)
        Ok(vec![])
    }

    async fn run_generic_audit(&self) -> Result<Vec<AuditFinding>> {
        // TODO: Generic security patterns
        Ok(vec![])
    }
}

/// Intelligent audit report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IntelligentAuditReport {
    pub tree_map: ProjectTreeMap,
    pub legitimacy_score: LegitimacyScore,
    pub project_type: ProjectType,
    pub findings: Vec<AuditFinding>,
}

impl IntelligentAuditReport {
    /// Print a human-readable summary
    pub fn print_summary(&self) {
        println!("\n╔══════════════════════════════════════════════════════════════╗");
        println!("║           INTELLIGENT AUDIT REPORT                          ║");
        println!("╚══════════════════════════════════════════════════════════════╝");
        println!();
        println!("📊 PROJECT ANALYSIS");
        println!("├─ Type: {:?}", self.project_type);
        println!("├─ Files: {}", self.tree_map.total_files);
        println!("├─ LOC: {}", self.tree_map.total_loc);
        println!(
            "└─ Languages: {:?}",
            self.tree_map.language_distribution.keys()
        );
        println!();
        println!(
            "🎯 LEGITIMACY SCORE: {:.2}/1.00",
            self.legitimacy_score.overall_score
        );
        println!("├─ Structure: {:.2}", self.legitimacy_score.structure_score);
        println!("├─ Diversity: {:.2}", self.legitimacy_score.diversity_score);
        println!("├─ Depth: {:.2}", self.legitimacy_score.depth_score);
        println!("├─ Testing: {:.2}", self.legitimacy_score.testing_score);
        println!(
            "└─ Documentation: {:.2}",
            self.legitimacy_score.documentation_score
        );
        println!();

        if !self.legitimacy_score.red_flags.is_empty() {
            println!("🚩 RED FLAGS:");
            for flag in &self.legitimacy_score.red_flags {
                println!("  • {}", flag);
            }
            println!();
        }

        if !self.legitimacy_score.green_flags.is_empty() {
            println!("✅ GREEN FLAGS:");
            for flag in &self.legitimacy_score.green_flags {
                println!("  • {}", flag);
            }
            println!();
        }

        println!("🔍 SECURITY FINDINGS: {}", self.findings.len());
        let critical = self
            .findings
            .iter()
            .filter(|f| matches!(f.severity, AuditSeverity::Critical))
            .count();
        let high = self
            .findings
            .iter()
            .filter(|f| matches!(f.severity, AuditSeverity::High))
            .count();
        let medium = self
            .findings
            .iter()
            .filter(|f| matches!(f.severity, AuditSeverity::Medium))
            .count();
        let low = self
            .findings
            .iter()
            .filter(|f| matches!(f.severity, AuditSeverity::Low))
            .count();

        println!("├─ Critical: {}", critical);
        println!("├─ High: {}", high);
        println!("├─ Medium: {}", medium);
        println!("└─ Low: {}", low);
        println!();
    }
}
