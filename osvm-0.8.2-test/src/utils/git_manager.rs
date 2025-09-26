//! Enhanced Git Repository Management with Dynamic Branch Detection
//!
//! This module provides sophisticated branch detection and repository cloning
//! capabilities that go beyond simple hardcoded branch names.

use anyhow::{Context, Result};
use std::path::Path;
use std::process::Command;

/// Git repository manager with intelligent branch detection
pub struct GitRepositoryManager {
    pub timeout_seconds: u64,
    pub custom_branches: Vec<String>,
    pub enable_dynamic_probing: bool,
}

/// Branch detection result
#[derive(Debug, Clone)]
pub struct BranchInfo {
    pub name: String,
    pub commit_hash: String,
    pub is_default: bool,
    pub last_commit_date: String,
}

/// Repository metadata
#[derive(Debug, Clone)]
pub struct RepositoryMetadata {
    pub default_branch: String,
    pub available_branches: Vec<BranchInfo>,
    pub clone_url: String,
    pub last_activity: String,
}

impl GitRepositoryManager {
    pub fn new() -> Self {
        Self {
            timeout_seconds: 60,
            custom_branches: Vec::new(),
            enable_dynamic_probing: true,
        }
    }

    /// Create a new manager with custom branch preferences
    pub fn with_custom_branches(branches: Vec<String>) -> Self {
        Self {
            timeout_seconds: 60,
            custom_branches: branches,
            enable_dynamic_probing: true,
        }
    }

    /// Detect available branches from remote repository
    pub async fn detect_remote_branches(&self, repo_url: &str) -> Result<Vec<BranchInfo>> {
        println!("🔍 Probing remote branches for: {}", repo_url);

        let output = Command::new("git")
            .args(&["ls-remote", "--heads", repo_url])
            .output()
            .context("Failed to execute git ls-remote")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!("Failed to list remote branches: {}", stderr);
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        let mut branches = Vec::new();

        for line in stdout.lines() {
            if let Some((commit_hash, ref_name)) = line.split_once('\t') {
                if let Some(branch_name) = ref_name.strip_prefix("refs/heads/") {
                    branches.push(BranchInfo {
                        name: branch_name.to_string(),
                        commit_hash: commit_hash.to_string(),
                        is_default: false, // Will be determined later
                        last_commit_date: "Unknown".to_string(),
                    });
                }
            }
        }

        println!(
            "📋 Found {} remote branches: {:?}",
            branches.len(),
            branches.iter().map(|b| &b.name).collect::<Vec<_>>()
        );

        Ok(branches)
    }

    /// Get the default branch of the remote repository
    pub async fn get_default_branch(&self, repo_url: &str) -> Result<String> {
        println!("🎯 Detecting default branch for: {}", repo_url);

        let output = Command::new("git")
            .args(&["ls-remote", "--symref", repo_url, "HEAD"])
            .output()
            .context("Failed to execute git ls-remote")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            println!("⚠️  Could not detect default branch: {}", stderr);
            return Ok("main".to_string()); // Fallback to main
        }

        let stdout = String::from_utf8_lossy(&output.stdout);

        for line in stdout.lines() {
            if line.starts_with("ref: refs/heads/") {
                if let Some(branch_name) = line.strip_prefix("ref: refs/heads/") {
                    if let Some(branch) = branch_name.split('\t').next() {
                        println!("✅ Detected default branch: {}", branch);
                        return Ok(branch.to_string());
                    }
                }
            }
        }

        println!("⚠️  Could not parse default branch, using 'main'");
        Ok("main".to_string())
    }

    /// Get comprehensive repository metadata
    pub async fn get_repository_metadata(&self, repo_url: &str) -> Result<RepositoryMetadata> {
        let default_branch = self.get_default_branch(repo_url).await?;
        let mut available_branches = self.detect_remote_branches(repo_url).await?;

        // Mark the default branch
        for branch in &mut available_branches {
            if branch.name == default_branch {
                branch.is_default = true;
            }
        }

        Ok(RepositoryMetadata {
            default_branch,
            available_branches,
            clone_url: repo_url.to_string(),
            last_activity: "Unknown".to_string(),
        })
    }

    /// Generate intelligent branch priority list
    pub fn generate_branch_priority(
        &self,
        repo_metadata: &RepositoryMetadata,
        user_branch: Option<&str>,
    ) -> Vec<String> {
        let mut priority_branches = Vec::new();

        // 1. If user specified a branch, try it first
        if let Some(branch) = user_branch {
            priority_branches.push(branch.to_string());
        }

        // 2. Add custom branches from configuration
        priority_branches.extend(self.custom_branches.clone());

        // 3. Add the detected default branch
        if !priority_branches.contains(&repo_metadata.default_branch) {
            priority_branches.push(repo_metadata.default_branch.clone());
        }

        // 4. Add common branch patterns if they exist in the repo
        let common_branches = [
            "main",
            "master",
            "develop",
            "development",
            "dev",
            "trunk",
            "stable",
            "release",
            "prod",
            "production",
        ];

        for common_branch in &common_branches {
            if repo_metadata
                .available_branches
                .iter()
                .any(|b| b.name == *common_branch)
            {
                if !priority_branches.contains(&common_branch.to_string()) {
                    priority_branches.push(common_branch.to_string());
                }
            }
        }

        // 5. Add any remaining branches from the repository
        for branch in &repo_metadata.available_branches {
            if !priority_branches.contains(&branch.name) {
                priority_branches.push(branch.name.clone());
            }
        }

        priority_branches
    }

    /// Clone repository with intelligent branch selection
    pub async fn clone_with_branch_detection(
        &self,
        repo_url: &str,
        target_dir: &Path,
        preferred_branch: Option<&str>,
    ) -> Result<String> {
        // Get repository metadata first
        let repo_metadata = if self.enable_dynamic_probing {
            self.get_repository_metadata(repo_url).await?
        } else {
            // Fallback metadata for when dynamic probing is disabled
            RepositoryMetadata {
                default_branch: preferred_branch.unwrap_or("main").to_string(),
                available_branches: Vec::new(),
                clone_url: repo_url.to_string(),
                last_activity: "Unknown".to_string(),
            }
        };

        let priority_branches = self.generate_branch_priority(&repo_metadata, preferred_branch);

        println!("📋 Branch priority order: {:?}", priority_branches);

        let mut last_error = None;

        for branch in &priority_branches {
            println!("🔄 Attempting to clone branch: {}", branch);

            let result = self
                .attempt_clone_branch(repo_url, target_dir, branch)
                .await;

            match result {
                Ok(_) => {
                    println!("✅ Successfully cloned branch: {}", branch);
                    return Ok(branch.clone());
                }
                Err(e) => {
                    println!("❌ Failed to clone branch '{}': {}", branch, e);
                    last_error = Some(e);

                    // Clean up failed clone attempt
                    if target_dir.exists() {
                        let _ = std::fs::remove_dir_all(target_dir);
                    }
                }
            }
        }

        // If all branches failed, return the last error
        if let Some(error) = last_error {
            anyhow::bail!("Failed to clone any branch. Last error: {}", error);
        } else {
            anyhow::bail!("No branches available to clone");
        }
    }

    /// Attempt to clone a specific branch
    async fn attempt_clone_branch(
        &self,
        repo_url: &str,
        target_dir: &Path,
        branch: &str,
    ) -> Result<()> {
        let output = Command::new("git")
            .args(&[
                "clone",
                "--single-branch",
                "--branch",
                branch,
                "--depth",
                "1",
                repo_url,
                target_dir.to_str().unwrap(),
            ])
            .output()
            .context("Failed to execute git clone")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!("Git clone failed: {}", stderr);
        }

        Ok(())
    }

    /// Add user-defined branch override flags
    pub fn add_branch_override(&mut self, branch: String) {
        if !self.custom_branches.contains(&branch) {
            self.custom_branches.insert(0, branch); // Add to front for highest priority
        }
    }

    /// Set timeout for git operations
    pub fn set_timeout(&mut self, seconds: u64) {
        self.timeout_seconds = seconds;
    }

    /// Enable or disable dynamic branch probing
    pub fn set_dynamic_probing(&mut self, enabled: bool) {
        self.enable_dynamic_probing = enabled;
    }
}

impl Default for GitRepositoryManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_branch_priority_generation() {
        let manager = GitRepositoryManager::new();

        let repo_metadata = RepositoryMetadata {
            default_branch: "develop".to_string(),
            available_branches: vec![
                BranchInfo {
                    name: "main".to_string(),
                    commit_hash: "abc123".to_string(),
                    is_default: false,
                    last_commit_date: "2024-01-01".to_string(),
                },
                BranchInfo {
                    name: "develop".to_string(),
                    commit_hash: "def456".to_string(),
                    is_default: true,
                    last_commit_date: "2024-01-02".to_string(),
                },
            ],
            clone_url: "https://github.com/test/repo".to_string(),
            last_activity: "2024-01-02".to_string(),
        };

        let priorities = manager.generate_branch_priority(&repo_metadata, Some("feature-branch"));

        assert_eq!(priorities[0], "feature-branch"); // User branch first
        assert!(priorities.contains(&"develop".to_string())); // Default branch included
        assert!(priorities.contains(&"main".to_string())); // Common branch included
    }

    #[test]
    fn test_custom_branches() {
        let manager = GitRepositoryManager::with_custom_branches(vec![
            "custom-main".to_string(),
            "custom-dev".to_string(),
        ]);

        let repo_metadata = RepositoryMetadata {
            default_branch: "master".to_string(),
            available_branches: vec![],
            clone_url: "https://github.com/test/repo".to_string(),
            last_activity: "Unknown".to_string(),
        };

        let priorities = manager.generate_branch_priority(&repo_metadata, None);

        assert!(priorities.contains(&"custom-main".to_string()));
        assert!(priorities.contains(&"custom-dev".to_string()));
        assert!(priorities.contains(&"master".to_string()));
    }
}
