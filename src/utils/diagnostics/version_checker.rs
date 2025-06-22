//! Version checking and compatibility utilities
//!
//! This module provides version comparison and compatibility checking
//! for system dependencies and tools.

use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt;
use std::str::FromStr;

/// Version comparison result
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum VersionComparison {
    /// Current version is newer than required
    Newer,
    /// Current version matches required version
    Equal,
    /// Current version is older than required
    Older,
    /// Version could not be compared (incompatible formats)
    Incomparable,
}

/// Semantic version representation
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SemanticVersion {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
    pub pre_release: Option<String>,
    pub build: Option<String>,
}

/// Version requirement specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VersionRequirement {
    pub tool: String,
    pub minimum_version: Option<SemanticVersion>,
    pub maximum_version: Option<SemanticVersion>,
    pub exact_version: Option<SemanticVersion>,
    pub blacklisted_versions: Vec<SemanticVersion>,
}

/// Version checker for system tools
pub struct VersionChecker;

impl VersionChecker {
    /// Create a new version checker
    pub fn new() -> Self {
        Self
    }

    /// Check if a version meets the requirements
    pub fn check_version_requirement(
        &self,
        current: &str,
        requirement: &VersionRequirement,
    ) -> Result<bool, VersionError> {
        let current_version = SemanticVersion::from_str(current)?;

        // Check exact version requirement
        if let Some(ref exact) = requirement.exact_version {
            return Ok(current_version == *exact);
        }

        // Check blacklisted versions
        if requirement.blacklisted_versions.contains(&current_version) {
            return Ok(false);
        }

        // Check minimum version
        if let Some(ref min_version) = requirement.minimum_version {
            if current_version < *min_version {
                return Ok(false);
            }
        }

        // Check maximum version
        if let Some(ref max_version) = requirement.maximum_version {
            if current_version > *max_version {
                return Ok(false);
            }
        }

        Ok(true)
    }

    /// Compare two version strings
    pub fn compare_versions(&self, version1: &str, version2: &str) -> VersionComparison {
        match (
            SemanticVersion::from_str(version1),
            SemanticVersion::from_str(version2),
        ) {
            (Ok(v1), Ok(v2)) => match v1.cmp(&v2) {
                Ordering::Greater => VersionComparison::Newer,
                Ordering::Equal => VersionComparison::Equal,
                Ordering::Less => VersionComparison::Older,
            },
            _ => VersionComparison::Incomparable,
        }
    }

    /// Check if a version is compatible with a range
    pub fn is_version_compatible(
        &self,
        version: &str,
        min_version: &str,
        max_version: Option<&str>,
    ) -> bool {
        let version_obj = match SemanticVersion::from_str(version) {
            Ok(v) => v,
            Err(_) => return false,
        };

        let min_obj = match SemanticVersion::from_str(min_version) {
            Ok(v) => v,
            Err(_) => return false,
        };

        if version_obj < min_obj {
            return false;
        }

        if let Some(max_ver) = max_version {
            let max_obj = match SemanticVersion::from_str(max_ver) {
                Ok(v) => v,
                Err(_) => return false,
            };
            if version_obj > max_obj {
                return false;
            }
        }

        true
    }

    /// Get recommended versions for common tools
    pub fn get_recommended_versions(&self) -> Vec<VersionRequirement> {
        vec![
            VersionRequirement {
                tool: "rust".to_string(),
                minimum_version: Some(SemanticVersion::from_str("1.70.0").unwrap()),
                maximum_version: None,
                exact_version: None,
                blacklisted_versions: vec![],
            },
            VersionRequirement {
                tool: "solana".to_string(),
                minimum_version: Some(SemanticVersion::from_str("1.16.0").unwrap()),
                maximum_version: None,
                exact_version: None,
                blacklisted_versions: vec![],
            },
            VersionRequirement {
                tool: "node".to_string(),
                minimum_version: Some(SemanticVersion::from_str("16.0.0").unwrap()),
                maximum_version: None,
                exact_version: None,
                blacklisted_versions: vec![],
            },
        ]
    }

    /// Extract version from command output
    pub fn extract_version_from_output(&self, output: &str, tool: &str) -> Option<String> {
        // Common patterns for version extraction
        let patterns = match tool.to_lowercase().as_str() {
            "rust" | "rustc" => vec![r"rustc (\d+\.\d+\.\d+)", r"(\d+\.\d+\.\d+)"],
            "solana" => vec![r"solana-cli (\d+\.\d+\.\d+)", r"(\d+\.\d+\.\d+)"],
            "node" => vec![r"v(\d+\.\d+\.\d+)", r"(\d+\.\d+\.\d+)"],
            "git" => vec![r"git version (\d+\.\d+\.\d+)", r"(\d+\.\d+\.\d+)"],
            _ => vec![r"(\d+\.\d+\.\d+)"],
        };

        for pattern in patterns {
            if let Ok(regex) = regex::Regex::new(pattern) {
                if let Some(captures) = regex.captures(output) {
                    if let Some(version_match) = captures.get(1) {
                        return Some(version_match.as_str().to_string());
                    }
                }
            }
        }

        None
    }

    /// Check if a tool needs an update
    pub fn needs_update(&self, current_version: &str, latest_version: &str) -> bool {
        matches!(
            self.compare_versions(current_version, latest_version),
            VersionComparison::Older
        )
    }

    /// Get severity of version mismatch
    pub fn get_version_mismatch_severity(
        &self,
        current: &str,
        required: &str,
    ) -> VersionMismatchSeverity {
        match self.compare_versions(current, required) {
            VersionComparison::Older => {
                // Check how far behind we are
                if let (Ok(curr), Ok(req)) = (
                    SemanticVersion::from_str(current),
                    SemanticVersion::from_str(required),
                ) {
                    if curr.major < req.major {
                        VersionMismatchSeverity::Critical
                    } else if curr.minor < req.minor {
                        VersionMismatchSeverity::Major
                    } else {
                        VersionMismatchSeverity::Minor
                    }
                } else {
                    VersionMismatchSeverity::Unknown
                }
            }
            VersionComparison::Newer => VersionMismatchSeverity::Acceptable,
            VersionComparison::Equal => VersionMismatchSeverity::None,
            VersionComparison::Incomparable => VersionMismatchSeverity::Unknown,
        }
    }
}

/// Version mismatch severity levels
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum VersionMismatchSeverity {
    None,
    Acceptable,
    Minor,
    Major,
    Critical,
    Unknown,
}

/// Version parsing and comparison errors
#[derive(Debug, Clone)]
pub enum VersionError {
    InvalidFormat(String),
    ParseError(String),
    ComparisonError(String),
}

impl fmt::Display for VersionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VersionError::InvalidFormat(msg) => write!(f, "Invalid version format: {}", msg),
            VersionError::ParseError(msg) => write!(f, "Version parse error: {}", msg),
            VersionError::ComparisonError(msg) => write!(f, "Version comparison error: {}", msg),
        }
    }
}

impl std::error::Error for VersionError {}

impl FromStr for SemanticVersion {
    type Err = VersionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Remove common prefixes
        let cleaned = s.trim_start_matches('v').trim();

        // Split on '+' to separate build metadata
        let (version_part, build) = if let Some(plus_pos) = cleaned.find('+') {
            let (ver, build_part) = cleaned.split_at(plus_pos);
            (ver, Some(build_part[1..].to_string()))
        } else {
            (cleaned, None)
        };

        // Split on '-' to separate pre-release
        let (core_version, pre_release) = if let Some(dash_pos) = version_part.find('-') {
            let (core, pre) = version_part.split_at(dash_pos);
            (core, Some(pre[1..].to_string()))
        } else {
            (version_part, None)
        };

        // Parse major.minor.patch
        let parts: Vec<&str> = core_version.split('.').collect();
        if parts.len() < 2 {
            return Err(VersionError::InvalidFormat(format!(
                "Version must have at least major.minor: {}",
                s
            )));
        }

        let major = parts[0].parse::<u32>().map_err(|_| {
            VersionError::ParseError(format!("Invalid major version: {}", parts[0]))
        })?;

        let minor = parts[1].parse::<u32>().map_err(|_| {
            VersionError::ParseError(format!("Invalid minor version: {}", parts[1]))
        })?;

        let patch = if parts.len() >= 3 {
            parts[2].parse::<u32>().map_err(|_| {
                VersionError::ParseError(format!("Invalid patch version: {}", parts[2]))
            })?
        } else {
            0
        };

        Ok(SemanticVersion {
            major,
            minor,
            patch,
            pre_release,
            build,
        })
    }
}

impl fmt::Display for SemanticVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)?;

        if let Some(ref pre) = self.pre_release {
            write!(f, "-{}", pre)?;
        }

        if let Some(ref build) = self.build {
            write!(f, "+{}", build)?;
        }

        Ok(())
    }
}

impl PartialOrd for SemanticVersion {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SemanticVersion {
    fn cmp(&self, other: &Self) -> Ordering {
        // Compare major, minor, patch
        match self.major.cmp(&other.major) {
            Ordering::Equal => {}
            other => return other,
        }

        match self.minor.cmp(&other.minor) {
            Ordering::Equal => {}
            other => return other,
        }

        match self.patch.cmp(&other.patch) {
            Ordering::Equal => {}
            other => return other,
        }

        // Handle pre-release versions
        match (&self.pre_release, &other.pre_release) {
            (None, None) => Ordering::Equal,
            (Some(_), None) => Ordering::Less, // Pre-release < release
            (None, Some(_)) => Ordering::Greater, // Release > pre-release
            (Some(a), Some(b)) => a.cmp(b),    // Compare pre-release strings
        }
    }
}

impl Default for VersionChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_semantic_version_parsing() {
        let version = SemanticVersion::from_str("1.2.3").unwrap();
        assert_eq!(version.major, 1);
        assert_eq!(version.minor, 2);
        assert_eq!(version.patch, 3);
        assert!(version.pre_release.is_none());
        assert!(version.build.is_none());

        let pre_version = SemanticVersion::from_str("1.2.3-alpha.1").unwrap();
        assert_eq!(pre_version.major, 1);
        assert_eq!(pre_version.minor, 2);
        assert_eq!(pre_version.patch, 3);
        assert_eq!(pre_version.pre_release, Some("alpha.1".to_string()));

        let build_version = SemanticVersion::from_str("1.2.3+build.123").unwrap();
        assert_eq!(build_version.build, Some("build.123".to_string()));

        let v_prefix = SemanticVersion::from_str("v1.2.3").unwrap();
        assert_eq!(v_prefix.major, 1);
    }

    #[test]
    fn test_version_comparison() {
        let v1 = SemanticVersion::from_str("1.2.3").unwrap();
        let v2 = SemanticVersion::from_str("1.2.4").unwrap();
        let v3 = SemanticVersion::from_str("1.3.0").unwrap();

        assert!(v1 < v2);
        assert!(v2 < v3);
        assert!(v1 < v3);

        let pre_version = SemanticVersion::from_str("1.2.3-alpha").unwrap();
        let release_version = SemanticVersion::from_str("1.2.3").unwrap();
        assert!(pre_version < release_version);
    }

    #[test]
    fn test_version_checker() {
        let checker = VersionChecker::new();

        let comparison = checker.compare_versions("1.2.3", "1.2.4");
        assert_eq!(comparison, VersionComparison::Older);

        let comparison = checker.compare_versions("1.3.0", "1.2.4");
        assert_eq!(comparison, VersionComparison::Newer);

        let comparison = checker.compare_versions("1.2.3", "1.2.3");
        assert_eq!(comparison, VersionComparison::Equal);
    }

    #[test]
    fn test_version_compatibility() {
        let checker = VersionChecker::new();

        assert!(checker.is_version_compatible("1.5.0", "1.0.0", Some("2.0.0")));
        assert!(!checker.is_version_compatible("0.9.0", "1.0.0", Some("2.0.0")));
        assert!(!checker.is_version_compatible("2.1.0", "1.0.0", Some("2.0.0")));
        assert!(checker.is_version_compatible("1.5.0", "1.0.0", None));
    }

    #[test]
    fn test_needs_update() {
        let checker = VersionChecker::new();

        assert!(checker.needs_update("1.2.3", "1.2.4"));
        assert!(!checker.needs_update("1.2.4", "1.2.3"));
        assert!(!checker.needs_update("1.2.3", "1.2.3"));
    }

    #[test]
    fn test_version_mismatch_severity() {
        let checker = VersionChecker::new();

        let severity = checker.get_version_mismatch_severity("1.0.0", "2.0.0");
        assert_eq!(severity, VersionMismatchSeverity::Critical);

        let severity = checker.get_version_mismatch_severity("1.1.0", "1.2.0");
        assert_eq!(severity, VersionMismatchSeverity::Major);

        let severity = checker.get_version_mismatch_severity("1.1.1", "1.1.2");
        assert_eq!(severity, VersionMismatchSeverity::Minor);

        let severity = checker.get_version_mismatch_severity("1.2.3", "1.2.3");
        assert_eq!(severity, VersionMismatchSeverity::None);
    }

    #[test]
    fn test_version_requirement() {
        let checker = VersionChecker::new();
        let requirement = VersionRequirement {
            tool: "rust".to_string(),
            minimum_version: Some(SemanticVersion::from_str("1.70.0").unwrap()),
            maximum_version: None,
            exact_version: None,
            blacklisted_versions: vec![],
        };

        assert!(checker
            .check_version_requirement("1.75.0", &requirement)
            .unwrap());
        assert!(!checker
            .check_version_requirement("1.69.0", &requirement)
            .unwrap());
    }
}
