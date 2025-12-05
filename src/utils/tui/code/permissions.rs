//! Permission management for tool execution
//!
//! Handles approval flows for tool calls, with support for:
//! - Auto-approved tools (read, search)
//! - Session-level "always approve" settings
//! - Per-invocation approval decisions

use super::tools::{RiskLevel, Tool};
use serde_json::Value;
use std::collections::HashSet;

/// Manages tool permissions and approval state
#[derive(Debug, Clone)]
pub struct PermissionManager {
    /// Tools that are always auto-approved (safe operations)
    auto_approved_tools: HashSet<String>,
    /// Tools the user has approved for this session ("always approve")
    session_approved_tools: HashSet<String>,
    /// Specific tool+params combinations approved this session
    approved_invocations: HashSet<String>,
}

impl Default for PermissionManager {
    fn default() -> Self {
        Self::new()
    }
}

impl PermissionManager {
    /// Create a new permission manager with default safe tools
    pub fn new() -> Self {
        let mut auto_approved = HashSet::new();

        // Read-only operations are always safe
        auto_approved.insert("read".to_string());
        auto_approved.insert("glob".to_string());
        auto_approved.insert("grep".to_string());

        Self {
            auto_approved_tools: auto_approved,
            session_approved_tools: HashSet::new(),
            approved_invocations: HashSet::new(),
        }
    }

    /// Check if a tool invocation needs user approval
    pub fn needs_approval(&self, tool: &dyn Tool, params: &Value) -> bool {
        let tool_name = tool.name();

        // Check if tool is globally auto-approved
        if self.auto_approved_tools.contains(tool_name) {
            return false;
        }

        // Check if user has approved this tool for the session
        if self.session_approved_tools.contains(tool_name) {
            return false;
        }

        // Check tool's own approval logic
        tool.requires_approval(params)
    }

    /// Mark a tool as approved for this session
    pub fn approve_tool_for_session(&mut self, tool_name: &str) {
        self.session_approved_tools.insert(tool_name.to_string());
    }

    /// Approve a specific invocation (one-time)
    pub fn approve_invocation(&mut self, tool_name: &str, params: &Value) {
        let key = format!("{}:{}", tool_name, params);
        self.approved_invocations.insert(key);
    }

    /// Check if a specific invocation was already approved
    pub fn is_invocation_approved(&self, tool_name: &str, params: &Value) -> bool {
        let key = format!("{}:{}", tool_name, params);
        self.approved_invocations.contains(&key)
    }

    /// Get the approval status description for a tool
    pub fn approval_status(&self, tool: &dyn Tool) -> ApprovalStatus {
        let tool_name = tool.name();

        if self.auto_approved_tools.contains(tool_name) {
            ApprovalStatus::AutoApproved("Safe operation")
        } else if self.session_approved_tools.contains(tool_name) {
            ApprovalStatus::SessionApproved
        } else {
            match tool.risk_level() {
                RiskLevel::Safe => ApprovalStatus::AutoApproved("Low risk"),
                RiskLevel::Low => ApprovalStatus::RequiresApproval("Modifies files"),
                RiskLevel::Medium => ApprovalStatus::RequiresApproval("Executes commands"),
                RiskLevel::High => ApprovalStatus::RequiresApproval("High risk operation"),
            }
        }
    }

    /// Reset session approvals (e.g., when changing projects)
    pub fn reset_session(&mut self) {
        self.session_approved_tools.clear();
        self.approved_invocations.clear();
    }

    /// List all session-approved tools
    pub fn session_approved(&self) -> Vec<&str> {
        self.session_approved_tools.iter().map(|s| s.as_str()).collect()
    }
}

/// Status of approval for a tool
#[derive(Debug, Clone)]
pub enum ApprovalStatus {
    /// Tool is auto-approved (with reason)
    AutoApproved(&'static str),
    /// Tool was approved for this session
    SessionApproved,
    /// Tool requires per-invocation approval (with reason)
    RequiresApproval(&'static str),
}

impl ApprovalStatus {
    pub fn requires_approval(&self) -> bool {
        matches!(self, ApprovalStatus::RequiresApproval(_))
    }

    pub fn reason(&self) -> &str {
        match self {
            ApprovalStatus::AutoApproved(r) => r,
            ApprovalStatus::SessionApproved => "Approved for session",
            ApprovalStatus::RequiresApproval(r) => r,
        }
    }
}

/// Pending approval request
#[derive(Debug, Clone)]
pub struct ApprovalRequest {
    /// Tool being invoked
    pub tool_name: String,
    /// Parameters for the invocation
    pub params: Value,
    /// Risk level of the tool
    pub risk_level: RiskLevel,
    /// Preview content (e.g., diff for edits)
    pub preview: Option<String>,
    /// Unique ID for this request
    pub request_id: String,
}

impl ApprovalRequest {
    pub fn new(
        tool_name: impl Into<String>,
        params: Value,
        risk_level: RiskLevel,
        preview: Option<String>,
    ) -> Self {
        Self {
            tool_name: tool_name.into(),
            params,
            risk_level,
            preview,
            request_id: uuid::Uuid::new_v4().to_string(),
        }
    }
}

/// User's response to an approval request
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ApprovalResponse {
    /// Approve this single invocation
    Approve,
    /// Reject this invocation
    Reject,
    /// Approve and remember for this session
    ApproveAlways,
    /// Edit the parameters before approving
    Edit,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::tui::code::tools::ReadTool;

    #[test]
    fn test_auto_approved_tools() {
        let manager = PermissionManager::new();
        let read_tool = ReadTool;

        assert!(!manager.needs_approval(&read_tool, &serde_json::json!({})));
    }

    #[test]
    fn test_session_approval() {
        let mut manager = PermissionManager::new();

        // Initially, write should need approval
        // (We'd need a WriteTool instance, but the concept is clear)

        // After session approval
        manager.approve_tool_for_session("write");

        assert!(manager.session_approved_tools.contains("write"));
    }

    #[test]
    fn test_reset_session() {
        let mut manager = PermissionManager::new();
        manager.approve_tool_for_session("write");
        manager.approve_tool_for_session("bash");

        assert_eq!(manager.session_approved().len(), 2);

        manager.reset_session();

        assert!(manager.session_approved().is_empty());
    }
}
