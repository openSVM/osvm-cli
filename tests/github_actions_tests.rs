use std::fs;
use std::path::Path;

/// Test GitHub Actions workflow file validation
#[test]
fn test_svm_deploy_workflow_structure() {
    let workflow_path = ".github/workflows/svm-deploy.yml";

    // Ensure the workflow file exists
    assert!(
        Path::new(workflow_path).exists(),
        "SVM deploy workflow file should exist"
    );

    // Parse the workflow YAML
    let content = fs::read_to_string(workflow_path).unwrap();
    let workflow: serde_yaml::Value = serde_yaml::from_str(&content).unwrap();

    // Validate basic structure
    assert!(
        workflow.get("name").is_some(),
        "Workflow should have a name"
    );
    assert!(
        workflow.get("on").is_some(),
        "Workflow should have triggers"
    );
    assert!(workflow.get("jobs").is_some(), "Workflow should have jobs");

    // Check for workflow_call trigger (required for reusable workflows)
    let on_section = workflow.get("on").unwrap();
    assert!(
        on_section.get("workflow_call").is_some(),
        "Should be a reusable workflow"
    );
}

/// Test workflow inputs validation
#[test]
fn test_workflow_inputs() {
    let workflow_path = ".github/workflows/svm-deploy.yml";
    let content = fs::read_to_string(workflow_path).unwrap();
    let workflow: serde_yaml::Value = serde_yaml::from_str(&content).unwrap();

    let inputs = workflow
        .get("on")
        .and_then(|on| on.get("workflow_call"))
        .and_then(|wc| wc.get("inputs"))
        .expect("Should have workflow inputs");

    // Validate required inputs
    let required_inputs = ["svm-name", "host"];
    for input in required_inputs {
        assert!(
            inputs.get(input).is_some(),
            "Required input '{}' should be defined",
            input
        );

        let input_def = inputs.get(input).unwrap();
        assert!(
            input_def.get("required").is_some(),
            "Input '{}' should have required field",
            input
        );
        assert!(
            input_def.get("type").is_some(),
            "Input '{}' should have type field",
            input
        );
    }

    // Validate optional inputs with defaults
    let optional_inputs = ["network", "node-type"];
    for input in optional_inputs {
        if let Some(input_def) = inputs.get(input) {
            assert!(
                input_def.get("default").is_some(),
                "Optional input '{}' should have default",
                input
            );
        }
    }
}

/// Test workflow secrets configuration
#[test]
fn test_workflow_secrets() {
    let workflow_path = ".github/workflows/svm-deploy.yml";
    let content = fs::read_to_string(workflow_path).unwrap();
    let workflow: serde_yaml::Value = serde_yaml::from_str(&content).unwrap();

    let secrets = workflow
        .get("on")
        .and_then(|on| on.get("workflow_call"))
        .and_then(|wc| wc.get("secrets"))
        .expect("Should have workflow secrets");

    // Validate required secrets
    assert!(
        secrets.get("SSH_PRIVATE_KEY").is_some(),
        "SSH_PRIVATE_KEY secret should be defined"
    );

    let ssh_key_def = secrets.get("SSH_PRIVATE_KEY").unwrap();
    assert!(
        ssh_key_def.get("required").is_some(),
        "SSH_PRIVATE_KEY should have required field"
    );

    // Check that SSH_PRIVATE_KEY is required
    let required = ssh_key_def
        .get("required")
        .unwrap()
        .as_bool()
        .unwrap_or(false);
    assert!(required, "SSH_PRIVATE_KEY should be required");
}

/// Test CI workflow structure
#[test]
fn test_ci_workflow_structure() {
    let workflow_path = ".github/workflows/ci.yml";

    assert!(
        Path::new(workflow_path).exists(),
        "CI workflow file should exist"
    );

    let content = fs::read_to_string(workflow_path).unwrap();
    let workflow: serde_yaml::Value = serde_yaml::from_str(&content).unwrap();

    // Validate CI workflow structure
    assert!(
        workflow.get("name").is_some(),
        "CI workflow should have a name"
    );

    let on_section = workflow.get("on").unwrap();
    assert!(on_section.get("push").is_some(), "Should trigger on push");
    assert!(
        on_section.get("pull_request").is_some(),
        "Should trigger on pull requests"
    );

    // Check for main branch protection
    let push_branches = on_section.get("push").and_then(|p| p.get("branches"));
    if let Some(branches) = push_branches {
        let branches_list = branches.as_sequence().unwrap();
        let has_main = branches_list.iter().any(|b| b.as_str() == Some("main"));
        assert!(has_main, "Should trigger on main branch");
    }
}

/// Test that CI includes testing jobs
#[test]
fn test_ci_includes_tests() {
    let workflow_path = ".github/workflows/ci.yml";
    let content = fs::read_to_string(workflow_path).unwrap();
    let workflow: serde_yaml::Value = serde_yaml::from_str(&content).unwrap();

    let jobs = workflow.get("jobs").expect("CI should have jobs");

    // Check for test-related jobs
    let job_names: Vec<&str> = jobs
        .as_mapping()
        .unwrap()
        .keys()
        .filter_map(|k| k.as_str())
        .collect();

    // Should have some form of testing
    let has_test_job = job_names
        .iter()
        .any(|&name| name.contains("test") || name.contains("check") || name.contains("sanity"));

    assert!(
        has_test_job,
        "CI should include testing jobs. Found jobs: {:?}",
        job_names
    );
}

/// Test environment variables in workflows
#[test]
fn test_workflow_environment_variables() {
    let workflow_path = ".github/workflows/ci.yml";
    let content = fs::read_to_string(workflow_path).unwrap();
    let workflow: serde_yaml::Value = serde_yaml::from_str(&content).unwrap();

    // Check for environment variables
    if let Some(env) = workflow.get("env") {
        // Validate common CI environment variables
        assert!(
            env.get("CARGO_TERM_COLOR").is_some(),
            "Should set cargo color output"
        );
        assert!(
            env.get("RUST_BACKTRACE").is_some(),
            "Should enable rust backtraces"
        );
    }
}

/// Test workflow job dependencies and strategy
#[test]
fn test_workflow_job_strategy() {
    let workflow_path = ".github/workflows/ci.yml";
    let content = fs::read_to_string(workflow_path).unwrap();
    let workflow: serde_yaml::Value = serde_yaml::from_str(&content).unwrap();

    let jobs = workflow.get("jobs").expect("Should have jobs");

    // Check each job for proper configuration
    for (job_name, job_def) in jobs.as_mapping().unwrap() {
        let job_name = job_name.as_str().unwrap();

        // Each job should have runs-on
        assert!(
            job_def.get("runs-on").is_some(),
            "Job '{}' should specify runs-on",
            job_name
        );

        // Each job should have steps
        assert!(
            job_def.get("steps").is_some(),
            "Job '{}' should have steps",
            job_name
        );

        let steps = job_def.get("steps").unwrap().as_sequence().unwrap();
        assert!(
            !steps.is_empty(),
            "Job '{}' should have at least one step",
            job_name
        );
    }
}

/// Test cross-platform workflow exists
#[test]
fn test_cross_platform_workflow_exists() {
    let workflow_path = ".github/workflows/cross-platform.yml";

    if Path::new(workflow_path).exists() {
        let content = fs::read_to_string(workflow_path).unwrap();
        let workflow: serde_yaml::Value = serde_yaml::from_str(&content).unwrap();

        // Validate cross-platform structure
        assert!(
            workflow.get("name").is_some(),
            "Cross-platform workflow should have a name"
        );

        let jobs = workflow.get("jobs").expect("Should have jobs");

        // Look for matrix strategy or multiple OS support
        for (_, job_def) in jobs.as_mapping().unwrap() {
            if let Some(strategy) = job_def.get("strategy") {
                if let Some(matrix) = strategy.get("matrix") {
                    // Should test multiple platforms
                    if let Some(os) = matrix.get("os") {
                        let os_list = os.as_sequence().unwrap();
                        assert!(os_list.len() > 1, "Should test multiple operating systems");
                    }
                }
            }
        }
    }
}

/// Test workflow action references are valid
#[test]
fn test_workflow_action_references() {
    let workflow_paths = [
        ".github/workflows/ci.yml",
        ".github/workflows/svm-deploy.yml",
    ];

    for workflow_path in &workflow_paths {
        if !Path::new(workflow_path).exists() {
            continue;
        }

        let content = fs::read_to_string(workflow_path).unwrap();
        let workflow: serde_yaml::Value = serde_yaml::from_str(&content).unwrap();

        let jobs = workflow.get("jobs").expect("Should have jobs");

        for (job_name, job_def) in jobs.as_mapping().unwrap() {
            let job_name = job_name.as_str().unwrap();
            let steps = job_def.get("steps").unwrap().as_sequence().unwrap();

            for (step_idx, step) in steps.iter().enumerate() {
                if let Some(uses) = step.get("uses") {
                    let action = uses.as_str().unwrap();

                    // Validate action format
                    if action.starts_with("actions/") {
                        // GitHub official actions should have version
                        assert!(
                            action.contains("@"),
                            "Action '{}' in job '{}' step {} should specify version",
                            action,
                            job_name,
                            step_idx
                        );
                    } else if action.starts_with("./") {
                        // Local actions should exist (we can't verify this in tests but format is correct)
                        assert!(
                            action.starts_with("./.github/"),
                            "Local action '{}' should be in .github/ directory",
                            action
                        );
                    }
                }
            }
        }
    }
}

/// Test workflow outputs are properly defined
#[test]
fn test_workflow_outputs() {
    let workflow_path = ".github/workflows/svm-deploy.yml";
    let content = fs::read_to_string(workflow_path).unwrap();
    let workflow: serde_yaml::Value = serde_yaml::from_str(&content).unwrap();

    // Check for workflow outputs
    if let Some(workflow_call) = workflow.get("on").and_then(|on| on.get("workflow_call")) {
        if let Some(outputs) = workflow_call.get("outputs") {
            // Validate output definitions
            for (output_name, output_def) in outputs.as_mapping().unwrap() {
                let output_name = output_name.as_str().unwrap();

                assert!(
                    output_def.get("description").is_some(),
                    "Output '{}' should have description",
                    output_name
                );
                assert!(
                    output_def.get("value").is_some(),
                    "Output '{}' should have value reference",
                    output_name
                );
            }
        }
    }
}

/// Test deployment workflow security
#[test]
fn test_deployment_security_practices() {
    let workflow_path = ".github/workflows/svm-deploy.yml";
    let content = fs::read_to_string(workflow_path).unwrap();
    let workflow: serde_yaml::Value = serde_yaml::from_str(&content).unwrap();

    let jobs = workflow.get("jobs").expect("Should have jobs");

    for (job_name, job_def) in jobs.as_mapping().unwrap() {
        let job_name = job_name.as_str().unwrap();
        let steps = job_def.get("steps").unwrap().as_sequence().unwrap();

        for step in steps {
            // Check that secrets are not exposed in run commands
            if let Some(run_cmd) = step.get("run") {
                let cmd = run_cmd.as_str().unwrap();

                // Should not contain raw secret values
                assert!(
                    !cmd.contains("-----BEGIN"),
                    "Job '{}' should not contain raw private keys in run commands",
                    job_name
                );
                assert!(
                    !cmd.contains("sk_"),
                    "Job '{}' should not contain raw API keys in run commands",
                    job_name
                );
            }

            // Check environment variable usage for secrets
            if let Some(env) = step.get("env") {
                // Environment variables with secrets should use ${{ secrets.* }} syntax
                for (env_name, env_value) in env.as_mapping().unwrap() {
                    let env_value_str = env_value.as_str().unwrap_or("");
                    if env_name.as_str().unwrap().to_lowercase().contains("key")
                        || env_name.as_str().unwrap().to_lowercase().contains("secret")
                    {
                        assert!(
                            env_value_str.contains("secrets.") || env_value_str.contains("inputs."),
                            "Secret environment variable should use GitHub secrets syntax"
                        );
                    }
                }
            }
        }
    }
}

/// Test workflow file permissions and security
#[test]
fn test_workflow_permissions() {
    let workflow_paths = [
        ".github/workflows/ci.yml",
        ".github/workflows/svm-deploy.yml",
    ];

    for workflow_path in &workflow_paths {
        if !Path::new(workflow_path).exists() {
            continue;
        }

        let content = fs::read_to_string(workflow_path).unwrap();
        let workflow: serde_yaml::Value = serde_yaml::from_str(&content).unwrap();

        // Check if permissions are explicitly defined
        if let Some(permissions) = workflow.get("permissions") {
            // If permissions are defined, they should be minimal
            if let Some(contents) = permissions.get("contents") {
                let contents_perm = contents.as_str().unwrap_or("none");
                assert!(
                    ["read", "none"].contains(&contents_perm),
                    "Contents permission should be minimal (read or none)"
                );
            }
        }
    }
}
