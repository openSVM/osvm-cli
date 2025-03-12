//! Dashboard HTML generation for node monitoring
//!
//! This module provides functions to generate HTML for the node monitoring dashboard.

use {
    crate::utils::nodes::NodeInfo, crate::utils::nodes::NodeStatus, chrono::Local,
    std::collections::HashMap,
};

/// Generate HTML content for a monitoring dashboard
///
/// # Arguments
/// * `nodes` - List of node information
///
/// * `verbosity` - Level of detail to include (0=minimal, 1=standard, 2=detailed, 3=debug)
///
/// # Returns
/// * `String` - HTML content
pub fn generate_monitoring_dashboard(nodes: &[NodeInfo], verbosity: u8) -> String {
    // Construct HTML with charts and tables for monitoring nodes
    let mut html = String::new();

    // HTML header
    html.push_str("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n");
    html.push_str("    <meta charset=\"UTF-8\">\n");
    html.push_str(
        "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n",
    );
    html.push_str("    <title>OSVM Node Monitoring Dashboard</title>\n");
    html.push_str("    <style>\n");
    html.push_str("        body {\n");
    html.push_str("            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;\n");
    html.push_str("            margin: 0;\n");
    html.push_str("            padding: 20px;\n");
    html.push_str("            background-color: #f5f7fa;\n");
    html.push_str("            color: #333;\n");
    html.push_str("        }\n");
    html.push_str("        .header {\n");
    html.push_str("            display: flex;\n");
    html.push_str("            justify-content: space-between;\n");
    html.push_str("            align-items: center;\n");
    html.push_str("            margin-bottom: 20px;\n");
    html.push_str("            border-bottom: 1px solid #ddd;\n");
    html.push_str("            padding-bottom: 10px;\n");
    html.push_str("        }\n");
    html.push_str("        .header h1 {\n");
    html.push_str("            color: #4285f4;\n");
    html.push_str("            margin: 0;\n");
    html.push_str("        }\n");
    html.push_str("        .last-update {\n");
    html.push_str("            color: #666;\n");
    html.push_str("            font-size: 0.9em;\n");
    html.push_str("        }\n");
    html.push_str("        .dashboard-grid {\n");
    html.push_str("            display: grid;\n");
    html.push_str("            grid-template-columns: repeat(auto-fit, minmax(400px, 1fr));\n");
    html.push_str("            grid-gap: 20px;\n");
    html.push_str("            margin-bottom: 20px;\n");
    html.push_str("        }\n");
    html.push_str("        .card {\n");
    html.push_str("            background: #fff;\n");
    html.push_str("            border-radius: 8px;\n");
    html.push_str("            box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);\n");
    html.push_str("            padding: 20px;\n");
    html.push_str("        }\n");
    html.push_str("        .card h2 {\n");
    html.push_str("            margin-top: 0;\n");
    html.push_str("            color: #5f6368;\n");
    html.push_str("            font-size: 1.2em;\n");
    html.push_str("            border-bottom: 1px solid #eee;\n");
    html.push_str("            padding-bottom: 10px;\n");
    html.push_str("        }\n");
    html.push_str("        .stat-grid {\n");
    html.push_str("            display: grid;\n");
    html.push_str("            grid-template-columns: repeat(auto-fit, minmax(120px, 1fr));\n");
    html.push_str("            grid-gap: 15px;\n");
    html.push_str("        }\n");
    html.push_str("        .stat-box {\n");
    html.push_str("            text-align: center;\n");
    html.push_str("            padding: 15px;\n");
    html.push_str("            border-radius: 6px;\n");
    html.push_str("            background: #f8f9fa;\n");
    html.push_str("        }\n");
    html.push_str("        .stat-value {\n");
    html.push_str("            font-size: 1.8em;\n");
    html.push_str("            font-weight: bold;\n");
    html.push_str("            margin: 5px 0;\n");
    html.push_str("            color: #4285f4;\n");
    html.push_str("        }\n");
    html.push_str("        .stat-label {\n");
    html.push_str("            font-size: 0.9em;\n");
    html.push_str("            color: #5f6368;\n");
    html.push_str("        }\n");
    html.push_str("        table {\n");
    html.push_str("            width: 100%;\n");
    html.push_str("            border-collapse: collapse;\n");
    html.push_str("            margin-top: 15px;\n");
    html.push_str("        }\n");
    html.push_str("        th, td {\n");
    html.push_str("            text-align: left;\n");
    html.push_str("            padding: 12px;\n");
    html.push_str("            border-bottom: 1px solid #eee;\n");
    html.push_str("        }\n");
    html.push_str("        th {\n");
    html.push_str("            background-color: #f8f9fa;\n");
    html.push_str("            color: #5f6368;\n");
    html.push_str("        }\n");
    html.push_str("        tr:hover {\n");
    html.push_str("            background-color: #f8f9fa;\n");
    html.push_str("        }\n");
    html.push_str("        .status-running {\n");
    html.push_str("            color: #34a853;\n");
    html.push_str("            font-weight: bold;\n");
    html.push_str("        }\n");
    html.push_str("        .status-stopped {\n");
    html.push_str("            color: #fbbc05;\n");
    html.push_str("            font-weight: bold;\n");
    html.push_str("        }\n");
    html.push_str("        .status-error {\n");
    html.push_str("            color: #ea4335;\n");
    html.push_str("            font-weight: bold;\n");
    html.push_str("        }\n");
    html.push_str("        .status-unknown {\n");
    html.push_str("            color: #9aa0a6;\n");
    html.push_str("            font-weight: bold;\n");
    html.push_str("        }\n");
    html.push_str("        .actions button {\n");
    html.push_str("            background: #4285f4;\n");
    html.push_str("            color: white;\n");
    html.push_str("            border: none;\n");
    html.push_str("            padding: 6px 12px;\n");
    html.push_str("            border-radius: 4px;\n");
    html.push_str("            cursor: pointer;\n");
    html.push_str("            font-size: 0.9em;\n");
    html.push_str("            margin-right: 5px;\n");
    html.push_str("        }\n");
    html.push_str("        .actions button:hover {\n");
    html.push_str("            background: #3b78e7;\n");
    html.push_str("        }\n");
    html.push_str("    </style>\n");
    html.push_str("</head>\n<body>\n");

    // Header
    let current_time = Local::now().format("%Y-%m-%d %H:%M:%S").to_string();
    html.push_str("<div class=\"header\">\n");
    html.push_str("    <h1>OSVM Node Monitoring Dashboard</h1>\n");
    html.push_str(&format!(
        "    <div class=\"last-update\">Last updated: {}</div>\n",
        current_time
    ));

    // Add verbosity indicator if debug level
    if verbosity >= 3 {
        html.push_str("    <div style=\"margin-left: 20px; color: #888;\">\n");
        html.push_str(&format!(
            "        <small>Debug mode: Verbosity level {}</small>\n",
            verbosity
        ));
        html.push_str("    </div>\n");
    }
    html.push_str("</div>\n");

    // Summary statistics
    let mut running = 0;
    let mut stopped = 0;
    let mut error = 0;
    let mut unknown = 0;

    for node in nodes {
        match node.status {
            NodeStatus::Running => running += 1,
            NodeStatus::Stopped => stopped += 1,
            NodeStatus::Error => error += 1,
            NodeStatus::Unknown => unknown += 1,
        }
    }

    html.push_str("<div class=\"dashboard-grid\">\n");
    html.push_str("    <div class=\"card\">\n");
    html.push_str("        <h2>Node Status Summary</h2>\n");
    html.push_str("        <div class=\"stat-grid\">\n");

    html.push_str("            <div class=\"stat-box\">\n");
    html.push_str(&format!(
        "                <div class=\"stat-value\">{}</div>\n",
        nodes.len()
    ));
    html.push_str("                <div class=\"stat-label\">Total Nodes</div>\n");
    html.push_str("            </div>\n");

    html.push_str("            <div class=\"stat-box\" style=\"background-color: #e6f4ea;\">\n");
    html.push_str(&format!(
        "                <div class=\"stat-value\" style=\"color: #34a853;\">{}</div>\n",
        running
    ));
    html.push_str("                <div class=\"stat-label\">Running</div>\n");
    html.push_str("            </div>\n");

    html.push_str("            <div class=\"stat-box\" style=\"background-color: #fef7e0;\">\n");
    html.push_str(&format!(
        "                <div class=\"stat-value\" style=\"color: #fbbc05;\">{}</div>\n",
        stopped
    ));
    html.push_str("                <div class=\"stat-label\">Stopped</div>\n");
    html.push_str("            </div>\n");

    html.push_str("            <div class=\"stat-box\" style=\"background-color: #fce8e6;\">\n");
    html.push_str(&format!(
        "                <div class=\"stat-value\" style=\"color: #ea4335;\">{}</div>\n",
        error
    ));
    html.push_str("                <div class=\"stat-label\">Error</div>\n");
    html.push_str("            </div>\n");

    html.push_str("            <div class=\"stat-box\" style=\"background-color: #f1f3f4;\">\n");
    html.push_str(&format!(
        "                <div class=\"stat-value\" style=\"color: #9aa0a6;\">{}</div>\n",
        unknown
    ));
    html.push_str("                <div class=\"stat-label\">Unknown</div>\n");
    html.push_str("            </div>\n");
    html.push_str("        </div>\n");
    html.push_str("    </div>\n");

    // SVM distribution
    if verbosity >= 1 {
        let mut svm_counts = HashMap::new();
        for node in nodes {
            *svm_counts.entry(node.svm_type.clone()).or_insert(0) += 1;
        }

        html.push_str("    <div class=\"card\">\n");
        html.push_str("        <h2>SVM Distribution</h2>\n");
        html.push_str("        <table>\n");
        html.push_str("            <thead>\n");
        html.push_str("                <tr>\n");
        html.push_str("                    <th>SVM Type</th>\n");
        html.push_str("                    <th>Count</th>\n");
        html.push_str("                    <th>Percentage</th>\n");
        html.push_str("                </tr>\n");
        html.push_str("            </thead>\n");
        html.push_str("            <tbody>\n");

        for (svm_type, count) in svm_counts {
            let percentage = (count as f64 / nodes.len() as f64) * 100.0;
            html.push_str("                <tr>\n");
            html.push_str(&format!("                    <td>{}</td>\n", svm_type));
            html.push_str(&format!("                    <td>{}</td>\n", count));
            html.push_str(&format!(
                "                    <td>{:.1}%</td>\n",
                percentage
            ));
            html.push_str("                </tr>\n");
        }

        html.push_str("            </tbody>\n");
        html.push_str("        </table>\n");
        html.push_str("    </div>\n");
    }
    html.push_str("</div>\n");

    // Node list table
    html.push_str("<div class=\"card\">\n");
    html.push_str("    <h2>Node List</h2>\n");
    html.push_str("    <table>\n");
    html.push_str("        <thead>\n");
    html.push_str("            <tr>\n");
    html.push_str("                <th>ID</th>\n");
    html.push_str("                <th>Name</th>\n");
    html.push_str("                <th>SVM Type</th>\n");
    html.push_str("                <th>Node Type</th>\n");

    // Only show network/host with verbosity >= 1
    if verbosity >= 1 {
        html.push_str("                <th>Network</th>\n");
        html.push_str("                <th>Host</th>\n");
    }

    // Add more detail columns for higher verbosity
    if verbosity >= 2 {
        html.push_str("                <th>Last Updated</th>\n");
        html.push_str("                <th>Version</th>\n");
    }
    html.push_str("                <th>Status</th>\n");
    html.push_str("                <th>Actions</th>\n");
    html.push_str("            </tr>\n");
    html.push_str("        </thead>\n");
    html.push_str("        <tbody>\n");

    for node in nodes {
        let status_class = match node.status {
            NodeStatus::Running => "status-running",
            NodeStatus::Stopped => "status-stopped",
            NodeStatus::Error => "status-error",
            NodeStatus::Unknown => "status-unknown",
        };

        let status_text = match node.status {
            NodeStatus::Running => "● Running",
            NodeStatus::Stopped => "○ Stopped",
            NodeStatus::Error => "✕ Error",
            NodeStatus::Unknown => "? Unknown",
        };

        html.push_str("            <tr>\n");
        html.push_str(&format!("                <td>{}</td>\n", node.id));
        html.push_str(&format!("                <td>{}</td>\n", node.name));
        html.push_str(&format!("                <td>{}</td>\n", node.svm_type));
        html.push_str(&format!("                <td>{}</td>\n", node.node_type));

        // Only show network/host with verbosity >= 1
        if verbosity >= 1 {
            html.push_str(&format!("                <td>{}</td>\n", node.network));
            html.push_str(&format!("                <td>{}</td>\n", node.host));
        }

        // Add extra details for higher verbosity
        if verbosity >= 2 {
            html.push_str(&format!(
                "                <td>{}</td>\n",
                Local::now().format("%Y-%m-%d %H:%M")
            ));
            html.push_str(&format!("                <td>v{}</td>\n", "1.0.0")); // Placeholder for version
        }
        html.push_str(&format!(
            "                <td class=\"{}\">{}</td>\n",
            status_class, status_text
        ));
        html.push_str("                <td class=\"actions\">\n");

        // Add action buttons based on status
        match node.status {
            NodeStatus::Running => {
                html.push_str(&format!("                    <button onclick=\"alert('Would restart node {}')\">Restart</button>\n", node.id));
                html.push_str(&format!("                    <button onclick=\"alert('Would stop node {}')\">Stop</button>\n", node.id));
            }
            NodeStatus::Stopped => {
                html.push_str(&format!("                    <button onclick=\"alert('Would start node {}')\">Start</button>\n", node.id));
            }
            NodeStatus::Error => {
                html.push_str(&format!("                    <button onclick=\"alert('Would restart node {}')\">Restart</button>\n", node.id));
                html.push_str(&format!("                    <button onclick=\"alert('Would view logs for node {}')\">View Logs</button>\n", node.id));
            }
            NodeStatus::Unknown => {
                html.push_str(&format!("                    <button onclick=\"alert('Would check status of node {}')\">Check Status</button>\n", node.id));
            }
        }

        html.push_str("                </td>\n");
        html.push_str("            </tr>\n");
    }

    html.push_str("        </tbody>\n");
    html.push_str("    </table>\n");
    html.push_str("</div>\n");

    // Add debug information section if verbosity is high
    if verbosity >= 3 {
        html.push_str("<div class=\"card\">\n");
        html.push_str("    <h2>Debug Information</h2>\n");
        html.push_str("    <pre style=\"background-color: #f1f3f4; padding: 15px; overflow: auto; font-size: 12px;\">\n");
        html.push_str(&format!(
            "Dashboard generated with verbosity level: {}\nTotal nodes: {}\nTimestamp: {}\n",
            verbosity,
            nodes.len(),
            current_time
        ));
        html.push_str("    </pre>\n");
        html.push_str("</div>\n");
    }

    // Close HTML
    html.push_str("<script>\n");
    html.push_str("    // In a real implementation, this would include JavaScript to update data in real-time\n");
    html.push_str("    // and handle button clicks for node actions\n");

    // Add different console logs based on verbosity
    match verbosity {
        0 => html.push_str("    console.log('Dashboard loaded');\n"),
        1 => html.push_str("    console.log('Dashboard loaded with standard verbosity');\n"),
        2 => html.push_str("    console.log('Dashboard loaded with detailed verbosity');\n"),
        _ => {
            html.push_str("    console.log('Dashboard loaded in debug mode');\n");
            html.push_str("    console.log('Nodes:', ");
            html.push_str(&format!("{}); // This would normally show node details\n", nodes.len()));
        }
    }

    html.push_str("</script>\n");
    html.push_str("</body>\n");
    html.push_str("</html>");

    html
}
