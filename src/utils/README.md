# OSVM Utilities

This directory contains various utilities used by the OSVM CLI tool.

## Dashboard

The `dashboard` module provides a terminal user interface (TUI) for monitoring SVMs and nodes in real-time. It displays information about installed SVMs, their network configurations, and performance metrics.

### Features

- **Overview**: View a summary of all installed SVMs with their key metrics
- **Network Details**: See RPC endpoints and network configuration for each SVM
- **Performance**: Monitor TPS, latency, and other performance metrics with real-time charts
- **Nodes**: View information about individual nodes with resource usage indicators
- **Logs**: See aggregated logs from all nodes in one place

### Usage

To launch the dashboard, use the following command:

```bash
osvm svm dashboard
```

### Keyboard Controls

- `Tab`, `Right Arrow`, `Left Arrow`: Switch between tabs
- `Up Arrow`, `Down Arrow`: Navigate through nodes
- `n`: Select next SVM
- `p`: Select previous SVM
- `h`: Toggle help overlay
- `q` or `Ctrl+C`: Quit the dashboard

### Implementation Details

The dashboard is built using:
- **ratatui**: For the terminal user interface components
- **crossterm**: For cross-platform terminal control
- **threading**: For non-blocking UI updates

The dashboard uses a multi-threaded architecture to ensure the UI remains responsive while background tasks update metrics and fetch new data.