# GitHub Actions Deployment Guide for OSVM CLI

This comprehensive guide covers deploying Solana Virtual Machines (SVMs) using GitHub Actions workflows with the OSVM CLI. Whether you're setting up automated deployments, CI/CD pipelines, or scheduled maintenance tasks, this guide provides everything you need to integrate OSVM CLI into your GitHub Actions workflows.

## Table of Contents

- [Overview](#overview)
- [Prerequisites](#prerequisites)
- [Quick Start](#quick-start)
- [Setup & Configuration](#setup--configuration)
- [The SVM Deploy Action](#the-svm-deploy-action)
- [Workflow Examples](#workflow-examples)
- [Security Best Practices](#security-best-practices)
- [Troubleshooting](#troubleshooting)
- [Advanced Use Cases](#advanced-use-cases)
- [Testing & Validation](#testing--validation)
- [Additional Resources](#additional-resources)

## Overview

The OSVM CLI provides a dedicated GitHub Action that simplifies SVM deployment workflows. This action abstracts the complexity of remote server setup, SSH connections, and SVM installation into a single, reusable component.

### Benefits of GitHub Actions Integration

- **🚀 Automated Deployments**: Deploy SVMs automatically on code changes, releases, or schedules
- **🔒 Secure Management**: Use GitHub Secrets for sensitive data like SSH keys and Solana keypairs
- **🎯 Multi-Environment**: Deploy to different environments (dev, staging, production) with environment-specific configurations
- **📊 Monitoring**: Built-in logging and status reporting for deployment tracking
- **🔄 Repeatability**: Consistent, reproducible deployments across different environments
- **⚡ Scalability**: Deploy multiple SVMs in parallel using matrix strategies

## Prerequisites

### System Requirements

- GitHub repository with Actions enabled
- Target servers with SSH access
- Ubuntu 18.04+ or compatible Linux distribution on target servers
- Minimum 16 CPU cores and 256GB RAM for mainnet validators
- Rust 1.80.0+ (installed automatically by the action)
- Solana CLI tools 1.14.29+ (installed automatically by the action)

### Required Knowledge

- Basic understanding of GitHub Actions workflows
- Familiarity with SSH key management
- Basic knowledge of Solana networks and validators
- Understanding of YAML configuration files

## Quick Start

Here's the fastest way to get started with OSVM CLI GitHub Actions:

### 1. Set Up Repository Secrets

Add these secrets to your GitHub repository (Settings → Secrets and variables → Actions):

```bash
# Required: SSH private key for connecting to your server
SSH_PRIVATE_KEY="-----BEGIN OPENSSH PRIVATE KEY-----
...your private key content...
-----END OPENSSH PRIVATE KEY-----"

# Optional: Solana keypair for the validator
SOLANA_KEYPAIR='[1,2,3,...keypair array...]'

# Optional: Custom RPC endpoint
RPC_URL="https://api.mainnet-beta.solana.com"
```

### 2. Create a Basic Workflow

Create `.github/workflows/deploy-svm.yml`:

```yaml
name: Deploy SVM Node

on:
  push:
    branches: [ main ]
  workflow_dispatch:

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Deploy SVM Node
        uses: ./.github/actions/svm-deploy
        with:
          svm-name: 'my-svm'
          host: 'user@your-server.com'
          network: 'devnet'
          node-type: 'validator'
          ssh-private-key: ${{ secrets.SSH_PRIVATE_KEY }}
          verbose: true
```

### 3. Trigger the Workflow

- Push to your main branch, or
- Go to Actions tab → "Deploy SVM Node" → "Run workflow"

## Setup & Configuration

### SSH Key Configuration

#### Generate SSH Key Pair

```bash
# Generate a new SSH key pair for GitHub Actions
ssh-keygen -t ed25519 -C "github-actions@$(hostname)" -f ~/.ssh/github_actions

# Display the private key (add this to GitHub Secrets as SSH_PRIVATE_KEY)
cat ~/.ssh/github_actions

# Display the public key (add this to your server's authorized_keys)
cat ~/.ssh/github_actions.pub
```

#### Configure Server Access

On your target server:

```bash
# Add the public key to authorized_keys
echo "ssh-ed25519 AAAAC3... github-actions@hostname" >> ~/.ssh/authorized_keys

# Set proper permissions
chmod 600 ~/.ssh/authorized_keys
chmod 700 ~/.ssh
```

### Solana Keypair Setup

#### Generate Solana Keypair

```bash
# Install Solana CLI if not already installed
sh -c "$(curl -sSfL https://release.solana.com/stable/install)"

# Generate a new keypair
solana-keygen new --no-bip39-passphrase -o validator-keypair.json

# Display the keypair content (add this to GitHub Secrets as SOLANA_KEYPAIR)
cat validator-keypair.json
```

#### Network Configuration

Choose your target network:

- **devnet**: `https://api.devnet.solana.com` (for testing)
- **testnet**: `https://api.testnet.solana.com` (for staging)
- **mainnet**: `https://api.mainnet-beta.solana.com` (for production)

## The SVM Deploy Action

The OSVM CLI includes a pre-built GitHub Action located at `.github/actions/svm-deploy/`. This action handles all aspects of SVM deployment.

### Action Inputs

| Input | Description | Required | Default | Example |
|-------|-------------|----------|---------|---------|
| `svm-name` | Name of the SVM to deploy | ✅ | - | `my-validator` |
| `host` | Remote host (format: user@host[:port]) | ✅ | - | `admin@192.168.1.100:22` |
| `network` | Target Solana network | ❌ | `mainnet` | `devnet`, `testnet`, `mainnet` |
| `node-type` | Type of node to deploy | ❌ | `validator` | `validator`, `rpc` |
| `ssh-private-key` | SSH private key content | ✅ | - | `${{ secrets.SSH_PRIVATE_KEY }}` |
| `keypair` | Solana keypair JSON content | ❌ | - | `${{ secrets.SOLANA_KEYPAIR }}` |
| `rpc-url` | Custom JSON RPC URL | ❌ | - | `https://api.mainnet-beta.solana.com` |
| `verbose` | Enable detailed logging | ❌ | `false` | `true`, `false` |
| `deploy-args` | Additional CLI arguments | ❌ | - | `--custom-flag value` |

### Action Outputs

| Output | Description | Usage |
|--------|-------------|-------|
| `status` | Deployment status | `success` or `failure` |
| `logs` | Complete deployment logs | Full command output |

## Workflow Examples

### Basic Deployment

```yaml
name: Basic SVM Deployment

on:
  push:
    branches: [ main ]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Deploy SVM
        uses: ./.github/actions/svm-deploy
        with:
          svm-name: 'basic-validator'
          host: 'validator@svm-server.example.com'
          network: 'devnet'
          ssh-private-key: ${{ secrets.SSH_PRIVATE_KEY }}
```

### Multi-Environment Deployment

```yaml
name: Multi-Environment Deployment

on:
  push:
    branches: [ main, develop ]

jobs:
  deploy:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        environment: 
          - name: dev
            network: devnet
            host: dev-validator@dev.example.com
          - name: staging  
            network: testnet
            host: staging-validator@staging.example.com
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Deploy to ${{ matrix.environment.name }}
        uses: ./.github/actions/svm-deploy
        with:
          svm-name: '${{ matrix.environment.name }}-validator'
          host: ${{ matrix.environment.host }}
          network: ${{ matrix.environment.network }}
          ssh-private-key: ${{ secrets.SSH_PRIVATE_KEY }}
          keypair: ${{ secrets.SOLANA_KEYPAIR }}
          verbose: true
```

### Conditional Production Deployment

```yaml
name: Production Deployment

on:
  push:
    tags: [ 'v*' ]
  workflow_dispatch:
    inputs:
      confirm_production:
        description: 'Type "deploy" to confirm production deployment'
        required: true
        type: string

jobs:
  deploy-production:
    runs-on: ubuntu-latest
    if: github.event_name == 'push' || github.event.inputs.confirm_production == 'deploy'
    
    environment: production
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Deploy Production Validator
        uses: ./.github/actions/svm-deploy
        with:
          svm-name: 'prod-validator'
          host: ${{ secrets.PROD_VALIDATOR_HOST }}
          network: 'mainnet'
          ssh-private-key: ${{ secrets.PROD_SSH_KEY }}
          keypair: ${{ secrets.PROD_KEYPAIR }}
          rpc-url: ${{ secrets.PROD_RPC_URL }}
          verbose: true
          deploy-args: '--enable-monitoring --high-performance'
```

### Scheduled Maintenance

```yaml
name: Scheduled SVM Maintenance

on:
  schedule:
    # Run every Sunday at 02:00 UTC
    - cron: '0 2 * * 0'
  workflow_dispatch:

jobs:
  maintenance:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Update SVM
        uses: ./.github/actions/svm-deploy
        with:
          svm-name: 'maintenance-update'
          host: ${{ secrets.VALIDATOR_HOST }}
          network: 'mainnet'
          ssh-private-key: ${{ secrets.SSH_PRIVATE_KEY }}
          deploy-args: '--update --restart-if-needed'
```

### Matrix Deployment (Multiple Validators)

```yaml
name: Multi-Validator Deployment

on:
  workflow_dispatch:
    inputs:
      deploy_count:
        description: 'Number of validators to deploy'
        required: true
        default: '3'
        type: choice
        options: ['1', '2', '3', '5']

jobs:
  deploy:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        validator_id: [1, 2, 3, 4, 5]
      fail-fast: false
      max-parallel: 3
    
    if: matrix.validator_id <= fromJSON(github.event.inputs.deploy_count)
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Deploy Validator ${{ matrix.validator_id }}
        uses: ./.github/actions/svm-deploy
        with:
          svm-name: 'validator-${{ matrix.validator_id }}'
          host: 'validator${{ matrix.validator_id }}@cluster.example.com'
          network: 'devnet'
          ssh-private-key: ${{ secrets.SSH_PRIVATE_KEY }}
          keypair: ${{ secrets[format('KEYPAIR_VALIDATOR_{0}', matrix.validator_id)] }}
```

## Security Best Practices

### Secret Management

1. **Use GitHub Secrets**: Never commit sensitive data to your repository
2. **Rotate Keys Regularly**: Change SSH keys and Solana keypairs periodically
3. **Limit Secret Access**: Use environment-specific secrets when possible
4. **Audit Secret Usage**: Regularly review which workflows have access to secrets

### SSH Security

```yaml
# Example of secure SSH configuration
- name: Setup SSH with strict security
  run: |
    mkdir -p ~/.ssh
    echo "${{ secrets.SSH_PRIVATE_KEY }}" > ~/.ssh/id_rsa
    chmod 600 ~/.ssh/id_rsa
    
    # Add known hosts to prevent MITM attacks
    ssh-keyscan -H your-server.com >> ~/.ssh/known_hosts
    
    # Test connection
    ssh -o BatchMode=yes -o ConnectTimeout=10 user@your-server.com 'echo "Connection test successful"'
```

### Network Security

1. **Use Private Networks**: Deploy validators on private/internal networks when possible
2. **Firewall Configuration**: Ensure proper firewall rules are in place
3. **VPN Access**: Consider requiring VPN access for sensitive deployments
4. **Monitor Access**: Log and monitor all deployment activities

### Keypair Security

```yaml
# Secure keypair handling
- name: Secure Keypair Setup
  run: |
    # Create secure directory
    mkdir -p ~/.config/solana
    chmod 700 ~/.config/solana
    
    # Write keypair with secure permissions
    echo '${{ secrets.SOLANA_KEYPAIR }}' > ~/.config/solana/id.json
    chmod 600 ~/.config/solana/id.json
    
    # Verify keypair
    solana-keygen pubkey ~/.config/solana/id.json
  env:
    SOLANA_KEYPAIR: ${{ secrets.SOLANA_KEYPAIR }}
```

## Troubleshooting

### Common Issues and Solutions

#### SSH Connection Failures

**Problem**: SSH connection fails during deployment

**Solutions**:
```yaml
# Add debugging to your workflow
- name: Debug SSH Connection
  run: |
    echo "Testing SSH connection..."
    ssh -v -o BatchMode=yes -o ConnectTimeout=10 user@host 'echo "Connected successfully"'
    
    # Check SSH key format
    echo "${{ secrets.SSH_PRIVATE_KEY }}" | head -1
    
    # Verify host reachability
    ping -c 3 your-host.com
```

**Common causes**:
- Incorrect SSH key format in secrets
- Missing public key on target server
- Firewall blocking SSH access
- Wrong username or hostname

#### SVM Installation Failures

**Problem**: SVM installation fails on target server

**Solutions**:
```yaml
# Add system requirements check
- name: Verify System Requirements
  uses: ./.github/actions/svm-deploy
  with:
    svm-name: 'requirements-check'
    host: ${{ secrets.HOST }}
    ssh-private-key: ${{ secrets.SSH_PRIVATE_KEY }}
    deploy-args: '--check-requirements-only'
    verbose: true
```

**Common causes**:
- Insufficient system resources (CPU, RAM, disk)
- Missing system dependencies
- Network connectivity issues
- Insufficient permissions

#### Network Configuration Issues

**Problem**: Cannot connect to Solana network

**Solutions**:
```yaml
# Test network connectivity
- name: Test Network Connectivity
  run: |
    # Test RPC endpoint
    curl -X POST -H "Content-Type: application/json" \
      -d '{"jsonrpc":"2.0","id":1,"method":"getHealth"}' \
      https://api.devnet.solana.com
    
    # Test from target server
    ssh user@host 'curl -s https://api.devnet.solana.com | head -20'
```

#### Action Failures

**Problem**: The GitHub Action itself fails

**Debugging steps**:

1. **Enable verbose logging**:
```yaml
- name: Deploy with verbose logging
  uses: ./.github/actions/svm-deploy
  with:
    verbose: true
    # ... other parameters
```

2. **Check action logs**:
   - Go to Actions tab in your repository
   - Click on the failed workflow run
   - Expand the "Deploy SVM" step
   - Review the detailed logs

3. **Validate inputs**:
```yaml
- name: Validate Inputs
  run: |
    echo "SVM Name: my-svm"
    echo "Host: ${{ secrets.HOST }}"
    echo "Network: devnet"
    echo "SSH Key present: ${{ secrets.SSH_PRIVATE_KEY != '' }}"
```

### Debugging Workflows

#### Enable Debug Logging

```yaml
env:
  ACTIONS_STEP_DEBUG: true
  ACTIONS_RUNNER_DEBUG: true
```

#### Test Components Individually

```yaml
jobs:
  test-ssh:
    runs-on: ubuntu-latest
    steps:
      - name: Test SSH Connection
        run: |
          mkdir -p ~/.ssh
          echo "${{ secrets.SSH_PRIVATE_KEY }}" > ~/.ssh/id_rsa
          chmod 600 ~/.ssh/id_rsa
          ssh -o StrictHostKeyChecking=no user@host 'echo "SSH test successful"'
  
  test-osvm:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Test OSVM Installation
        run: |
          # Test local OSVM installation
          cargo build --release
          ./target/release/osvm --version
```

## Advanced Use Cases

### Blue-Green Deployments

```yaml
name: Blue-Green Deployment

on:
  workflow_dispatch:
    inputs:
      target_slot:
        description: 'Deployment slot (blue/green)'
        required: true
        type: choice
        options: ['blue', 'green']

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Deploy to ${{ github.event.inputs.target_slot }}
        uses: ./.github/actions/svm-deploy
        with:
          svm-name: '${{ github.event.inputs.target_slot }}-validator'
          host: '${{ github.event.inputs.target_slot }}-validator@cluster.example.com'
          network: 'mainnet'
          ssh-private-key: ${{ secrets.SSH_PRIVATE_KEY }}
          deploy-args: '--slot ${{ github.event.inputs.target_slot }}'
          
      - name: Health Check
        run: |
          # Wait for validator to be ready
          sleep 60
          
          # Perform health checks
          curl -f "http://${{ github.event.inputs.target_slot }}-validator.example.com:8080/health"
          
      - name: Switch Traffic (if green is healthy)
        if: github.event.inputs.target_slot == 'green'
        run: |
          # Switch load balancer to green
          echo "Switching traffic to green deployment"
          # Your traffic switching logic here
```

### Rollback Mechanism

```yaml
name: Rollback Deployment

on:
  workflow_dispatch:
    inputs:
      rollback_version:
        description: 'Version to rollback to'
        required: true
        type: string

jobs:
  rollback:
    runs-on: ubuntu-latest
    environment: production
    
    steps:
      - uses: actions/checkout@v4
        with:
          ref: ${{ github.event.inputs.rollback_version }}
      
      - name: Rollback Validator
        uses: ./.github/actions/svm-deploy
        with:
          svm-name: 'rollback-validator'
          host: ${{ secrets.PROD_HOST }}
          network: 'mainnet'
          ssh-private-key: ${{ secrets.SSH_PRIVATE_KEY }}
          deploy-args: '--rollback-to ${{ github.event.inputs.rollback_version }}'
```

### Integration with External Monitoring

```yaml
name: Deploy with Monitoring Integration

on:
  push:
    branches: [ main ]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Deploy Validator
        id: deploy
        uses: ./.github/actions/svm-deploy
        with:
          svm-name: 'monitored-validator'
          host: ${{ secrets.HOST }}
          ssh-private-key: ${{ secrets.SSH_PRIVATE_KEY }}
          verbose: true
          
      - name: Report to Monitoring
        if: always()
        run: |
          # Send deployment status to monitoring system
          curl -X POST "${{ secrets.MONITORING_WEBHOOK }}" \
            -H "Content-Type: application/json" \
            -d '{
              "deployment_id": "${{ github.run_id }}",
              "status": "${{ steps.deploy.outputs.status }}",
              "timestamp": "'$(date -u +%Y-%m-%dT%H:%M:%SZ)'",
              "logs": "${{ steps.deploy.outputs.logs }}"
            }'
          
      - name: Slack Notification
        if: failure()
        uses: 8398a7/action-slack@v3
        with:
          status: failure
          channel: '#validators'
          text: 'SVM deployment failed! Check logs for details.'
        env:
          SLACK_WEBHOOK_URL: ${{ secrets.SLACK_WEBHOOK }}
```

## Testing & Validation

### Testing Your Workflows

#### Local Testing with act

```bash
# Install act (GitHub Actions local runner)
curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash

# Test your workflow locally
act -n  # Dry run
act     # Full run
```

#### Validation Workflow

Create `.github/workflows/validate-deployment.yml`:

```yaml
name: Validate Deployment Configuration

on:
  pull_request:
    paths:
      - '.github/workflows/**'
      - '.github/actions/**'

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Validate YAML Syntax
        run: |
          # Install yamllint
          pip install yamllint
          
          # Validate all workflow files
          find .github/workflows -name "*.yml" -exec yamllint {} \;
          
      - name: Test Action Syntax
        run: |
          # Validate action.yml files
          find .github/actions -name "action.yml" -exec yamllint {} \;
          
      - name: Simulate Deployment (Dry Run)
        uses: ./.github/actions/svm-deploy
        with:
          svm-name: 'test-validator'
          host: 'test@localhost'
          network: 'devnet'
          ssh-private-key: 'fake-key-for-testing'
          deploy-args: '--dry-run'
```

### End-to-End Testing

```yaml
name: E2E Deployment Test

on:
  schedule:
    - cron: '0 6 * * 1'  # Weekly on Monday
  workflow_dispatch:

jobs:
  e2e-test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        network: [devnet, testnet]
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Deploy Test Validator
        uses: ./.github/actions/svm-deploy
        with:
          svm-name: 'e2e-test-${{ matrix.network }}'
          host: ${{ secrets.TEST_HOST }}
          network: ${{ matrix.network }}
          ssh-private-key: ${{ secrets.SSH_PRIVATE_KEY }}
          
      - name: Validate Deployment
        run: |
          # Check if validator is running
          ssh user@test-host 'systemctl is-active solana-validator'
          
          # Check sync status
          ssh user@test-host 'solana validators --url ${{ matrix.network }}'
          
      - name: Cleanup
        if: always()
        run: |
          # Clean up test deployment
          ssh user@test-host 'systemctl stop solana-validator'
```

## Additional Resources

### Documentation Links

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [OSVM CLI Repository](https://github.com/openSVM/osvm-cli)
- [Solana Validator Documentation](https://docs.solana.com/running-validator)
- [GitHub Actions for Rust](https://github.com/actions-rs/meta)

### Example Repositories

- [OSVM CLI Examples](../../../examples/github-actions/) - Ready-to-use workflow examples
- [SVM Deploy Action](../../../.github/actions/svm-deploy/) - Complete action documentation

### Community Resources

- [OSVM CLI Discussions](https://github.com/openSVM/osvm-cli/discussions)
- [Solana Discord](https://discord.gg/solana)
- [GitHub Actions Community](https://github.community/t/github-actions/9)

### Support

For issues and questions:

1. **Documentation**: Start with this guide and the linked resources
2. **Issues**: Report bugs or feature requests on [GitHub Issues](https://github.com/openSVM/osvm-cli/issues)
3. **Discussions**: Join conversations in [GitHub Discussions](https://github.com/openSVM/osvm-cli/discussions)
4. **Community**: Connect with other users in the Solana Discord

---

## Next Steps

1. **Set up your first workflow** using the [Quick Start](#quick-start) guide
2. **Review the security practices** to ensure your deployments are secure
3. **Explore the examples** in the `examples/github-actions/` directory
4. **Test your configuration** using the validation workflows
5. **Join the community** to share experiences and get help

Happy deploying! 🚀

---

*This documentation is maintained as part of the OSVM CLI project. For the latest updates, please refer to the [official repository](https://github.com/openSVM/osvm-cli).*