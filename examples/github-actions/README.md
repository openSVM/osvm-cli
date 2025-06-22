# GitHub Actions Examples for SVM Deployment

This directory contains example workflows and configurations for using the SVM Deploy GitHub Action in various scenarios.

## Examples

### 1. [Basic Deployment](basic-deployment.yml)
Simple SVM deployment with minimal configuration.

### 2. [Multi-Environment Deployment](multi-environment.yml)
Deploy to multiple environments (dev, staging, production) with environment-specific configurations.

### 3. [Scheduled Deployment](scheduled-deployment.yml)
Automated SVM deployments on a schedule using cron expressions.

### 4. [Matrix Deployment](matrix-deployment.yml)
Deploy multiple SVMs in parallel using GitHub Actions matrix strategy.

### 5. [Conditional Deployment](conditional-deployment.yml)
Deploy SVMs based on specific conditions (branch, tags, etc.).

## Quick Start

1. Copy the example workflow that best matches your use case
2. Customize the inputs and configuration
3. Set up the required secrets in your repository
4. Commit and push to trigger the workflow

## Required Secrets

All examples require these secrets to be configured in your repository:

- `SSH_PRIVATE_KEY`: SSH private key for connecting to remote hosts
- `SOLANA_KEYPAIR`: (Optional) Solana keypair JSON content

## Directory Structure

```
examples/github-actions/
├── README.md
├── basic-deployment.yml
├── multi-environment.yml
├── scheduled-deployment.yml
├── matrix-deployment.yml
└── conditional-deployment.yml
```