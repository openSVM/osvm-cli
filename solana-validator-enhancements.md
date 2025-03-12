# Solana Validator/RPC Enhancements

This document outlines the enhancements made to the OSVM-CLI for Solana validator and RPC node deployment based on the [Validator Jumpstart](https://github.com/ice-staking/validator-jumpstart) repository.

## Overview of Enhancements

The following enhancements have been implemented:

1. **Hardware Optimization & Disk Configuration**
   - Support for 3-disk setup (OS, Ledger, Accounts/Snapshots)
   - Disk validation and mounting
   - Directory structure creation

2. **System Performance Optimization**
   - Kernel and network parameter tuning
   - CPU governor settings
   - File descriptor limits
   - Memory management

3. **Enhanced Installation Options**
   - Version selection (standard, Jito, Agave)
   - Client type configuration
   - Installation verification

4. **Hot-Swap Capability**
   - Staked/unstaked keypair management
   - Identity transition scripts
   - Automatic failover monitoring

5. **Monitoring & Management**
   - Log rotation configuration
   - System monitoring scripts
   - Metrics dashboard integration
   - Alert system for critical issues

6. **Security Enhancements**
   - Firewall configuration
   - Port management
   - Enhanced service configuration

## Command Line Usage

### Deploy a Solana Validator

```bash
# Basic validator deployment
osvm solana validator user@host --network mainnet

# Enhanced validator with disk configuration
osvm solana validator user@host --network mainnet \
  --ledger-disk /dev/nvme0n1 --accounts-disk /dev/nvme1n1

# Validator with Jito client
osvm solana validator user@host --network mainnet \
  --client-type jito --version v1.18.23-jito

# Validator with hot-swap capability
osvm solana validator user@host --network mainnet \
  --ledger-disk /dev/nvme0n1 --accounts-disk /dev/nvme1n1 \
  --hot-swap

# Full configuration with metrics
osvm solana validator user@host --network mainnet \
  --ledger-disk /dev/nvme0n1 --accounts-disk /dev/nvme1n1 \
  --client-type jito --version v1.18.23-jito \
  --hot-swap \
  --metrics-config "host=https://metrics.solana.com:8086,db=mainnet-beta,u=mainnet-beta_write,p=password"
```

### Deploy a Solana RPC Node

```bash
# Basic RPC node deployment
osvm solana rpc user@host --network mainnet

# Enhanced RPC with disk configuration
osvm solana rpc user@host --network mainnet \
  --ledger-disk /dev/nvme0n1 --accounts-disk /dev/nvme1n1

# RPC with transaction history enabled
osvm solana rpc user@host --network mainnet \
  --ledger-disk /dev/nvme0n1 --accounts-disk /dev/nvme1n1 \
  --enable-history
```

## Disk Configuration

The enhanced deployment supports the recommended 3-disk configuration:

1. **OS Disk** - Typically ~500GB SSD for the operating system
2. **Ledger Disk** - High-performance NVMe (≥2TB) mounted at `/mnt/ledger`
3. **Accounts Disk** - High-performance NVMe (≥2TB) mounted at `/mnt/extras` with subdirectories:
   - `/mnt/extras/accounts` - For accounts database
   - `/mnt/extras/snapshot` - For snapshots

When using the `--ledger-disk` and `--accounts-disk` options, the system will:
- Validate disk existence and size requirements
- Format disks if not already formatted
- Mount disks to the appropriate locations
- Create necessary subdirectories
- Set proper permissions

## System Optimizations

The deployment applies the following system optimizations:

1. **Network Parameters**
   - TCP buffer sizes and congestion control
   - TCP optimization settings
   - Solana-specific network parameters

2. **Kernel Optimizations**
   - Timer migration settings
   - Process limits
   - Task timeout configuration

3. **Virtual Memory Tuning**
   - Swappiness and memory mapping
   - Dirty ratio and background ratio
   - Write-back settings

## Hot-Swap Capability

The hot-swap capability implements the Identity Transition methodology from Pumpkin's Pool:

1. **Keypair Management**
   - Staked keypair for normal operation
   - Unstaked keypair as fallback

2. **Automatic Failover**
   - Monitoring script checks validator health
   - Automatic transition to unstaked identity when falling behind
   - Automatic transition back to staked identity when caught up

3. **Service Coordination**
   - Log rotation with USR1 signal
   - Service management scripts

## Monitoring Setup

The deployment includes:

1. **Basic Monitoring**
   - Regular service health checks
   - Performance metrics collection
   - Disk usage monitoring

2. **Alert System**
   - Critical error detection
   - Disk space warnings
   - Catching-up state monitoring

3. **Optional Grafana + InfluxDB Stack**
   - Docker-based monitoring stack
   - Pre-configured for Solana metrics
   - Dashboard templates

## Limitations and Notes

- Disk configuration requires direct device paths (e.g., `/dev/nvme0n1`)
- Hot-swap capability is optimized for single-machine operation
- Metrics configuration requires a valid InfluxDB instance
- Some optimizations may require system restarts to take full effect
