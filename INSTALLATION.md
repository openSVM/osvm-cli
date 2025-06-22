# OSVM CLI Installation Guide

## 📦 Installation Methods

The OSVM CLI provides multiple ways to build and install the compiled release binary to `/usr/bin`:

### 1. Using Make (Recommended)

```bash
# Install debug version (fast development)
make install-dev

# Install release version (optimized)
make build-release && sudo cp target/release/osvm /usr/bin/osvm

# Verify installation
make verify-install

# Uninstall
make uninstall

# Clean everything
make clean-install
```

### 2. Using Installation Script

```bash
# Make script executable (if not already)
chmod +x install-release.sh

# Run installation script
./install-release.sh
```

### 3. Manual Installation

```bash
# Build release binary
cargo build --release

# Copy to system path
sudo cp target/release/osvm /usr/bin/osvm
sudo chmod +x /usr/bin/osvm

# Verify
osvm --version
```

### 4. Using Cargo Make (Advanced)

If you have `cargo-make` installed:

```bash
# Install cargo-make if needed
cargo install cargo-make

# Use cargo-make tasks
cargo make install
cargo make install-dev
cargo make verify-install
```

## 🔧 Available Make Targets

- `make build` - Build debug binary
- `make build-release` - Build optimized release binary
- `make test` - Run all tests
- `make install` - Build release and install to /usr/bin (requires sudo)
- `make install-dev` - Install debug binary to /usr/bin (requires sudo)
- `make uninstall` - Remove osvm from /usr/bin (requires sudo)
- `make clean` - Clean build artifacts
- `make clean-install` - Remove installed binaries and backups
- `make verify-install` - Verify osvm installation
- `make help` - Show all available commands

## 🎯 Quick Start

For most users, the simplest installation is:

```bash
# Debug version (for development)
make install-dev

# Or release version (for production)
make build-release
sudo cp target/release/osvm /usr/bin/osvm
```

Then verify with:
```bash
osvm --version
osvm --help
```

## 🔒 Backup and Safety

The installation system automatically:
- Creates backups of existing binaries at `/usr/bin/osvm.backup`
- Verifies installation before committing changes
- Provides rollback capability if installation fails

## 🚀 Usage After Installation

Once installed, you can use `osvm` from anywhere:

```bash
osvm --help                    # Show help
osvm rpc-manager test --status # Check test RPC status
osvm rpc-manager devnet --help # Devnet RPC options
osvm diagnostics              # Run system diagnostics
```
